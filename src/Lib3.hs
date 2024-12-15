{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib3
  ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Statements (..),
    Command (..),
  )
where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM.TVar
import Control.Exception (IOException, catch)
import Control.Monad (forever)
import Control.Monad.STM
import Control.Monad.STM (atomically)
import Data.Maybe (fromMaybe)
import qualified Lib2
import System.IO.Error (isDoesNotExistError)

data StorageOp = Save String (Chan ()) | Load (Chan String)

-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = loop
  where
    loop = do
      op <- readChan chan
      case op of
        Save info saveChannel -> do
          writeFile "state.txt" info
          writeChan saveChannel ()
        Load loadChannel -> do
          info <- readFile "state.txt"
          writeChan loadChannel info
      loop

data Statements -- queries is lib2
  = Batch [Lib2.Query]
  | Single Lib2.Query
  deriving (Show, Eq)

data Command
  = StatementCommand Statements
  | LoadCommand
  | SaveCommand
  deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input =
  let trimmedInput = dropWhile (== ' ') input
   in if trimmedInput == "LOAD"
        then Right (LoadCommand, "")
        else
          if trimmedInput == "SAVE"
            then Right (SaveCommand, "")
            else case parseStatements input of
              Left err -> Left err
              Right (statements, rest) -> Right (StatementCommand statements, rest)

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements str =
  if take 5 str == "BEGIN"
    then case parseBatch str of
      Right (queries, rest) -> Right (Batch queries, "")
      Left err -> Left err
    else case Lib2.parseQuery str of
      Right query -> Right (Single query, "")
      Left err -> Left err

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (== ' ')

parseBatch :: String -> Either String ([Lib2.Query], String)
parseBatch input = do
  let lines' = lines input
  let (queryLines, rest) = break (== "END") lines'

  let cleanedQueries =
        filter
          ( \line ->
              not (null (trim line))
                && line /= "BEGIN"
                && line /= "END"
          )
          queryLines

  case mapM Lib2.parseQuery cleanedQueries of
    Right queries -> Right (queries, unlines (drop 1 rest))
    Left err -> Left err

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
-- idejos ateiciai: jei paremovina ir tada pacallina load, kas tada nutinka??
marshallState :: Lib2.State -> Statements
marshallState (Lib2.State sellerName concerts)
  | null sellerName && null concerts = Single (Lib2.AddConcertTicketsSeller "" [])
  | otherwise = Batch [Lib2.RemoveConcertTicketsSeller, Lib2.AddConcertTicketsSeller sellerName concerts]

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file.
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Batch queries) =
  "BEGIN\n"
    ++ unlines (map renderQuery queries) -- unline sudeda enterius tarp stringu
    ++ "END"
renderStatements (Single query) = renderQuery query

showConcertAlt :: Lib2.Concert -> String
showConcertAlt Lib2.Concert {Lib2.title = t, Lib2.artist = a, Lib2.date = d, Lib2.tickets = ts} =
  t
    ++ ","
    ++ a
    ++ ","
    ++ show d
    ++ if null ts
      then ""
      else "," ++ renderList1 ts

renderList :: [Lib2.Concert] -> String
renderList [] = "" -- Base case: empty list produces an empty string
renderList [x] = showConcertAlt x -- Single item: just show it without a trailing comma
renderList (x : xs) = showConcertAlt x ++ "," ++ renderList xs -- Show head, add comma, recurse on tail

renderList1 :: (Show a) => [a] -> String
renderList1 [] = "" -- Base case: empty list produces an empty string
renderList1 [x] = show x -- Single item: just show it without a trailing comma
renderList1 (x : xs) = show x ++ "," ++ renderList1 xs -- Show head, add comma, recurse on tail

renderQuery :: Lib2.Query -> String
renderQuery (Lib2.AddConcertTicketsSeller name concerts) =
  "add_concert_tickets_seller" ++ "," ++ name ++ "," ++ renderList concerts
renderQuery (Lib2.AddConcert concert) =
  "add_concert" ++ "," ++ showConcertAlt concert
renderQuery (Lib2.AddTicket concert ticket) =
  "add_ticket" ++ "," ++ showConcertAlt concert ++ ",," ++ show ticket
renderQuery (Lib2.RemoveConcert concert) =
  "remove_concert" ++ "," ++ showConcertAlt concert
renderQuery (Lib2.RemoveConcertTicketsSeller) =
  "remove_concert_tickets_seller"
renderQuery (Lib2.SellTicket ticket) =
  "sell_ticket" ++ "," ++ show ticket
renderQuery (Lib2.ReturnTicket ticket) =
  "return_ticket" ++ "," ++ show ticket
renderQuery (Lib2.RemoveTicket concert ticket) =
  "remove_ticket" ++ "," ++ showConcertAlt concert ++ ",," ++ show ticket
renderQuery (Lib2.CheckAvailableTickets concert) =
  "check_available_tickets" ++ "," ++ showConcertAlt concert
renderQuery Lib2.ShowState =
  "show_state"
renderQuery _ = "Unknown Query"

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition ::
  TVar Lib2.State ->
  Command ->
  Chan StorageOp ->
  IO (Either String (Maybe String))
stateTransition s SaveCommand ioChan = do
  s' <- readTVarIO s
  chan <- newChan :: IO (Chan ())
  writeChan ioChan (Save (renderStatements $ marshallState s') chan)
  readChan chan
  return $ Right $ Just "State saved successfully"
stateTransition s LoadCommand ioChan = do
  chan <- newChan :: IO (Chan (String))
  writeChan ioChan (Load chan)
  qs <- readChan chan ---cia jau nuskaito ka perskaite
  case parseStatements qs of
    Left e -> do
      return $ Left $ "Failed to parse file" ++ e
    Right (qs', _) -> stateTransition s (StatementCommand qs') ioChan
stateTransition s (StatementCommand sts) _ = atomically $ atomicStatements s sts -- arg tikisi statements ir grazina komanda

transitionThroughList :: Lib2.State -> [Lib2.Query] -> Either String (Maybe String, Lib2.State)
transitionThroughList _ [] = Left "Empty query list"
transitionThroughList s (q : qs) = case Lib2.stateTransition s q of
  Left e -> Left e
  Right (msg, ns) ->
    if null qs
      then Right (msg, ns)
      else case transitionThroughList ns qs of
        Left e -> Left e
        Right (msg', ns') -> Right (Just (concatMaybeWithNewline msg msg'), ns')

concatMaybeWithNewline :: Maybe String -> Maybe String -> String
concatMaybeWithNewline m1 m2 = case (m1, m2) of
  (Just s1, Just s2) -> s1 ++ "\n" ++ s2 -- Insert a newline between both strings
  _ -> fromMaybe "" m1 ++ fromMaybe "" m2

atomicStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String))
atomicStatements s (Batch qs) = do
  s' <- readTVar s
  case transitionThroughList s' qs of
    Left e -> return $ Left e
    Right (msg, ns) -> do
      writeTVar s ns
      return $ Right msg
atomicStatements s (Single q) = do
  s' <- readTVar s
  case Lib2.stateTransition s' q of
    Left e -> return $ Left e
    Right (msg, ns) -> do
      writeTVar s ns
      return $ Right msg
