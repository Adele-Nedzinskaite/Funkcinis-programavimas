{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Control.Concurrent (Chan, forkIO, newChan)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Control.Lens
import Control.Monad.Free (Free (..), liftF)
import Data.ByteString
import Data.Maybe (fromMaybe)
import Data.String.Conversions
import qualified Lib2
import qualified Lib3
import Network.Wreq

data MyDomainAlgebra next
  = AddConcertTicketsSeller String [Lib2.Concert] next
  | AddConcert Lib2.Concert next
  | AddTicket Lib2.Concert Lib2.Ticket next
  | RemoveConcertTicketsSeller next
  | RemoveConcert Lib2.Concert next
  | RemoveTicket Lib2.Concert Lib2.Ticket next
  | SellTicket Lib2.Ticket next
  | ReturnTicket Lib2.Ticket next
  | CheckAvailableTickets Lib2.Concert (String -> next)
  | ShowState (String -> next)
  | Save next
  | Load next
  deriving (Functor)

type MyDomain = Free MyDomainAlgebra

addConcertTicketsSeller :: String -> [Lib2.Concert] -> MyDomain ()
addConcertTicketsSeller organizer concerts = liftF $ AddConcertTicketsSeller organizer concerts ()

addConcert :: Lib2.Concert -> MyDomain ()
addConcert concert = liftF $ AddConcert concert ()

addTicket :: Lib2.Concert -> Lib2.Ticket -> MyDomain ()
addTicket concert ticket = liftF $ AddTicket concert ticket ()

removeConcertTicketsSeller :: MyDomain ()
removeConcertTicketsSeller = liftF $ RemoveConcertTicketsSeller ()

removeConcert :: Lib2.Concert -> MyDomain ()
removeConcert concert = liftF $ RemoveConcert concert ()

removeTicket :: Lib2.Concert -> Lib2.Ticket -> MyDomain ()
removeTicket concert ticket = liftF $ RemoveTicket concert ticket ()

sellTicket :: Lib2.Ticket -> MyDomain ()
sellTicket ticket = liftF $ SellTicket ticket ()

returnTicket :: Lib2.Ticket -> MyDomain ()
returnTicket ticket = liftF $ ReturnTicket ticket ()

checkAvailableTickets :: Lib2.Concert -> MyDomain String
checkAvailableTickets concert = liftF $ CheckAvailableTickets concert (\s -> s)

showState :: MyDomain String
showState = liftF $ ShowState (\s -> s)

save :: MyDomain ()
save = liftF $ Save ()

load :: MyDomain ()
load = liftF $ Load ()

interpretorSingleRequest :: MyDomain a -> IO a
interpretorSingleRequest (Pure a) = return a
interpretorSingleRequest (Free step) = do
  next <- runStep step
  interpretorSingleRequest next
  where
    runStep :: MyDomainAlgebra a -> IO a
    runStep (AddConcertTicketsSeller organizer concerts next) = sendSingleStatement (Lib2.AddConcertTicketsSeller organizer concerts) >> return next
    runStep (AddConcert concert next) = sendSingleStatement (Lib2.AddConcert concert) >> return next
    runStep (AddTicket concert ticket next) = sendSingleStatement (Lib2.AddTicket concert ticket) >> return next
    runStep (RemoveConcertTicketsSeller next) = sendSingleStatement Lib2.RemoveConcertTicketsSeller >> return next
    runStep (RemoveConcert concert next) = sendSingleStatement (Lib2.RemoveConcert concert) >> return next
    runStep (RemoveTicket concert ticket next) = sendSingleStatement (Lib2.RemoveTicket concert ticket) >> return next
    runStep (SellTicket ticket next) = sendSingleStatement (Lib2.SellTicket ticket) >> return next
    runStep (ReturnTicket ticket next) = sendSingleStatement (Lib2.ReturnTicket ticket) >> return next
    runStep (CheckAvailableTickets concert f) = do
      result <- sendSingleStatement (Lib2.CheckAvailableTickets concert)
      return $ f result
    runStep (ShowState f) = do
      result <- sendSingleStatement Lib2.ShowState
      return $ f result
    runStep (Save next) = postString "save" >> return next
    runStep (Load next) = postString "load" >> return next

interpretWithBatching' :: MyDomain a -> [Lib2.Query] -> IO a
interpretWithBatching' (Pure a) batch = dumpBatch batch >> return a
interpretWithBatching' (Free step) batch = do
  case step of
    AddConcertTicketsSeller organizer concerts next -> interpretWithBatching' next $ batch ++ [Lib2.AddConcertTicketsSeller organizer concerts]
    AddConcert concert next -> interpretWithBatching' next $ batch ++ [Lib2.AddConcert concert]
    AddTicket concert ticket next -> interpretWithBatching' next $ batch ++ [Lib2.AddTicket concert ticket]
    RemoveConcertTicketsSeller next -> interpretWithBatching' next $ batch ++ [Lib2.RemoveConcertTicketsSeller]
    RemoveConcert concert next -> interpretWithBatching' next $ batch ++ [Lib2.RemoveConcert concert]
    RemoveTicket concert ticket next -> interpretWithBatching' next $ batch ++ [Lib2.RemoveTicket concert ticket]
    SellTicket ticket next -> interpretWithBatching' next $ batch ++ [Lib2.SellTicket ticket]
    ReturnTicket ticket next -> interpretWithBatching' next $ batch ++ [Lib2.ReturnTicket ticket]
    CheckAvailableTickets concert f -> do
      result <- sendSingleStatement (Lib2.CheckAvailableTickets concert)
      interpretWithBatching' (f result) batch
    ShowState f -> do
      result <- sendSingleStatement Lib2.ShowState
      interpretWithBatching' (f result) batch
    Save next -> dumpBatch batch >> postString "save" >> interpretWithBatching' next []
    Load next -> dumpBatch batch >> postString "load" >> interpretWithBatching' next []

interpretWithBatching :: MyDomain a -> IO a
interpretWithBatching program = interpretWithBatching' program []

sendSingleStatement :: Lib2.Query -> IO String
sendSingleStatement = postString . Lib3.renderStatements . Lib3.Single

postString :: String -> IO String
postString s = do
  let rawRequest = cs s :: ByteString
  putStrLn $ "Sending request:\n" ++ cs rawRequest
  resp <- post "http://localhost:3000" rawRequest
  return $ cs $ resp ^. responseBody

dumpBatch :: [Lib2.Query] -> IO (Maybe String)
dumpBatch [] = return Nothing
dumpBatch [single] = Just <$> sendSingleStatement single
dumpBatch batch = Just <$> sendAsBatch batch

sendAsBatch :: [Lib2.Query] -> IO String
sendAsBatch = postString . Lib3.renderStatements . Lib3.Batch

program :: MyDomain (Maybe String, Maybe String)
program = do
  addConcertTicketsSeller organizer [concert]
  addConcert (Lib2.Concert "AnotherTitle" "AnotherArtist" (Lib2.Date 2025 '-' Lib2.December '-' 10) [])
  addTicket concert (Lib2.Ticket 1 "Seat,A1VIP" "Available" 1)
  o1 <- checkAvailableTickets concert
  save
  load
  o1 <- checkAvailableTickets concert
  o2 <- showState
  return (Just o1, Just o2)
  where
    concert = Lib2.Concert "Title" "Artist" (Lib2.Date 2000 '-' Lib2.January '-' 1) []
    organizer = "Organizer"

testInterpretator :: MyDomain a -> IO a
testInterpretator p = do
  state <- newTVarIO Lib2.emptyState
  chan <- newChan :: IO (Chan Lib3.StorageOp)
  initialState <- readTVarIO state
  putStrLn $ "Initial state:\n" ++ show initialState ++ "\n"
  _ <- forkIO $ Lib3.storageOpLoop chan
  testInterpretator' state chan p
  where
    testInterpretator' :: TVar Lib2.State -> Chan Lib3.StorageOp -> MyDomain a -> IO a
    testInterpretator' _ _ (Pure a) = return a
    testInterpretator' state chan (Free step) = do
      next <- runStep state chan step
      putStrLn "State after:"
      newState <- readTVarIO state
      putStrLn $ show newState ++ "\n"
      testInterpretator' state chan next

    runStep :: TVar Lib2.State -> Chan Lib3.StorageOp -> MyDomainAlgebra a -> IO a
    runStep state chan (AddConcertTicketsSeller organizer concerts next) =
      transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.AddConcertTicketsSeller organizer concerts) chan >> return next
    runStep state chan (AddConcert concert next) =
      transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.AddConcert concert) chan >> return next
    runStep state chan (AddTicket concert ticket next) =
      transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.AddTicket concert ticket) chan >> return next
    runStep state chan (RemoveConcertTicketsSeller next) =
      transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single Lib2.RemoveConcertTicketsSeller) chan >> return next
    runStep state chan (RemoveConcert concert next) =
      transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.RemoveConcert concert) chan >> return next
    runStep state chan (RemoveTicket concert ticket next) =
      transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.RemoveTicket concert ticket) chan >> return next
    runStep state chan (SellTicket ticket next) =
      transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.SellTicket ticket) chan >> return next
    runStep state chan (ReturnTicket ticket next) =
      transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.ReturnTicket ticket) chan >> return next
    runStep state chan (CheckAvailableTickets concert f) = do
      str <- transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.CheckAvailableTickets concert) chan
      return $ f str
    runStep state chan (ShowState f) = do
      str <- transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single Lib2.ShowState) chan
      return $ f str
    runStep state chan (Save next) =
      transitionAndPrint state Lib3.SaveCommand chan >> return next
    runStep state chan (Load next) =
      transitionAndPrint state Lib3.LoadCommand chan >> return next

transitionAndPrint :: TVar Lib2.State -> Lib3.Command -> Chan Lib3.StorageOp -> IO String
transitionAndPrint state cmd chan = do
  putStrLn $ "Command:\n" ++ show cmd
  res <- Lib3.stateTransition state cmd chan
  let str = either id (fromMaybe "Success") res
  putStrLn "Result:"
  putStrLn str
  return str

main :: IO ()
main = do
  -- str <- interpretorSingleRequest program
  -- str <- interpretWithBatching program
  str <- testInterpretator program
  print str