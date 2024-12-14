{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Lib2
  ( Query (..),
    parseQuery,
    State (..),
    emptyState,
    stateTransition,
    Ticket (..),
    Concert (..),
    Date (..),
    Month (..),
  )
where

import Control.Monad (unless, when)
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.State as S
import Data.Char (isDigit, isUpper)
import qualified Parsers hiding (State)

data Query
  = AddConcertTicketsSeller String [Concert]
  | AddConcert Concert
  | AddTicket Concert Ticket
  | RemoveConcertTicketsSeller
  | RemoveConcert Concert
  | RemoveTicket Concert Ticket
  | SellTicket Ticket
  | ReturnTicket Ticket
  | ChangeConcertInformation Concert
  | CheckAvailableTickets Concert
  | UpdateConcert Concert Concert
  | ShowState
  deriving (Show, Eq)

data State = State
  { sellerName :: String,
    concerts :: [Concert]
  }
  deriving (Show, Eq)

data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Show, Eq, Enum)

data Concert = Concert
  { title :: String,
    artist :: String,
    date :: Date,
    tickets :: [Ticket]
  }
  deriving (Eq)

instance Show Concert where
  show Concert {title = t, artist = a, date = d, tickets = ts} =
    t ++ "," ++ a ++ "," ++ show d ++ "," ++ show ts

data Date = Date
  { year :: Int,
    hyphen1 :: Char,
    month :: Month,
    hyphen2 :: Char,
    day :: Int
  }
  deriving (Eq)

instance Show Date where
  show Date {year = y, hyphen1 = h1, month = m, hyphen2 = h2, day = d} =
    show y ++ "-" ++ show m ++ "-" ++ show d

data Ticket = Ticket
  { ticketId :: Int,
    ticketType :: String,
    availability :: String,
    price :: Int
  }
  deriving (Eq)

instance Show Ticket where
  show Ticket {ticketId = tID, ticketType = tT, availability = a, price = p} =
    show tID ++ "," ++ tT ++ "," ++ a ++ "," ++ show p

emptyState :: State
emptyState = State {sellerName = "", concerts = []}

-- Parser implementations
parseLetterAndDigit :: Parsers.Parser String
parseLetterAndDigit = do
  input <- Parsers.lift Parsers.get
  case input of
    (x : y : xs) | isUpper x && isDigit y -> do
      Parsers.lift $ Parsers.put xs
      return [x, y]
    _ -> Parsers.throwE "Error: expected uppercase letter followed by a digit"

parseType :: Parsers.Parser String
parseType = do
  input <- Parsers.lift Parsers.get
  case input of
    [] -> Parsers.throwE "Ticket type Error: empty input"
    _ -> do
      firstPart <- Parsers.parseString
      when (firstPart /= "Seat") $ Parsers.throwE "Error: false type"
      matched <- parseLetterAndDigit
      string <- Parsers.parseString
      unless (string `elem` ["Standing", "VIP", "Group seated", "Group standing"]) $
        Parsers.throwE "Error: expected standing || VIP || Group Seated || Group standing"
      return $ firstPart ++ "," ++ matched ++ string

parseMonth :: Parsers.Parser Month
parseMonth = do
  input <- Parsers.lift Parsers.get
  case break (== '-') input of
    (monthStr, rest) -> do
      month <- case monthStr of
        "January" -> return January
        "February" -> return February
        "March" -> return March
        "April" -> return April
        "May" -> return May
        "June" -> return June
        "July" -> return July
        "August" -> return August
        "September" -> return September
        "October" -> return October
        "November" -> return November
        "December" -> return December
        _ -> Parsers.throwE "Invalid month"
      Parsers.lift $ Parsers.put rest
      return month

parseDay :: Parsers.Parser Int
parseDay = do
  num <- Parsers.parseNumber
  unless (num >= 1 && num <= 31) $ Parsers.throwE "Error: Invalid day"
  return num

parseDate :: Parsers.Parser Date
parseDate = do
  year1 <- Parsers.parseNumber
  when (year1 < 1000 || year1 > 9999) $ Parsers.throwE "Invalid year"
  _ <- Parsers.parseChar '-'
  month1 <- parseMonth
  _ <- Parsers.parseChar '-'
  day1 <- parseDay
  return
    Date
      { year = year1,
        hyphen1 = '-',
        month = month1,
        hyphen2 = '-',
        day = day1
      }

parseTicket :: Parsers.Parser Ticket
parseTicket = do
  matchedId <-
    Parsers.parseNumber `Parsers.catchE` \err ->
      Parsers.throwE $ "Error while parsing ticket ID: " ++ err
  _ <-
    Parsers.parseChar ',' `Parsers.catchE` \err ->
      Parsers.throwE $ "Error while parsing ',' after ticket ID: " ++ err
  matchedType <-
    parseType `Parsers.catchE` \err ->
      Parsers.throwE $ "Error while parsing ticket type: " ++ err
  matchedAvailability <-
    Parsers.parseString `Parsers.catchE` \err ->
      Parsers.throwE $ "Error while parsing ticket availability: " ++ err
  when (matchedAvailability /= "Available" && matchedAvailability /= "Sold") $
    Parsers.throwE $
      "Error: wrong availability '"
        ++ matchedAvailability
        ++ "'. Expected 'Available' or 'Sold'."
  matchedPrice <-
    Parsers.parseNumber `Parsers.catchE` \err ->
      Parsers.throwE $ "Error while parsing ticket price: " ++ err
  return
    Ticket
      { ticketId = matchedId,
        ticketType = matchedType,
        availability = matchedAvailability,
        price = matchedPrice
      }

parseTicketsList :: Parsers.Parser [Ticket]
parseTicketsList = do
  input <- Parsers.lift Parsers.get
  case Parsers.parse parseTicket input of
    (Left _, _) -> return []
    (Right ticket, rest) -> do
      Parsers.lift $ Parsers.put rest
      if null rest
        then return [ticket]
        else do
          _ <- Parsers.parseChar ','
          if null rest
            then return [ticket]
            else case Parsers.parse (Parsers.parseChar ',') rest of
              (Left _, _) -> return [ticket]
              (Right _, remaining) -> do
                moreTickets <- parseTicketsList
                return (ticket : moreTickets)

parseConcert :: Parsers.Parser Concert
parseConcert = do
  matchedTitle <- Parsers.parseString
  matchedArtist <- Parsers.parseString
  matchedDate <- parseDate
  input <- Parsers.lift Parsers.get
  if null input
    then
      return
        Concert
          { title = matchedTitle,
            artist = matchedArtist,
            date = matchedDate,
            tickets = []
          }
    else do
      _ <- Parsers.parseChar ','
      matchedTicketsList <- parseTicketsList
      return
        Concert
          { title = matchedTitle,
            artist = matchedArtist,
            date = matchedDate,
            tickets = matchedTicketsList
          }

parseConcertList :: Parsers.Parser [Concert]
parseConcertList = do
  input <- Parsers.lift Parsers.get
  case Parsers.parse parseConcert input of
    (Left _, _) -> return []
    (Right concert, rest) -> do
      Parsers.lift $ Parsers.put (dropWhile (== ',') rest)
      moreConcerts <- parseConcertList
      return (concert : moreConcerts)

parseQuery :: String -> Either String Query
parseQuery input = case Parsers.parse parser input of
  (Left err, _) -> Left err
  (Right query, remaining) ->
    if null remaining
      then Right query
      else Left $ "Unparsed input remaining: " ++ remaining
  where
    parser = do
      command <- Parsers.parseString
      case command of
        "add_concert_tickets_seller" -> do
          name <- Parsers.parseString
          AddConcertTicketsSeller name <$> parseConcertList
        "add_concert" -> AddConcert <$> parseConcert
        "add_ticket" -> do
          concert <- parseConcert
          _ <-
            Parsers.parseChar ',' `Parsers.catchE` \err ->
              Parsers.throwE $ "AS UZLUZTU CIA " ++ err
          AddTicket concert <$> parseTicket
        "remove_concert_tickets_seller" -> return RemoveConcertTicketsSeller
        "remove_concert" -> RemoveConcert <$> parseConcert
        "remove_ticket" -> do
          concert <- parseConcert
          _ <- Parsers.parseChar ','
          RemoveTicket concert <$> parseTicket
        "sell_ticket" -> SellTicket <$> parseTicket
        "return_ticket" -> ReturnTicket <$> parseTicket
        "change_concert_information" -> do
          concert <- parseConcert
          _ <- Parsers.parseChar ','
          UpdateConcert concert <$> parseConcert
        "check_available_tickets" -> CheckAvailableTickets <$> parseConcert
        "show_state" -> return ShowState
        _ -> Parsers.throwE $ "Unknown command: " ++ command

-- State transition functions
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of
  AddConcertTicketsSeller name concerts ->
    if not (null (sellerName st))
      then Left "ConcertTicketsSeller already exists."
      else Right (Nothing, st {sellerName = name, concerts = concerts})
  AddConcert concert ->
    if any (\c -> title c == title concert && artist c == artist concert && date c == date concert) (concerts st)
      then Left "Concert already exists."
      else Right (Nothing, st {concerts = concert : concerts st})
  AddTicket concert ticket -> addTicketToState st concert ticket
  RemoveConcertTicketsSeller -> Right (Nothing, st {sellerName = "", concerts = []})
  RemoveConcert concert -> removeConcertFromState st concert
  RemoveTicket concert ticket -> removeTicketFromState st concert ticket
  SellTicket ticket -> updateTicketAvailability st ticket "Sold"
  ReturnTicket ticket -> updateTicketAvailability st ticket "Available"
  UpdateConcert oldConcert newConcert -> updateConcertInState st oldConcert newConcert
  CheckAvailableTickets concert -> checkAvailableTicketsInState st concert
  ShowState -> Right (Just (show st), st)
  ChangeConcertInformation newConcert -> changeConcertInformation st newConcert

-- Helper functions for state transitions
addTicketToState :: State -> Concert -> Ticket -> Either String (Maybe String, State)
addTicketToState st concert ticket
  | not concertExists = Left "Concert not found."
  | ticketExists = Left "Ticket with the same ID already exists."
  | otherwise = Right (Nothing, st {concerts = updatedConcerts})
  where
    concertExists =
      any
        ( \c ->
            title c == title concert
              && artist c == artist concert
              && date c == date concert
        )
        (concerts st)
    ticketExists =
      any
        (any (\t -> ticketId t == ticketId ticket) . tickets)
        ( filter
            ( \c ->
                title c == title concert
                  && artist c == artist concert
                  && date c == date concert
            )
            (concerts st)
        )

    updatedConcerts = map updateConcert (concerts st)
    updateConcert c
      | title c == title concert
          && artist c == artist concert
          && date c == date concert =
          if not ticketExists then c {tickets = ticket : tickets c} else c
      | otherwise = c

removeConcertFromState :: State -> Concert -> Either String (Maybe String, State)
removeConcertFromState st concert =
  if length remainingConcerts == length (concerts st)
    then Left "Concert not found."
    else Right (Nothing, st {concerts = remainingConcerts})
  where
    remainingConcerts = filter (\c -> title c /= title concert || artist c /= artist concert || date c /= date concert) (concerts st)

removeTicketFromState :: State -> Concert -> Ticket -> Either String (Maybe String, State)
removeTicketFromState st concert ticket =
  if any (\c -> title c == title concert && artist c == artist concert && date c == date concert && ticket `elem` tickets c) (concerts st)
    then Right (Nothing, st {concerts = updatedConcerts})
    else Left "Ticket not found."
  where
    updatedConcerts = map updateConcert (concerts st)
    updateConcert c
      | title c == title concert && artist c == artist concert && date c == date concert =
          c {tickets = filter (/= ticket) (tickets c)}
      | otherwise = c

updateTicketAvailability :: State -> Ticket -> String -> Either String (Maybe String, State)
updateTicketAvailability st ticket newStatus
  | not ticketFound = Left "Ticket not found."
  | ticketAlready = Left ("Ticket is already " ++ newStatus)
  | otherwise = Right (Nothing, st {concerts = updatedConcerts})
  where
    updatedConcerts = map updateConcert (concerts st)
    updateConcert c = c {tickets = map updateTicket (tickets c)}
    updateTicket t =
      if ticketId t == ticketId ticket
        then
          t {availability = newStatus}
        else
          t
    ticketFound =
      any
        (any (\t -> ticketId t == ticketId ticket) . tickets)
        (concerts st)
    ticketAlready =
      any
        ( any
            ( \t ->
                ticketId t == ticketId ticket && availability t == newStatus
            )
            . tickets
        )
        (concerts st)

updateConcertInState :: State -> Concert -> Concert -> Either String (Maybe String, State)
updateConcertInState st oldConcert newConcert =
  if not concertExists
    then Left "Concert not found."
    else Right (Nothing, st {concerts = updatedConcerts})
  where
    concertExists = any (\c -> title c == title oldConcert && artist c == artist oldConcert && date c == date oldConcert) (concerts st)
    updatedConcerts = map updateConcert (concerts st)
    updateConcert c
      | title c == title oldConcert && artist c == artist oldConcert && date c == date oldConcert = newConcert
      | otherwise = c

checkAvailableTicketsInState :: State -> Concert -> Either String (Maybe String, State)
checkAvailableTicketsInState st concert =
  if availableTicketCount > 0
    then Right (Just (show availableTicketCount), st)
    else Left "No available tickets."
  where
    availableTickets = concatMap tickets $ filter (\c -> title c == title concert && artist c == artist concert && date c == date concert) (concerts st)
    availableTicketCount = length $ filter (\t -> availability t == "Available") availableTickets

changeConcertInformation :: State -> Concert -> Either String (Maybe String, State)
changeConcertInformation st newConcert =
  if any (\c -> title c == title newConcert && artist c == artist newConcert && date c == date newConcert) (concerts st)
    then Right (Nothing, st {concerts = updatedConcerts})
    else Left "Concert not found."
  where
    updatedConcerts = map updateConcert (concerts st)
    updateConcert c
      | title c == title newConcert && artist c == artist newConcert && date c == date newConcert = newConcert
      | otherwise = c
