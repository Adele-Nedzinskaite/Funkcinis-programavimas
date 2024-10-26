{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
      parseQuery,
      State(..),
      emptyState,
      stateTransition
    ) where

import qualified Data.Char as C
import qualified Data.List as L
import Data.Char (isUpper, isDigit)

data State = State {
    sellerName :: String,
    concerts :: [Concert] 
}deriving (Show)

data Query
    = AddConcertTicketsSeller String [Concert]
    | AddConcert Concert
    | AddTicket Concert Ticket
    | RemoveConcertTicketsSeller String
    | RemoveConcert Concert
    | RemoveTicket Concert Ticket
    | SellTicket Ticket
    | ReturnTicket Ticket
    | ChangeConcertInformation Concert
    | CheckAvailableTickets Concert
    deriving (Show)

data Month = January | February | March | April | May | June
           | July | August | September | October | November | December
           deriving (Show, Eq, Enum)

data Concert = Concert
    { title :: String
    , artist :: String
    , date :: Date
    , tickets :: [Ticket]
    } deriving (Show,Eq)

data Date = Date
    {
        year :: Int
        , hyphen1 :: Char
        ,month :: Month
        , hyphen2 :: Char
        ,day :: Int
    } deriving (Show, Eq)

data Ticket = Ticket
    { ticketId :: Int
    , ticketType :: String
    , availability :: String
    , price :: Int
    } deriving (Show,Eq)

data ConcertTicketsSeller = ConcertTicketsSeller {
    organizationName :: String,
    concertList :: [Concert]
}

parseQuery :: String -> Either String Query
parseQuery input =
    case parseString input of  -- Pass the input to parseString
        Left err -> Left err
        Right (command, remaining) -> 
            if command == "add_concert_tickets_seller" then
                case parseString remaining of
                    Left err -> Left err
                    Right (name, concertsStr) -> 
                        case parseConcertList concertsStr of
                            Left err -> Left err
                            Right (concerts, _) -> 
                                Right (AddConcertTicketsSeller name concerts)

            else if command == "add_concert" then
                case parseString remaining of
                    Left err -> Left err
                    Right (concertStr, _) ->
                        fmap (AddConcert . fst) (parseConcert concertStr)

            else if command == "add_ticket" then
                case parseString remaining of
                    Left err -> Left err
                    Right (concertStr, ticketStr) -> 
                        case parseConcert concertStr of
                            Left err -> Left err
                            Right (concert, _) -> 
                                case parseTicket ticketStr of
                                    Left err -> Left err
                                    Right (ticket, _) -> 
                                        Right (AddTicket concert ticket)

            else if command == "remove_concert_tickets_seller" then
                case parseString remaining of
                    Left err -> Left err
                    Right (name, _) -> 
                        Right (RemoveConcertTicketsSeller name)

            else if command == "remove_concert" then
                case parseString remaining of
                    Left err -> Left err
                    Right (concertStr, _) -> 
                        fmap (RemoveConcert . fst) (parseConcert concertStr)

            else if command == "remove_ticket" then
                case parseString remaining of
                    Left err -> Left err
                    Right (concertStr, ticketStr) -> 
                        case parseConcert concertStr of
                            Left err -> Left err
                            Right (concert, _) -> 
                                case parseTicket ticketStr of
                                    Left err -> Left err
                                    Right (ticket, _) -> 
                                        Right (RemoveTicket concert ticket)

            else if command == "sell_ticket" then
                case parseString remaining of
                    Left err -> Left err
                    Right (ticketStr, _) -> 
                        fmap (SellTicket . fst) (parseTicket ticketStr)

            else if command == "return_ticket" then
                case parseString remaining of
                    Left err -> Left err
                    Right (ticketStr, _) -> 
                        fmap (ReturnTicket . fst) (parseTicket ticketStr)

            else
                Left "Error: Unknown command"



type Parser a = String -> Either String (a, String) 

emptyState :: State
emptyState = State { sellerName = "", concerts = [] }

stateTransition :: State -> Query -> Either String State
stateTransition st query = case query of
    AddConcertTicketsSeller name concerts -> 
        if not (null (sellerName st))
        then Left "ConcertTicketsSeller already exists."
        else Right $ st { sellerName = name, concerts = concerts }

    AddConcert concert -> 
        if any (\c -> title c == title concert && artist c == artist concert && date c == date concert) (concerts st)
        then Left "Concert already exists."
        else Right $ st { concerts = concert : concerts st }

    AddTicket concert ticket -> 
        let updatedConcerts = map (\c -> 
                if title c == title concert && artist c == artist concert && date c == date concert
                then c { tickets = ticket : tickets c } 
                else c) (concerts st)
        in if any (\c -> title c == title concert && artist c == artist concert && date c == date concert) (concerts st)
            then Right $ st { concerts = updatedConcerts }
            else Left "Concert not found."

    RemoveConcertTicketsSeller name ->
        if sellerName st /= name
        then Left "No such ConcertTicketsSeller exists."
        else Right $ st { sellerName = "", concerts = [] }

    RemoveConcert concert ->
        let remainingConcerts = filter (\c -> title c /= title concert || artist c /= artist concert || date c /= date concert) (concerts st)
        in if length remainingConcerts == length (concerts st)
            then Left "Concert not found."
            else Right $ st { concerts = remainingConcerts }

    RemoveTicket concert ticket -> 
        let updatedConcerts = map (\c -> 
                if title c == title concert && artist c == artist concert && date c == date concert
                then c { tickets = filter (/= ticket) (tickets c) } -- Ensure we update the concert tickets correctly
                else c) (concerts st) -- Keep other concerts unchanged
        in if any (\c -> title c == title concert && artist c == artist concert && date c == date concert && ticket `elem` tickets c) (concerts st)
            then Right $ st { concerts = updatedConcerts } -- Make sure updatedConcerts is a list of Concert
            else Left "Ticket not found."
    
    SellTicket ticket -> updateTicketAvailability st ticket "Sold"

    ReturnTicket ticket -> updateTicketAvailability st ticket "Available"

    ChangeConcertInformation newConcert ->
        let updatedConcerts = map (\c -> 
                if title c == title newConcert && artist c == artist newConcert && date c == date newConcert 
                then newConcert 
                else c) (concerts st)
        in if any (\c -> title c == title newConcert && artist c == artist newConcert && date c == date newConcert) (concerts st)
            then Right $ st { concerts = updatedConcerts }
            else Left "Concert not found."

    CheckAvailableTickets concert ->
        let availableTickets = concatMap tickets $ filter (\c -> title c == title concert && artist c == artist concert && date c == date concert) (concerts st)
            availableTicketCount = length $ filter (\t -> availability t == "Available") availableTickets
        in if availableTicketCount > 0
            then Right st  
            else Left "No available tickets."

updateTicketAvailability :: State -> Ticket -> String -> Either String State
updateTicketAvailability st ticket newStatus = 
    let updatedConcerts = map (\c -> 
            if ticket `elem` (tickets c) 
            then c { tickets = map (\t -> if t == ticket then t { availability = newStatus } else t) (tickets c) } 
            else c) (concerts st)  
    in if any (\c -> ticket `elem` (tickets c)) (concerts st)  
        then Right $ st { concerts = updatedConcerts }  
        else Left "Ticket not found."  

or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input -> case a input of
    Right r1 -> Right r1
    Left _ -> b input

parseChar :: Char -> String -> Either String (Char, String)
parseChar c [] = Left ("Error: empty input")
parseChar c s@(h:t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

parseNumber :: String -> Either String (Int, String)
parseNumber [] = Left "Error: empty input"
parseNumber str =
    let
        digits = L.takeWhile C.isDigit str
        rest = drop (length digits) str
    in
        case digits of
            [] -> Left (rest ++ "not a number")
            _ -> Right (read digits, rest)

parseString :: String -> Either String (String, String)
parseString [] = Left "Error: empty input"
parseString str = 
    let (beforeComma, rest) = break (== ',') str
    in if null rest
       then Right (beforeComma, "")  
       else Right (beforeComma, tail rest)  

parseString1 :: String -> Either String (String, String)
parseString1 [] = Left "Error: empty input"
parseString1 str = 
    let (beforeHyphen, rest) = break (== '-') str
    in if null rest
       then Right (beforeHyphen, "")  
       else Right (beforeHyphen, tail rest) 

parseOrganizationName :: String -> Either String (String, String)
parseOrganizationName a = parseString a

parseID :: String -> Either String (Int, String)
parseID a = parseNumber a

parsePrice :: String -> Either String (Int, String)
parsePrice a = parseNumber a

parseType :: String -> Either String (String, String)
parseType [] = Left "Error: empty input"
parseType a =
    case parseString a of
        Left err -> Left err               
        Right (firstPart, rest) -> 
            if firstPart /= "Seat"
            then Left ("Error: false type")   
            else  case parseLetterAndDigit rest of
                Left err -> Left err
                Right (matched, remaining) -> 
                    case parseString remaining of
                        Left err -> Left err
                        Right(string, remainder) ->
                            if string == "Standing" || string == "VIP" || string == "Group seated" || string == "Group standing"
                                then  Right(firstPart ++ "," ++ matched ++ string, remainder) 
                                else Left  ("Error: expected standing || VIP || Group Seated || Group standing")

                
parseLetterAndDigit :: String -> Either String (String, String)
parseLetterAndDigit [] = Left "Error: insufficient input after 'Seat'"
parseLetterAndDigit (x:y:xs)  
    | isUpper x && isDigit y = Right ([x, y], xs)  
    | otherwise = Left "Error: expected uppercase letter followed by a digit"
parseLetterAndDigit _ = Left "Error: insufficient input after 'Seat'" 

parseTicket :: String -> Either String (Ticket, String)
parseTicket [] = Left "Error: empty input"
parseTicket a = 
    case parseID a of
        Left err -> Left err
        Right (matchedId, remaining) ->  
            case parseChar ',' remaining of  
                Left err -> Left err
                Right (_, remaining') -> 
                    case parseType remaining' of 
                        Left err -> Left err
                        Right (matchedType, remaining1) -> 
                            case parseString remaining1 of 
                                Left err -> Left err
                                Right (matchedAvailability, remaining2) -> 
                                    if matchedAvailability /= "Available" && matchedAvailability /= "Sold"
                                        then Left "Error: wrong availability"
                                        else case parsePrice remaining2 of 
                                            Left err -> Left err
                                            Right (matchedPrice, remaining3) -> 
                                                let ticket = Ticket
                                                                { ticketId = matchedId
                                                                , ticketType = matchedType
                                                                , availability = matchedAvailability
                                                                , price = matchedPrice
                                                                }
                                                in Right (ticket, remaining3)

parseTicketsList :: String -> Either String ([Ticket], String)
parseTicketsList str =
    case parseTicket str of
        Left _ -> Right ([], str)  -- If parsing fails, return what we've accumulated so far
        Right (ticket, remaining) ->
            if null remaining
            then Right ([ticket], "")
            else case parseChar ',' remaining of
                Left _ -> Right ([ticket], remaining)  -- No comma means no more tickets
                Right (_, rest) ->
                    case parseTicketsList rest of
                        Left err -> Left err
                        Right (otherTickets, remainingAfter) ->
                            Right (ticket : otherTickets, remainingAfter)

parseConcert :: String -> Either String (Concert, String) 
parseConcert str = 
    case parseString str of
        Left err -> Left err
        Right (matchedTitle, restTitle) -> 
            case parseString restTitle of
                Left err -> Left err
                Right (matchedArtist, restArtist) -> 
                    case parseDate restArtist of
                        Left err -> Left err
                        Right (matchedDate, restDate) -> 
                            case parseChar ',' restDate of 
                                Left err -> Left err
                                Right (_, remaining) ->  
                                    case parseTicketsList remaining of
                                        Left err -> Left err
                                        Right (matchedTicketsList, finalRemaining) ->  
                                            let concert = Concert
                                                            {
                                                                title = matchedTitle,
                                                                artist = matchedArtist,
                                                                date = matchedDate,
                                                                tickets = matchedTicketsList
                                                            }
                                            in Right (concert, finalRemaining)
        
parseConcertList :: String -> Either String ([Concert], String)
parseConcertList input = parseConcerts input []
  where
    parseConcerts :: String -> [Concert] -> Either String ([Concert], String)
    parseConcerts remaining concertsSoFar =
      case parseConcert remaining of
        Left err -> Right (concertsSoFar, remaining)  -- Parsing failed, return what we've parsed so far
        Right (concert, rest) -> parseConcerts (dropWhile (== ',') rest) (concertsSoFar ++ [concert])

splitOnComma :: String -> [String]
splitOnComma [] = []
splitOnComma s =
    let (first, rest) = break (== ',') s
    in first : case rest of
        [] -> []
        _:xs -> splitOnComma xs


parseMonth :: String -> Either String (Month, String)
parseMonth str = 
    case parseString1 str of
        Left err -> Left err
        Right (monthStr, remaining) -> 
            case monthStr of
                "January"   -> Right (January, remaining)
                "February"  -> Right (February, remaining)
                "March"     -> Right (March, remaining)
                "April"     -> Right (April, remaining)
                "May"       -> Right (May, remaining)
                "June"      -> Right (June, remaining)
                "July"      -> Right (July, remaining)
                "August"    -> Right (August, remaining)
                "September" -> Right (September, remaining)
                "October"   -> Right (October, remaining)
                "November"  -> Right (November, remaining)
                "December"  -> Right (December, remaining)
                _           -> Left "Invalid month"

parseDay :: String -> Either String (Int, String)
parseDay str =
    case parseNumber str of
        Left err -> Left err
        Right (num, rest) ->
            if num >=1 && num <=31
                then Right(num,rest)
                else Left("Eror: Invalid day")

parseDate :: String -> Either String (Date, String)
parseDate str = 
    case parseNumber str of
        Left err -> Left err
        Right (year1, rest1) -> 
            if year1 < 1000 || year1 > 9999
            then Left "Invalid year"
            else case parseChar '-' rest1 of
                Left err -> Left err
                Right (_, rest2) ->
                    case parseMonth rest2 of
                        Left err -> Left err
                        Right (month1, rest3) ->
                            case parseDay rest3 of
                                Left err -> Left err
                                Right (day1, remaining) ->  -- This line is correct
                                    let date = Date 
                                                { year = year1
                                                , hyphen1 = '-'
                                                , month = month1
                                                , hyphen2 = '-'
                                                , day = day1
                                                 }
                                        in Right (date, remaining)

                        
                    

