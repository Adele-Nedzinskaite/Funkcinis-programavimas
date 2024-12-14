{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}

import GHC.Stack qualified as Lib3
import GHC.Stack.CCS qualified as Lib3
import Lib1 qualified
import Lib2 qualified
import Lib3 qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Lib1 tests"
    [ testCase "List of completions is not empty" $
        null Lib1.completions @?= False,
      testCase "Bad query" $
        let q = "add_concert_tickets_seller Organizer"
         in Lib2.parseQuery q @?= Left ("Unknown command: " ++ q),
      testCase "Sell/Return ticket that doesn't exist" $
        let q = (Lib2.SellTicket (Lib2.Ticket {Lib2.ticketId = 1, Lib2.ticketType = "Seat,A1VIP", Lib2.availability = "Sold", Lib2.price = 100}))
            st = Lib2.emptyState
         in Lib2.stateTransition st q @?= Left "Ticket not found.",
      testCase "Sell/Return ticket that is already Sold/Available" $
        let q = Lib2.SellTicket (Lib2.Ticket {Lib2.ticketId = 1, Lib2.ticketType = "Seat,A1VIP", Lib2.availability = "Sold", Lib2.price = 100})
            st = Lib2.State {Lib2.sellerName = "", Lib2.concerts = [Lib2.Concert {Lib2.title = "T", Lib2.artist = "A", Lib2.date = Lib2.Date {Lib2.year = 2000, Lib2.hyphen1 = '-', Lib2.month = Lib2.January, Lib2.hyphen2 = '-', Lib2.day = 1}, Lib2.tickets = [Lib2.Ticket {Lib2.ticketId = 1, Lib2.ticketType = "Seat,A1VIP", Lib2.availability = "Sold", Lib2.price = 1}]}]}
         in Lib2.stateTransition st q @?= Left "Ticket is already Sold",
      testCase "Add ticket with the same ID" $
        let concert = Lib2.Concert {Lib2.title = "T", Lib2.artist = "A", Lib2.date = Lib2.Date {Lib2.year = 2000, Lib2.hyphen1 = '-', Lib2.month = Lib2.January, Lib2.hyphen2 = '-', Lib2.day = 1}, Lib2.tickets = [Lib2.Ticket {Lib2.ticketId = 1, Lib2.ticketType = "Seat,A1VIP", Lib2.availability = "Sold", Lib2.price = 1}]}
            ticket = Lib2.Ticket {Lib2.ticketId = 1, Lib2.ticketType = "Seat,A2Standing", Lib2.availability = "Available", Lib2.price = 100}
            q = Lib2.AddTicket concert ticket
            st = Lib2.State {Lib2.sellerName = "", Lib2.concerts = [concert]}
         in Lib2.stateTransition st q @?= Left "Ticket with the same ID already exists.",
      testCase "Change non existent concert information" $
        let concert1 = Lib2.Concert {Lib2.title = "T1", Lib2.artist = "A", Lib2.date = Lib2.Date {Lib2.year = 2000, Lib2.hyphen1 = '-', Lib2.month = Lib2.January, Lib2.hyphen2 = '-', Lib2.day = 1}, Lib2.tickets = [Lib2.Ticket {Lib2.ticketId = 1, Lib2.ticketType = "Seat,A1VIP", Lib2.availability = "Sold", Lib2.price = 1}]}
            concert2 = Lib2.Concert {Lib2.title = "T2", Lib2.artist = "A", Lib2.date = Lib2.Date {Lib2.year = 2000, Lib2.hyphen1 = '-', Lib2.month = Lib2.January, Lib2.hyphen2 = '-', Lib2.day = 1}, Lib2.tickets = [Lib2.Ticket {Lib2.ticketId = 1, Lib2.ticketType = "Seat,A1VIP", Lib2.availability = "Sold", Lib2.price = 1}]}
            st = Lib2.emptyState
            q = Lib2.UpdateConcert concert1 concert2
         in Lib2.stateTransition st q @?= Left "Concert not found."
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property test"
    [ QC.testProperty "parseStatements (renderStatements s) == Right(s,\" \")" $
        \s -> Lib3.parseStatements (Lib3.renderStatements s) == Right (s, "")
    ]

instance QC.Arbitrary Lib3.Statements where
  arbitrary = QC.sized $ \n ->
    if n <= 1
      then Lib3.Single <$> QC.arbitrary
      else
        QC.oneof
          [ Lib3.Single <$> QC.arbitrary,
            Lib3.Batch <$> QC.vectorOf (n `div` 2) QC.arbitrary
          ]

instance QC.Arbitrary Lib2.Query where
  arbitrary :: QC.Gen Lib2.Query
  arbitrary =
    QC.oneof
      [ pure Lib2.ShowState,
        pure Lib2.RemoveConcertTicketsSeller,
        Lib2.CheckAvailableTickets <$> QC.arbitrary,
        Lib2.RemoveConcert <$> QC.arbitrary,
        Lib2.AddConcert <$> QC.arbitrary,
        Lib2.SellTicket <$> QC.arbitrary,
        Lib2.ReturnTicket <$> QC.arbitrary,
        Lib2.AddTicket <$> QC.arbitrary <*> QC.arbitrary,
        Lib2.RemoveTicket <$> QC.arbitrary <*> QC.arbitrary,
        Lib2.AddConcertTicketsSeller <$> QC.listOf1 genAlphaNumChar <*> QC.arbitrary
      ]

instance QC.Arbitrary Lib2.Concert where
  arbitrary =
    Lib2.Concert
      <$> QC.listOf1
        (QC.elements ['a' .. 'z']) -- title
      <*> QC.listOf1
        (QC.elements ['a' .. 'z']) -- artist
      <*> QC.arbitrary -- date
      <*> QC.vectorOf 2 QC.arbitrary

instance QC.Arbitrary Lib2.Date where
  arbitrary =
    Lib2.Date
      <$> QC.choose (2000, 2100) -- year
      <*> pure '-' -- hyphen1
      <*> QC.elements [Lib2.January .. Lib2.December] -- month
      <*> pure '-' -- hyphen2
      <*> QC.choose (1, 31) -- day

instance QC.Arbitrary Lib2.Ticket where
  arbitrary =
    Lib2.Ticket
      <$> QC.choose (1, 1000) -- ticketId
      <*> QC.elements ["Seat,A1VIP"] -- ticketType
      <*> QC.elements ["Available", "Sold"] -- availability
      <*> QC.choose (1, 1000) -- price

instance QC.Arbitrary Lib2.Month where
  arbitrary =
    QC.elements
      [ Lib2.January,
        Lib2.February,
        Lib2.March,
        Lib2.April,
        Lib2.May,
        Lib2.June,
        Lib2.July,
        Lib2.August,
        Lib2.September,
        Lib2.October,
        Lib2.November,
        Lib2.December
      ]

genAlphaNumChar :: QC.Gen Char
genAlphaNumChar = QC.elements $ ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']