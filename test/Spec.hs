{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
    testCase "Bad query" $
        let q = "add_concert_tickets_seller Organizer"
        in Lib2.parseQuery q @?= Left ("Unknown command " ++ q),
    testCase "Sell/Return ticket that doesn't exist" $
      let q = (Lib2.SellTicket (Lib2.Ticket {Lib2.ticketId = 1, Lib2.ticketType = "Seat,A1VIP", Lib2.availability = "Sold", Lib2.price = 100}))
          st = Lib2.emptyState
      in Lib2.stateTransition st q @?= Left "Ticket not found.",
    testCase "Sell/Return ticket that is already Sold/Available" $
      let 
        q = Lib2.SellTicket (Lib2.Ticket {Lib2.ticketId = 1, Lib2.ticketType = "Seat,A1VIP", Lib2.availability = "Sold", Lib2.price = 100})
        st = Lib2.State {Lib2.sellerName = "", Lib2.concerts = [Lib2.Concert {Lib2.title = "T", Lib2.artist = "A", Lib2.date = Lib2.Date {Lib2.year = 2000, Lib2.hyphen1 = '-', Lib2.month = Lib2.January, Lib2.hyphen2 = '-', Lib2.day = 1}, Lib2.tickets = [Lib2.Ticket {Lib2.ticketId = 1, Lib2.ticketType = "Seat,A1VIP", Lib2.availability = "Sold", Lib2.price = 1}]}]}
      in Lib2.stateTransition st q @?= Left "Ticket is already Sold",
    testCase "Add ticket with the same ID" $ 
      let 
        concert = Lib2.Concert {Lib2.title = "T", Lib2.artist = "A", Lib2.date = Lib2.Date {Lib2.year = 2000, Lib2.hyphen1 = '-', Lib2.month = Lib2.January, Lib2.hyphen2 = '-', Lib2.day = 1}, Lib2.tickets = [Lib2.Ticket {Lib2.ticketId = 1, Lib2.ticketType = "Seat,A1VIP", Lib2.availability = "Sold", Lib2.price = 1}]}
        ticket = Lib2.Ticket {Lib2.ticketId = 1, Lib2.ticketType = "Seat,A2Standing", Lib2.availability = "Available", Lib2.price = 100}
        q = Lib2.AddTicket concert ticket
        st = Lib2.State {Lib2.sellerName = "", Lib2.concerts = [concert]}
      in Lib2.stateTransition st q @?= Left "Ticket with the same ID already exists.",
    testCase "Change non existent concert information" $ 
      let 
          concert1 = Lib2.Concert {Lib2.title = "T1", Lib2.artist = "A", Lib2.date = Lib2.Date {Lib2.year = 2000, Lib2.hyphen1 = '-', Lib2.month = Lib2.January, Lib2.hyphen2 = '-', Lib2.day = 1}, Lib2.tickets = [Lib2.Ticket {Lib2.ticketId = 1, Lib2.ticketType = "Seat,A1VIP", Lib2.availability = "Sold", Lib2.price = 1}]}
          concert2 = Lib2.Concert {Lib2.title = "T2", Lib2.artist = "A", Lib2.date = Lib2.Date {Lib2.year = 2000, Lib2.hyphen1 = '-', Lib2.month = Lib2.January, Lib2.hyphen2 = '-', Lib2.day = 1}, Lib2.tickets = [Lib2.Ticket {Lib2.ticketId = 1, Lib2.ticketType = "Seat,A1VIP", Lib2.availability = "Sold", Lib2.price = 1}]}
          st = Lib2.emptyState
          q = Lib2.UpdateConcert concert1 concert2
      in Lib2.stateTransition st q @?= Left "Concert not found."

  ]