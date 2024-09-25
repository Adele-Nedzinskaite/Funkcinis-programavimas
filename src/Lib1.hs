module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = ["upper" , "lower" , "letter", "digit", "number", "alphanumeric", "char", "string",
"concert_tickets_seller", "concert_list", "concert", "title", "artist", "year", "tickets_list",
"ticket", "ID", "type", "availability", "price"]
