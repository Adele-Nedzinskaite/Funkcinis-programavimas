{-# LANGUAGE InstanceSigs #-}

module Parsers
  ( Parser (..),
    parse,
    parseChar,
    parseString,
    parseNumber,
    lift,
    throwE,
    catchE,
    ExceptT (..),
    State (..),
    get,
    put,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.Trans.State (State, get, put, runState)
import Data.Char (isDigit)

type Parser a = ExceptT String (State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)

parseChar :: Char -> Parser Char
parseChar c = do
  input <- lift get
  case input of
    [] -> throwE "Char Error: empty input"
    (x : xs) ->
      if x == c
        then do
          lift $ put xs
          return c
        else throwE $ c : " is not found in " ++ input

parseString :: Parser String
parseString = do
  input <- lift get
  case break (== ',') input of
    ("", _) -> throwE "String Error: empty input"
    (word, []) -> do
      lift $ put ""
      return word
    (word, _ : rest) -> do
      lift $ put rest
      return word

parseNumber :: Parser Int
parseNumber = do
  input <- lift get
  case span isDigit input of
    ("", _) -> throwE "Num Error: empty input"
    (digits, rest) -> do
      lift $ put rest
      return $ read digits