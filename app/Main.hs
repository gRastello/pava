module Main where

import Pava.Lib

import System.Environment
import System.Exit
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  filename <- getArgs >>= \x -> if null x then help else return $ head x
  parserOutput <- parseFromFile pava filename
  case parserOutput of
    Left err -> print err
    Right derivation -> putStrLn $ check derivation

help :: IO a
help = putStrLn "Usage: pava FILE" >> exitFailure
