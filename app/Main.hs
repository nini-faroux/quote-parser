{-# LANGUAGE OverloadedStrings #-}

module Main where

import Stream (stream)
import StreamSorted (streamSorted)
import System.Environment (getArgs)

main :: IO ()
main = do 
  args <- getArgs' 
  let file = head args
  if length args == 1 
  then stream file
  else streamSorted file

getArgs' :: IO [String]
getArgs' = getArgs >>= parse
  where
    parse :: [String] -> IO [String]
    parse [] = return [defaultFile]
    parse [filePath] = return [filePath]
    parse [filePath, sortFlag] 
      | sortFlag /= "r" = error usage
      | otherwise       = return [filePath, sortFlag] 
    parse _ = error usage

usage :: String
usage = "Usage: stock-parser <filepath> <optional: -r>"

defaultFile :: String 
defaultFile = "./data/mdf-kospi200.20110216-0.pcap"
