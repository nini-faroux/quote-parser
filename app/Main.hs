{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative

import Stream (stream)
import StreamSorted (streamSorted)

main :: IO () 
main = execParser opts >>= run 
  where 
    parser = Quote <$> argument str (metavar "<FILEPATH>") 
                   <*> switch (short 'r' <> long "sort" <> help "Sort output on packet accept time") 
    opts = info parser mempty 

data Quote = 
  Quote { 
    inputFile :: FilePath
  , sortedOutput :: Bool 
  } 

run :: Quote -> IO () 
run opts 
  | sortedOutput opts = streamSorted fp 
  | otherwise         = stream fp 
  where fp = inputFile opts 
