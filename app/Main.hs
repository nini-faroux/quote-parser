module Main where

import Options.Applicative (execParser, str, metavar, switch, short, long, help, argument, info)
import Stream (stream)
import StreamSorted (streamSorted)

-- | Program entry point, requires path to input file
-- and optional flag for sorting output
main :: IO () 
main = execParser opts >>= run 
  where 
    parser = Quote <$> argument str (metavar "<FILEPATH>") 
                   <*> switch (short 'r' <> long "sort" <> help "Sort output on packet accept time") 
    opts = info parser mempty 

-- | Record representing command inputs
data Quote = 
  Quote { 
    inputFile :: FilePath
  , sortedOutput :: Bool 
  } 

-- | If the user supplies '-r' or '--sort' flag 
-- apply the 'streamSorted' function to the input
-- otherwise apply stream function
run :: Quote -> IO () 
run opts 
  | sortedOutput opts = streamSorted fp 
  | otherwise         = stream fp 
  where fp = inputFile opts 
