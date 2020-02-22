module Main where

import Options.Applicative (execParser, str, metavar, switch, short, long, help, argument, info)
import qualified Data.ByteString as BS
import Stream (stream)

-- | Program entry point, requires path to input file
-- and optional flag for sorting output
main :: IO () 
main = execParser options >>= run 
  where 
    parser = Quote <$> argument str (metavar "<FILEPATH>") 
                   <*> switch (short 'r' <> long "sort" <> help "Sort output on packet accept time") 
    options = info parser mempty 

-- | Record representing command inputs
data Quote = 
  Quote { 
    inputFilePath :: FilePath
  , sortedOutput  :: Bool 
  } 

-- | If the user supplies '-r' or '--sort' flag 
-- sort the output otherwise output in given order
run :: Quote -> IO () 
run options 
  | sortedFlag = stream' True
  | otherwise  = stream' False
  where 
    stream' sortFlag = skipGlobalHeader filePath >> stream sortFlag source
    sortedFlag = sortedOutput options
    filePath = inputFilePath options 

-- | The first 24 bytes are just the pcap global header
skipGlobalHeader :: FilePath -> IO ()
skipGlobalHeader fp = BS.readFile fp >>= \bytes -> BS.writeFile source (BS.drop 24 bytes)

source :: FilePath 
source = "./data/source.pcap"
