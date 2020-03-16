module Stream (stream) where 

import Types (QuotePacket(..))
import Parser (packetParser)
import Conduit (ConduitT, ResourceT, runConduitRes, sourceFile, await, (.|))
import Data.Conduit.Attoparsec (ParseError, conduitParserEither)
import Data.Time.LocalTime (todSec)
import Data.Foldable (traverse_)
import Data.Fixed (Pico, mod')
import Control.Monad.IO.Class (liftIO)

-- | Conduit pipeline, parserSink awaits data from upstream
-- upstream source file pipes data downstream to the packet parser which attempts to parse each chunk
-- if successful it sends the parsed data to parserSink which processes it and then awaits again
stream :: Bool -> FilePath -> IO ()
stream sortFlag fp = runConduitRes $ sourceFile fp .| conduitParserEither packetParser .| parserSink sortFlag [] 3

-- | Sink for printing quote packets 
-- Use a temporary list buffer to store quotes within 3 seconds intervals 
-- (assumption from problem statement that difference between packetTime and quote accept time is never more than 3 seconds)
-- When we reach the end of the next 3 second interval, if the sort flag is set we sort and print the quotes, otherwise 
-- print the buffer as is, and then reset the buffer to empty list and update the limit to 3 seconds ahead
parserSink :: Bool -> [QuotePacket] -> Pico -> ConduitT (Either ParseError (a, Either () QuotePacket)) o (ResourceT IO) ()
parserSink sortFlag buffer limit = do 
  nextPacket <- await 
  case nextPacket of 
    Nothing -> liftIO (printQuotes sortFlag buffer) >> endOfInput
    Just parsed -> 
      case parsed of 
        Left err -> printError err
        Right (_, packet) -> 
          case packet of 
            Left () -> loop buffer 
            Right quotePacket -> 
              if checkLimit limit quotePacket 
              then liftIO (printQuotes sortFlag (prependQuote quotePacket)) >> resetLoop
              else loop (prependQuote quotePacket) 
  where 
    resetLoop = parserSink sortFlag [] (updateLimit limit) 
    loop quotes = parserSink sortFlag quotes limit
    prependQuote quote = quote : buffer
    printError e = liftIO $ print e
    endOfInput = liftIO $ putStrLn "End of Input"

-- | Sort a list of quote packets based on their acceptTime field
sortQuotes :: [QuotePacket] -> [QuotePacket]
sortQuotes = sort' . reverse

sort' :: [QuotePacket] -> [QuotePacket]
sort' []  = []
sort' [q] = [q]
sort' qs  = merge (sort' firstPart) (sort' secondPart) []
  where 
    (firstPart, secondPart) = (take n qs, drop n qs) 
    n = length qs `div` 2

merge :: [QuotePacket] -> [QuotePacket] -> [QuotePacket] -> [QuotePacket]
merge [] qs2 acc = reverse acc ++ qs2
merge qs1 [] acc = reverse acc ++ qs1
merge qs1@(x:xs) qs2@(y:ys) acc 
    | acceptTime x < acceptTime y = merge xs qs2 (x:acc)
    | otherwise                   = merge qs1 ys (y:acc) 

printQuotes :: Bool -> [QuotePacket] -> IO ()
printQuotes sortFlag quotes
  | sortFlag  = print' $ sortQuotes quotes
  | otherwise = print' $ reverse quotes
  where print' = traverse_ print 

-- | Check if the packet time has reached 
-- the next 3 second limit
checkLimit :: Pico -> QuotePacket -> Bool 
checkLimit limit qp 
  | packetTimeSecs == limit = True 
  | otherwise               = False 
  where 
    packetTimeSecs = todSec $ packetTime qp

-- | Set the next limit time 
updateLimit :: Pico -> Pico 
updateLimit limit = (limit + 3) `mod'` 60
