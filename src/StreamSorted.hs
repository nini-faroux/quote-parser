module StreamSorted where 

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
streamSorted :: FilePath -> IO ()
streamSorted fp = runConduitRes $ sourceFile fp .| conduitParserEither packetParser .| parserSink [] 3

-- | Sink for printing quote packets 
-- Use a temporary list buffer to store quotes within 3 seconds intervals 
-- (assumption from problem statement that difference between packetTime and quote accept time is never more than 3 seconds)
-- When we reach the end of the next 3 seconds interval, sort and print the quotes, 
-- reset the buffer to empty list and update the limit to 3 seconds ahead
parserSink :: [QuotePacket] -> Pico -> ConduitT (Either ParseError (a, Either () QuotePacket)) o (ResourceT IO) ()
parserSink buffer limit = do 
  nextPacket <- await 
  case nextPacket of 
    Nothing -> liftIO $ printBuffer buffer >> liftIO (putStrLn "End of Input")
    Just parsed -> 
      case parsed of 
      Left err -> liftIO $ print err 
      Right (_, packet) -> 
        case packet of 
          Left () -> parserSink buffer limit 
          Right quotePacket -> 
            if checkLimit limit quotePacket 
            then liftIO (printBuffer (quotePacket : buffer)) >> parserSink [] (updateLimit limit)
            else parserSink (quotePacket : buffer) limit

-- | Sort a list of quote packets based on their acceptTime field
sortQuotes :: [QuotePacket] -> [QuotePacket] 
sortQuotes [] = [] 
sortQuotes (p:qs) = sortQuotes [q | q <- qs, qt q <= pivTime] ++ [p] ++ sortQuotes [q | q <- qs, qt q > pivTime] 
  where 
    pivTime = acceptTime p
    qt = acceptTime 

-- | Sort and then print packets
printBuffer :: [QuotePacket] -> IO () 
printBuffer qs = do 
  let sqs = sortQuotes qs 
  traverse_ print sqs

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
