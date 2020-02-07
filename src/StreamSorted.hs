module StreamSorted where 

import Types
import Parser
import Conduit
import Data.Time.LocalTime (todSec)
import Data.Foldable (traverse_)
import Data.Conduit.Attoparsec
import Data.Fixed (Pico, mod')
import Control.Monad.IO.Class (liftIO)

streamSorted :: IO ()
streamSorted = runConduitRes $ sourceFile fileName .| conduitParserEither packetParser .| parserSink [] 3

parserSink :: [QuotePacket] -> Pico -> ConduitT (Either ParseError (a, Either () QuotePacket)) o (ResourceT IO) ()
parserSink buffer limit = do 
  nextPacket <- await 
  case nextPacket of 
    Nothing -> liftIO $ printBuffer buffer >> liftIO (putStrLn "End of Input")
    Just parsed -> 
      case parsed of 
      Left err -> liftIO $ print err 
      Right (_, quotePacket) -> 
        case quotePacket of 
          Left () -> parserSink buffer limit 
          Right quotePacket -> 
            if checkLimit limit quotePacket 
            then liftIO (printBuffer buffer) >> parserSink [] (updateLimit limit)
            else parserSink (quotePacket : buffer) limit

sortQuotes :: [QuotePacket] -> [QuotePacket] 
sortQuotes [] = [] 
sortQuotes (p:qs) = sortQuotes [q | q <- qs, qt q <= pivTime] ++ [p] ++ sortQuotes [q | q <- qs, qt q > pivTime] 
  where 
    pivTime = acceptTime p
    qt = acceptTime 

printBuffer :: [QuotePacket] -> IO () 
printBuffer qs = do 
  let sqs = sortQuotes qs 
  traverse_ print sqs

checkLimit :: Pico -> QuotePacket -> Bool 
checkLimit limit qp 
  | packetTimeSecs == limit = True 
  | otherwise               = False 
  where 
    packetTimeSecs = todSec $ packetTime qp

updateLimit :: Pico -> Pico 
updateLimit limit = (limit + 3) `mod'` 60
