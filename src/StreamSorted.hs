module StreamSorted where 

import Types
import Parser
import Conduit
import Data.Conduit.Attoparsec
import Control.Monad.IO.Class (liftIO)

streamSorted :: IO ()
streamSorted = runConduitRes $ sourceFile fileName .| conduitParserEither packetParser .| parserSink [] 0

parserSink :: [QuotePacket] -> Int -> ConduitT (Either ParseError (a, Either () QuotePacket)) o (ResourceT IO) ()
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
            then liftIO (printBuffer buffer) >> parserSink [] 0 
            else parserSink (quotePacket : buffer) (updateSeconds limit quotePacket)

sortQuotes :: [QuotePacket] -> [QuotePacket] 
sortQuotes [] = [] 
sortQuotes (p:qs) = sortQuotes [q | q <- qs, qt q <= pivTime] ++ [p] ++ sortQuotes [q | q <- qs, qt q > pivTime] 
  where 
    pivTime = acceptTime p
    qt = acceptTime 

printBuffer :: [QuotePacket] -> IO () 
printBuffer qs = do 
  let sqs = sortQuotes qs 
  print sqs

checkLimit :: Int -> QuotePacket -> Bool 
checkLimit limit qp = undefined 

updateSeconds :: Int -> QuotePacket -> Int 
updateSeconds limit qp = undefined
