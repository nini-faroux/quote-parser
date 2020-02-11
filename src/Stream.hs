module Stream where 

import Types (QuotePacket)
import Parser (packetParser)
import Conduit (ConduitT, ResourceT, runConduitRes, sourceFile, await, (.|))
import Data.Conduit.Attoparsec (ParseError, conduitParserEither)
import Control.Monad.IO.Class (liftIO)

-- | See StreamSorted comments, the same idea except here we don't need to sort the data 
-- based on the quote accept time, simply print packets in the order they are passed on
stream :: FilePath -> IO ()
stream fp = runConduitRes $ sourceFile fp .| conduitParserEither packetParser .| parserSink

parserSink :: ConduitT (Either ParseError (a, Either () QuotePacket)) o (ResourceT IO) ()
parserSink = do 
  nextMessage <- await 
  case nextMessage of 
    Nothing -> liftIO $ putStrLn "End of Input"
    Just parsed -> 
      case parsed of 
      Left err -> liftIO $ print err 
      Right (_, quoteMessage) -> 
        case quoteMessage of 
          Left () -> parserSink 
          Right quote -> liftIO (print quote) >> parserSink 
