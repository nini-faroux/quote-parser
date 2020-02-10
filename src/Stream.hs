module Stream where 

import Types
import Parser
import Conduit
import Data.Conduit.Attoparsec
import Control.Monad.IO.Class (liftIO)

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
