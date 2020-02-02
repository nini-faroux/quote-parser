module Stream where 

import Types
import Parser
import Conduit
import Data.Conduit.Attoparsec
import Control.Monad.IO.Class (liftIO)

stream :: IO ()
stream = runConduitRes $ sourceFile fileName .| conduitParserEither skipThenQuoteParser .| parserSink
  where 
    parserSink = do 
      nextMessage <- await 
      case nextMessage of 
        Nothing -> liftIO $ putStrLn "End of Input"
        Just parsed -> 
          case parsed of 
            Left err -> liftIO $ print err 
            Right (_, quoteMessage) -> do 
                liftIO $ print quoteMessage 
                parserSink 
