{-# LANGUAGE OverloadedStrings #-}

module Parser where 

import           Types 
import           Data.Time (TimeOfDay(..), TimeZone(..), utcToLocalTime, localTimeOfDay)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as AC 
import           Data.Attoparsec.Combinator (lookAhead)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Binary.Get (Get, getWord16le, getWord32le, getInt32le, runGet)
import           Data.ByteString.Lex.Fractional (readDecimal)
import           Data.Word (Word32, Word16)
import           Data.Int (Int32)

packetParser :: P.Parser (Either () QuotePacket) 
packetParser = do 
  ph <- packetHeaderParser 
  _  <- skipToQuoteOrPacketEnd 
  nb <- P.take 1 
  case nb of 
    "\255" -> return $ Left () 
    _      -> do 
            quote <- quoteParser 
            return $ Right QuotePacket { 
              packetTime = epochToTimeOfDay (tsSec ph)
            , acceptTime = time quote 
            , issueCode = issueC quote 
            , bids = bs quote 
            , asks = as quote 
            } 

epochToTimeOfDay :: Integral a => a -> TimeOfDay
epochToTimeOfDay ts = localTimeOfDay $ utcToLocalTime (TimeZone 540 False "KST") $ posixSecondsToUTCTime $ fromIntegral ts 

skipToQuoteOrPacketEnd :: P.Parser String 
skipToQuoteOrPacketEnd = AC.manyTill AC.anyChar (lookAhead quoteStart <|> lookAhead packetEnd)
  where 
    quoteStart = AC.string "B6034" 
    packetEnd = AC.string "\255"

packetHeaderParser :: P.Parser PacketHeader 
packetHeaderParser = do 
  tss <- getWord32Parser
  tsu <- getWord32Parser
  il  <- getWord32Parser
  ol  <- getWord32Parser
  return PacketHeader { 
    tsSec = tss 
  , tsUsec = tsu 
  , inclLen = il 
  , origLen = ol 
  } 

getInt32Parser :: P.Parser Int32 
getInt32Parser = do 
  n <- P.take 4 
  return $ convert getInt32le n

getWord32Parser :: P.Parser Word32
getWord32Parser = do 
  n <- P.take 4 
  return $ convert getWord32le n

getWord16Parser :: P.Parser Word16 
getWord16Parser = do 
  n <- P.take 2 
  return $ convert getWord16le n

convert :: (Integral a, Num b) => Get a -> BS.ByteString -> b
convert f s = fromIntegral . runGet f $ BL.fromStrict s
           
quoteParser :: P.Parser QuoteMessage 
quoteParser = do
  _ <- P.take 4
  issueCode' <- issueCodeParser
  _ <- P.take 12 
  bids' <- bidsParser
  _ <- P.take 7 
  asks' <- asksParser
  _ <- P.take 50 
  acceptTime' <- quoteAcceptTimeParser
  _ <- P.take 1
  return QuoteMessage { 
    time = acceptTime'
  , issueC = issueCode'
  , bs = bids' 
  , as = asks'
  } 

issueCodeParser :: P.Parser ISIN
issueCodeParser = do 
  cc <- P.count 2 letter 
  ns <- P.count 9 alphaNum 
  cd <- P.count 1 AC.digit 
  return ISIN { countryCode = cc, nsin = ns, checkDigit = read cd }

alphaNum :: P.Parser Char
alphaNum = letter <|> AC.digit

letter :: P.Parser Char
letter = AC.letter_iso8859_15

quoteAcceptTimeParser :: P.Parser TimeOfDay
quoteAcceptTimeParser = do 
  h  <- P.count 2 AC.digit 
  m  <- P.count 2 AC.digit 
  su <- P.count 4 AC.digit 
  return $ TimeOfDay (read h) (read m) (read su) 

bidsParser :: P.Parser [Bid] 
bidsParser = do 
  bids' <- P.count 5 bidParser 
  return $ reverse bids'

asksParser :: P.Parser [Ask] 
asksParser = P.count 5 askParser 

bidParser :: P.Parser Bid 
bidParser = do 
  p <- priceParser 
  q <- quantityParser 
  return $ Bid { bidQuantity = q, bidPrice = p }

askParser :: P.Parser Ask 
askParser = do 
  p <- priceParser 
  q <- quantityParser 
  return $ Ask { askQuantity = q, askPrice = p }

priceParser :: P.Parser Double
priceParser = pqParser 5

quantityParser :: P.Parser Double
quantityParser = pqParser 7

pqParser :: Int -> P.Parser Double
pqParser c = do 
  n <- P.take c 
  return $ toDouble n

toDouble :: BS.ByteString -> Double 
toDouble s = maybe 0 fst (readDecimal s)

globalHeaderParser :: P.Parser GlobalHeader 
globalHeaderParser = do 
  mn   <- getWord32Parser 
  vmaj <- getWord16Parser
  vmin <- getWord16Parser
  tz   <- getInt32Parser 
  sf   <- getWord32Parser 
  sl   <- getWord32Parser 
  nw   <- getWord32Parser 
  return GlobalHeader { 
    magicNumber = mn 
  , versionMajor = vmaj 
  , versionMinor = vmin 
  , thisZone = tz 
  , sigfigs = sf 
  , snaplen = sl 
  , network = nw 
  } 

fileName :: String 
fileName = "./data/mdf-kospi200.20110216-0.pcap"
