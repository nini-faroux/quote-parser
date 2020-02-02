{-# LANGUAGE OverloadedStrings #-}

module Parser where 

import           Types 
import           Data.Time
import           Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as AC 
import qualified Data.ByteString as BS

skipThenQuoteParser :: P.Parser QuoteMessage 
skipThenQuoteParser = skipToQuote *> quoteParser

quoteParser :: P.Parser QuoteMessage 
quoteParser = do
  issueCode <- issueCodeParser
  _ <- P.take 12 
  bids <- bidsParser
  _ <- P.take 7 
  asks <- asksParser
  _ <- P.take 50 
  acceptTime <- quoteAcceptTimeParser
  return QuoteMessage { 
              packetTime = acceptTime
            , acceptTime = acceptTime
            , issueCode = issueCode
            , bids = bids 
            , asks = asks
         } 

skipToQuote :: P.Parser String 
skipToQuote = AC.manyTill AC.anyChar quoteStart

quoteStart :: P.Parser BS.ByteString 
quoteStart = AC.string "B6034"

issueCodeParser :: P.Parser ISIN
issueCodeParser = do 
  cc <- P.count 2 letter 
  ns <- P.count 9 alphaNum 
  cd <- P.count 1 AC.digit 
  return $ ISIN { countryCode = cc, nsin = ns, checkDigit = read cd }

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
  bs <- P.count 5 bidParser 
  return $ reverse bs

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

priceParser :: P.Parser Int 
priceParser = pqParser 5

quantityParser :: P.Parser Int 
quantityParser = pqParser 7

pqParser :: Int -> P.Parser Int 
pqParser c = do 
  n <- P.count c AC.digit 
  return $ read n

parseN :: IO () 
parseN = do 
  input <- pcapFile
  case P.parseOnly takeN input of 
    Right result -> print result 
    Left err     -> putStrLn err

takeN :: P.Parser BS.ByteString
takeN = P.take 12000 

pcapFile :: IO BS.ByteString 
pcapFile = BS.readFile fileName

fileName :: String 
fileName = "./data/mdf-kospi200.20110216-0.pcap"
