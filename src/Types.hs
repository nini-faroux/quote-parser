module Types where

import           Data.Time (TimeOfDay)

data QuoteMessage = 
  QuoteMessage { 
    packetTime :: TimeOfDay
  , acceptTime :: TimeOfDay
  , issueCode  :: ISIN
  , bids       :: [Bid] 
  , asks       :: [Ask] 
  } deriving (Show) 

data ISIN = 
  ISIN { 
    countryCode :: String
  , nsin        :: String
  , checkDigit  :: Int
  } deriving (Show)

data Bid = 
  Bid { 
    bidQuantity :: Int 
  , bidPrice    :: Int 
  } deriving (Show) 

data Ask = 
  Ask { 
    askQuantity :: Int 
  , askPrice    :: Int 
  } deriving (Show)
