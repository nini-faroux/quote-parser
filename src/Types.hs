module Types where

import Data.Time (TimeOfDay)
import Data.Word
import Data.Int

data QuotePacket = 
  QuotePacket { 
    packetTime :: TimeOfDay
  , acceptTime :: TimeOfDay 
  , issueCode  :: ISIN 
  , bids       :: [Bid] 
  , asks       :: [Ask] 
  } deriving (Show)

data PacketHeader = 
  PacketHeader { 
    tsSec   :: Word32 
  , tsUsec  :: Word32 
  , inclLen :: Word32 
  , origLen :: Word32 
  } deriving (Show)

data QuoteMessage = 
  QuoteMessage { 
    time   :: TimeOfDay
  , issueC :: ISIN
  , bs     :: [Bid] 
  , as     :: [Ask] 
  } deriving (Show) 

data ISIN = 
  ISIN { 
    countryCode :: String
  , nsin        :: String
  , checkDigit  :: Int
  } deriving (Show)

data Bid = 
  Bid { 
    bidQuantity :: Double
  , bidPrice    :: Double
  } deriving (Show) 

data Ask = 
  Ask { 
    askQuantity :: Double
  , askPrice    :: Double
  } deriving (Show)

data GlobalHeader = 
  GlobalHeader { 
    magicNumber   :: Word32 
  , versionMajor  :: Word16
  , versionMinor  :: Word16 
  , thisZone      :: Int32 
  , sigfigs       :: Word32 
  , snaplen       :: Word32 
  , network       :: Word32 
  } deriving (Show) 
