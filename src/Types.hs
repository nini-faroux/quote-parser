module Types
  ( QuotePacket (..),
    QuoteMessage (..),
    PacketHeader (..),
    Bid (..),
    Ask (..),
    ISIN (..),
    GlobalHeader (..),
  )
where

import Data.Int (Int32)
import Data.Time (TimeOfDay)
import Data.Word (Word16, Word32)

-- | Type for representing relevant
-- fields of a parsed quote packet
data QuotePacket
  = QuotePacket
      { packetTime :: !TimeOfDay,
        acceptTime :: !TimeOfDay,
        issueCode :: !ISIN,
        bids :: ![Bid],
        asks :: ![Ask]
      } deriving Eq

-- | Pcap Packet header per pcap docs
data PacketHeader
  = PacketHeader
      { tsSec :: !Word32,
        tsUsec :: !Word32,
        inclLen :: !Word32,
        origLen :: !Word32
      }
  deriving (Show)

-- | Type for relevant fields of
-- the data section of a quote packet
data QuoteMessage
  = QuoteMessage
      { time :: !TimeOfDay,
        issueC :: !ISIN,
        bs :: ![Bid],
        as :: ![Ask]
      }

-- | IssueCode type
data ISIN
  = ISIN
      { countryCode :: !String,
        nsin :: !String,
        checkDigit :: !Int
      } deriving Eq

data Bid
  = Bid
      { bidQuantity :: !Double,
        bidPrice :: !Double
      } deriving Eq

data Ask
  = Ask
      { askQuantity :: !Double,
        askPrice :: !Double
      } deriving Eq

-- | Pcap packet global header per docs
data GlobalHeader
  = GlobalHeader
      { magicNumber :: !Word32,
        versionMajor :: !Word16,
        versionMinor :: !Word16,
        thisZone :: !Int32,
        sigfigs :: !Word32,
        snaplen :: !Word32,
        network :: !Word32
      }
  deriving (Show, Eq)

-- | Display helpers, custom Show instances
-- and functions for clearer results output
instance Show QuotePacket where
  show = showPacket

instance Show ISIN where
  show = showIssueCode

instance Show Bid where
  show = showBid

instance Show Ask where
  show = showAsk

showPacket :: QuotePacket -> String
showPacket quote =
  "<Packet Details> \n"
    ++ "PacketTime: "
    ++ show (packetTime quote)
    ++ ", \n"
    ++ "AcceptTime: "
    ++ show (acceptTime quote)
    ++ ", \n"
    ++ "IssueCode, ISIN: "
    ++ show (issueCode quote)
    ++ "\n\n"
    ++ "<Best Five Bids, 5th to 1st> \n"
    ++ showBids (bids quote)
    ++ "\n"
    ++ "<Best Five Asks, 1st to 5th> \n"
    ++ showAsks (asks quote)
    ++ "<End Packet>"
    ++ "\n"

showIssueCode :: ISIN -> String
showIssueCode issueCode' =
  "CountryCode: "
    ++ countryCode issueCode'
    ++ ", "
    ++ "NSIN: "
    ++ nsin issueCode'
    ++ ", "
    ++ "CheckDigit: "
    ++ show (checkDigit issueCode')

showBid :: Bid -> String
showBid bid =
  "Quantity: " ++ show (bidQuantity bid) ++ ", "
    ++ "Price: "
    ++ show (bidPrice bid)

showAsk :: Ask -> String
showAsk ask =
  "Quantity: " ++ show (askQuantity ask) ++ ", "
    ++ "Price: "
    ++ show (askPrice ask)

showBids :: [Bid] -> String
showBids bids' = go bids' 5
  where
    go [] _ = ""
    go (b : bs') n = "Bid " ++ show n ++ ": " ++ show b ++ "\n" ++ go bs' (n - 1)

showAsks :: [Ask] -> String
showAsks asks' = go asks' 1
  where
    go [] _ = ""
    go (a : as') n = "Ask " ++ show n ++ ": " ++ show a ++ "\n" ++ go as' (n + 1)
