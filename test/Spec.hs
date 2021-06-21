{-# LANGUAGE OverloadedStrings #-}

import Hedgehog 
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS 
import Data.Time
import Data.Foldable (traverse_)
import Parser
import Types

main :: IO ()
main = traverse_ printCheck parserChecks

printCheck :: IO Bool -> IO () 
printCheck m = m >>= \b -> print b

parserChecks :: [IO Bool] 
parserChecks = check <$> [globalHeaderInputTest, packetParserQuoteTest, packetParserQuoteTest, packetParserNonQuoteTest]

packetParserQuoteTest :: Property 
packetParserQuoteTest = property $ P.parseOnly packetParser quotePacketInput === Right (Right quotePacketShouldBe)

packetParserNonQuoteTest :: Property 
packetParserNonQuoteTest = property $ P.parseOnly packetParser nonQuotePacketInput === Right (Left ())

globalHeaderInputTest :: Property 
globalHeaderInputTest = property $ P.parseOnly globalHeaderParser globalHeaderInput === Right globalHeaderInputShouldBe

-- | Expected Results
quotePacketShouldBe :: QuotePacket
quotePacketShouldBe = 
  QuotePacket { 
    packetTime = TimeOfDay 09 00 00.947704
  , acceptTime = TimeOfDay 09 00 00.88
  , issueCode = ISIN { countryCode = "KR", nsin = "4301F3252", checkDigit = 1 } 
  , bids = bidList 
  , asks = askList 
  } 

bidList :: [Bid] 
bidList = 
  [ Bid {bidQuantity = 135.0, bidPrice = 0.0}
  , Bid {bidQuantity = 136.0, bidPrice = 1.0} 
  , Bid {bidQuantity = 137.0, bidPrice = 18.0} 
  , Bid {bidQuantity = 138.0, bidPrice = 0.0} 
  , Bid {bidQuantity = 139.0, bidPrice = 4.0}
  ]

askList :: [Ask] 
askList =
  [ Ask {askQuantity = 141.0, askPrice = 130.0}
  , Ask {askQuantity = 142.0, askPrice = 8.0} 
  , Ask {askQuantity = 143.0, askPrice = 91.0} 
  , Ask {askQuantity = 144.0, askPrice = 13.0} 
  , Ask {askQuantity = 145.0, askPrice = 20.0 }
  ]

globalHeaderInputShouldBe :: GlobalHeader 
globalHeaderInputShouldBe = GlobalHeader {magicNumber = 2712847316, versionMajor = 2, versionMinor = 4, thisZone = 0, sigfigs = 0, snaplen = 65535, network = 1}

-- | Test inputs
quotePacketInput :: BS.ByteString 
quotePacketInput =  "\128\DC3[M\248u\SO\NUL\SOH\SOH\NUL\NUL\SOH\SOH\NUL\NUL\SOH\NUL^%6>\NUL\DC2D\200\&8\n\b\NULE\NUL\NUL\243\226.\NUL\NUL;\DC1\186I\192\166\STXx\233%6>\141\206<\156\NUL\223uDB6034KR4301F3252100840000642600139000000400138000000000137000001800136000000100135000000000054730014100001300014200000080014300000910014400000130014500000200024300030000000500010000003480001000300050007000609000088\255"

nonQuotePacketInput :: BS.ByteString
nonQuotePacketInput = "\128\DC3[M{J\SO\NUL@\SOH\NUL\NUL@\SOH\NUL\NUL\SOH\NUL^%6G\NUL\DC2D\200\&8\n\b\NULE\NUL\SOH2\147l\NUL\NUL;\DC1\t\198\192\164\SOHx\233%6G\141\254<\212\SOH\RS\204\249G7014KR4101F3000801 2650000000140090000860000000000 26495 26500 26495 2649500006680008849332540014083 26490000003 26485000000 26480000039 26475000015 26470000002010934 26500000050 26505000019 26510000025 26515000051 2652000002001948000200000003000100010129000240006000700090006\255"

globalHeaderInput :: BS.ByteString 
globalHeaderInput = "\212\195\178\161\STX\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\255\255\NUL\NUL\SOH\NUL\NUL\NUL\128\DC3[M\212\CAN\NUL\NUL\254\NUL\NUL\NUL\254\NUL\NUL\NUL\SOH\NUL^%6G\NUL\DC2D\200\&8\n\b\NULE\NUL\NUL\240\147W\NUL\NUL;\DC1\n\GS\192\164\SOHx\233%6G\141\254<\212\NUL\220\200TB6014KR4101F300080110014747 00000000000 00000000000 00000000000 00000000000 00000000000011601 00000000000 00000000000 00000000000 00000000000 000000000000208000000000000000000000014880000000000000000000008595998\255" 
