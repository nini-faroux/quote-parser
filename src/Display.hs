module Display where 

import Types 

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
  "<Packet Details> \n" ++ 
  "PacketTime: " ++ show (packetTime quote) ++ ", \n" 
  ++ "AcceptTime: " ++ show (acceptTime quote) ++ ", \n" 
  ++ "IssueCode, ISIN: " ++ show (issueCode quote) ++ "\n\n" 
  ++ "<Best Five Bids, 5th to 1st> \n" 
  ++ showBids (bids quote) ++ "\n"
  ++ "<Best Five Asks, 1st to 5th> \n"
  ++ showAsks (asks quote) 
  ++ "<End Packet>" ++ "\n"

showIssueCode :: ISIN -> String 
showIssueCode issueCode =
  "CountryCode: " 
  ++ countryCode issueCode ++ ", " 
  ++ "NSIN: " ++ nsin issueCode ++ ", " 
  ++ "CheckDigit: " ++ show (checkDigit issueCode)

showBid :: Bid -> String 
showBid bid = 
  "Quantity: " ++ show (bidQuantity bid) ++ ", " 
  ++ "Price: " ++ show (bidPrice bid)

showAsk :: Ask -> String 
showAsk ask = 
  "Quantity: " ++ show (askQuantity ask) ++ ", " 
  ++ "Price: " ++ show (askPrice ask)

showBids :: [Bid] -> String 
showBids bs = go bs 5 
  where 
    go [] _ = "" 
    go (b:bs) n = "Bid " ++ show n ++ ": " ++ show b ++ "\n" ++ go bs (n-1)

showAsks :: [Ask] -> String 
showAsks as = go as 1 
  where 
    go [] _ = "" 
    go (a:as) n = "Ask " ++ show n ++ ": " ++ show a ++ "\n" ++ go as (n+1)

