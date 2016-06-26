#http://api.eve-central.com/api/marketstat?typeid=34,35&usesystem=30002659
loadMarketPrices <- function(items, useSystem) {
  url <- paste("http://api.eve-central.com/api/marketstat?typeid=", items, "&usesystem=", useSystem, sep= "")
  doc <- getURL(url)
  doc <- xmlParse(doc)
  types <- as.vector(xpathSApply(doc, "//*/type/@id"))
  buyVolume <- as.vector(xpathSApply(doc, "//type/buy/volume", xmlValue))
  sellVolume <- as.vector(xpathSApply(doc, "//type/sell/volume", xmlValue))
  buyPrice <- as.vector(xpathSApply(doc, "//type/buy/max", xmlValue))
  sellPrice <- as.vector(xpathSApply(doc, "//type/sell/min", xmlValue))
  marketData <- cbind.data.frame(types, buyPrice, buyVolume, sellPrice, sellVolume)
  marketData
}

# Historical Market data 
ForgePriceAsOf <- function(TypeID, Date) { # Date as a string in the format '2015-04-02T00:00:00'
  require(jsonlite)
  priceHistory_addr <- paste("https://crest-tq.eveonline.com/market/10000002/types/", TypeID, "/history/", sep= "") #Fix region number
  market.json <- fromJSON(readLines(priceHistory_addr))
  MarketValues <- market.json$items
  as.integer(MarketValues[MarketValues$date==Date,5])
}

