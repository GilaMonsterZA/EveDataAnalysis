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
RegionPricesAsOf <- function(Region, TypeID, Dates) { # Dates as a list of strings in the format '2016-04-02T00:00:00', Example region = 10000002 (The Forge), Example TypeID = 34 (Trit)
  require(jsonlite)
  priceHistory_addr <- paste("https://crest-tq.eveonline.com/market/",Region,"/history/?type=https://crest-tq.eveonline.com/inventory/types/",TypeID,"/", sep= "") 
  suppressWarnings(market.json <- fromJSON(readLines(priceHistory_addr))) # throws a warning about incomplete final line
  MarketValues <- market.json$items
  MarketPrices <- numeric(length(Dates))
  for (i in seq_along(Dates)) {
    MarketPrices[i] <- as.numeric(MarketValues[MarketValues$date==Dates[[i]],5])
  }
  MarketPrices
}

getItemPrices <- function (Region, TypeIDs, Dates) { #to finish
  require(jsonlite)
  
  ItemPrices <- data.frame(matrix(0.0, ncol = length(Dates), nrow = length(TypeIDs)))
  colnames(ItemPrices) <- Dates
  
  for (i in seq_along(TypeIDs)) {
    ItemPrices[i,] <- RegionPricesAsOf(Region, TypeIDs[[i]], Dates)
  }
  cbind(TypeIDs, ItemPrices)
}
