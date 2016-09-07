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
  suppressWarnings(market.json <- try(fromJSON(readLines(priceHistory_addr)), silent=TRUE)) # throws a warning about incomplete final line
  if (class(market.json) == "try-error") {
    MarketPrices <- numeric(length(Dates))
  }
  else {
    MarketValues <- market.json$items
    MarketPrices <- numeric(length(Dates))
    for (i in seq_along(Dates)) {
      price <- as.numeric(MarketValues[MarketValues$date==Dates[[i]],5])
      if (length(price) == 0) {
        price <- 0
      }
      MarketPrices[i] <- price
    }
  }
  MarketPrices
}

getItemPrices <- function (Region, TypeIDs, Dates) { 

  ItemPrices <- data.frame(matrix(0.0, ncol = length(Dates), nrow = length(TypeIDs)))
  colnames(ItemPrices) <- Dates
  
  for (i in seq_along(TypeIDs)) {
    ItemPrices[i,] <- RegionPricesAsOf(Region, TypeIDs[[i]], Dates)
  }
  cbind(TypeIDs, ItemPrices)
}
