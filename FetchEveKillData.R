#regionID 
# 10000032 - Sing Laison, 10000064 = Essence, 10000037 = Everyshore
# 10000043 - Domain
# 10000002 - The Forge

loadKill <- function(region) {
  library("RCurl")
  library("XML")
  for (page in (1:10)) {
    url <- paste("https://zkillboard.com/api/losses/regionID/", region, "/page/", page, "/pastSeconds/86400/xml/", sep= "")
    doc <- getURL(url)
    doc <- xmlParse(doc)
    if (length(as.character(sapply(doc["//error"], xmlValue))) > 0) 
      break
    
    if (page==1)
      kills <- getNodeSet(doc, '/eveapi/result/rowset/row')
    else
      kills <- c(kills, getNodeSet(doc, '/eveapi/result/rowset/row'))
  }
  kills
}

extractItemsTotals <- function(killList) {
  rawItems <- data.frame(numeric(0), numeric(0), numeric(0))
  for (i in killList) { 
    ShipType<-as.numeric(xmlDoc(i)[["//victim/@shipTypeID"]][1]) # ship they were flying
    rawItems<- rbind(rawItems, c(ShipType,0,1))
    colnames(rawItems) <- c("V1","V2","V3")
    if (length(xpathSApply(xmlDoc(i), "/row/rowset[@name='items']")) > 0)  #If there are some dropped items
      rawItems<- rbind(rawItems, as.data.frame(cbind(  #Modules, rigs and cargo
        as.vector(xpathSApply(xmlDoc(i), "/row/rowset[@name='items']/row/@typeID"), mode = "numeric"), 
        as.vector(xpathSApply(xmlDoc(i), "/row/rowset[@name='items']/row/@qtyDropped"), mode = "numeric"), 
        as.vector(xpathSApply(xmlDoc(i), "/row/rowset[@name='items']/row/@qtyDestroyed"), mode = "numeric")
      )))
  }
  colnames(rawItems) <- c("TypeID", "QuantityDropped", "QuantityDestroyed")
  totals<-aggregate(rawItems$QuantityDropped+rawItems$QuantityDestroyed, by=list(rawItems$TypeID), FUN=sum)
  colnames(totals) <- c("TypeID", "TotalQuantity")
  totals

}