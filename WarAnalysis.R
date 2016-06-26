loadKillWar <- function(region, start, end) {
     require("RCurl")
     require("XML")
     for (page in (1:10)) {
         url <- paste("https://zkillboard.com/api/losses/regionID/", region,"/page/", page, "/startTime/",start,"/endTime/",end,"/xml/", sep= "")
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

extractKillSummary <- function(KillList) {
  
  killStats <- data.frame(numeric(0), character(0), numeric(0), numeric(0), numeric(0))
  colnames(killStats) <- c("V1","V2","V3", "V4", "V5")
  
  for (i in KillList) {
    if (length(xpathSApply(xmlDoc(i), "/row/rowset[@name='attackers']/row[@characterName!='']/@characterID")) > 0) {# not a NPC kill
      killStats <- rbind(killStats,
            cbind(xpathSApply(xmlDoc(i), "/row/@killID"),
              substr(xpathSApply(xmlDoc(i), "/row/@killTime"),1,10),
              xpathSApply(xmlDoc(i), "/row/rowset[@name='attackers']/row[1]/@shipTypeID"), 
              xpathSApply(xmlDoc(i), "/row/rowset[@name='attackers']/row[1]/@weaponTypeID"), 
              xpathSApply(xmlDoc(i), "/row/victim/@shipTypeID") 
            )
      )
      colnames(killStats) <- c("V1","V2","V3", "V4", "V5")
    }
  }
  colnames(killStats) <- c("KillID", "Time", "HighestDamageShip", "HighestDamageweapon", "VictimShip")
  killStats
}
  
extractKillItems <- function(KillList)  {
  counter<-0
  killItemStats <- data.frame(numeric(0), numeric(0), numeric(0), numeric(0), numeric(0))
  colnames(killItemStats) <- c("V1","V2","V3", "V4", "V5")
  
  for (i in KillList) {
    counter<-counter+1
    if (length(xpathSApply(xmlDoc(i), "/row/rowset[@name='attackers']/row[@characterName!='']/@characterID")) > 0) {# not a NPC kill
      if (length(xpathSApply(xmlDoc(i), "/row/rowset[@name='items']")) > 0)  {
  
        killItemStats <- rbind(killItemStats,
          cbind(xpathSApply(xmlDoc(i), "/row/@killID"),
                as.vector(xpathSApply(xmlDoc(i), "/row/rowset[@name='items']/row/@typeID"), mode = "numeric"),
                as.vector(xpathSApply(xmlDoc(i), "/row/rowset[@name='items']/row/@flag"), mode = "numeric"),
                as.vector(xpathSApply(xmlDoc(i), "/row/rowset[@name='items']/row/@qtyDropped"), mode = "numeric"),
                as.vector(xpathSApply(xmlDoc(i), "/row/rowset[@name='items']/row/@qtyDestroyed"), mode = "numeric")
          )
        )
      }
    }
    colnames(killItemStats) <- c("V1","V2","V3", "V4", "V5")
  }
  colnames(killItemStats) <- c("KillID", "TypeID", "Flag", "Dropped", "Destroyed")
  killItemStats
}

summariseKilledShips <- function(ShipSummary) {
  victimShipTotal <- aggregate(ShipSummary$KillID>0, by=list(ShipSummary$VictimShip), FUN=sum)
  colnames(victimShipTotal) <- c("TypeID", "Total")
  victimShipTotal <- victimShipTotal[with(victimShipTotal, order(-Total)), ]
  victimShipTotal
}

summariseAttackerShips <- function(ShipSummary) {
  attackerShipTotal <- aggregate(ShipSummary$KillID>0, by=list(ShipSummary$HighestDamageShip), FUN=sum)
  colnames(attackerShipTotal) <- c("TypeID", "Total")
  attackerShipTotal <- attackerShipTotal[with(attackerShipTotal, order(-Total)), ]
  attackerShipTotal
}

summariseAttackerWeapons <- function(ShipSummary) {
  attackerWeaponTotal <- aggregate(ShipSummary$KillID>0, by=list(ShipSummary$HighestDamageweapon), FUN=sum)
  colnames(attackerWeaponTotal) <- c("TypeID", "Total")
  attackerWeaponTotal <- attackerWeaponTotal[with(attackerWeaponTotal, order(-Total)), ]
  attackerWeaponTotal
}

summariseItemDrops <- function(Items) {
  VictimItems <- merge(
                  aggregate(Items$Dropped, by=list(Items$TypeID), FUN=sum), 
                  aggregate(Items$Destroyed, by=list(Items$TypeID), FUN=sum), 
                    by="Group.1", all = TRUE
                 )
  colnames(VictimItems) <- c("TypeID", "TotalDropped", "TotalDestroyed")
  VictimItems$Total <- VictimItems$TotalDropped + VictimItems$TotalDestroyed
  VictimItems <- VictimItems[with(VictimItems, order(-Total)), ]
  VictimItems
}

getItemPrices <- function (ItemList, KillDate) { #to finish
  require(jsonlite)
  ItemPrices <- data.frame(TypeID = numeric(length(ItemList)), JitaPrice = numeric(length(ItemList)), stringsAsFactors = FALSE)
  for (i in seq_along(ItemList)) {
    ItemPrices$TypeID[i] <- ItemList[[i]]
    ItemPrices$JitaPrice[i] <- ForgePriceAsOf(ItemList[[i]], KillDate)
  }
  ItemPrices
}
