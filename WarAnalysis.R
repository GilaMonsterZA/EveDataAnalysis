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
  
  killStats <- data.frame(numeric(0), string(0), numeric(0), numeric(0), numeric(0))
  colnames(killStats) <- c("V1","V2","V3", "V4", "V5")
  
  for (i in KillList) {
    if (length(xpathSApply(xmlDoc(i), "/row/rowset[@name='attackers']/row[@characterName!='']/@characterID")) > 0) {# not a NPC kill
      killStats <- rbind(killStats,
            cbind(1,
              substr(xpathSApply(xmlDoc(i), "/row/@killTime"),1,10),
              xpathSApply(xmlDoc(i), "/row/rowset[@name='attackers']/row[1]/@shipTypeID"), 
              xpathSApply(xmlDoc(i), "/row/rowset[@name='attackers']/row[1]/@weaponTypeID"), 
              xpathSApply(xmlDoc(i), "/row/victim/@shipTypeID") 
            )
      )
      colnames(killStats) <- c("V1","V2","V3", "V4", "V5")
    }
  }
}
  
extractKillItems <- function(KillList)  

  cbind(1,
        substr(xpathSApply(xmlDoc(test), "/row/@killTime"),1,10),
        xpathSApply(xmlDoc(test), "/row/rowset[@name='attackers']/row[1]/@shipTypeID"), 
        xpathSApply(xmlDoc(test), "/row/rowset[@name='attackers']/row[1]/@weaponTypeID"), 
        xpathSApply(xmlDoc(test), "/row/victim/@shipTypeID") 
  )
  
  cbind(1,
        as.vector(xpathSApply(xmlDoc(killsVale[[1]]), "/row/rowset[@name='items']/row/@typeID"), mode = "numeric"),
        as.vector(xpathSApply(xmlDoc(killsVale[[1]]), "/row/rowset[@name='items']/row/@flag"), mode = "numeric"),
        as.vector(xpathSApply(xmlDoc(killsVale[[1]]), "/row/rowset[@name='items']/row/@qtyDropped"), mode = "numeric"),
        as.vector(xpathSApply(xmlDoc(killsVale[[1]]), "/row/rowset[@name='items']/row/@qtyDestroyed"), mode = "numeric")
  )
  
}

test <- getURL("https://zkillboard.com/api/kills/killID/53617156/xml/")
test <- xmlParse(doc)
