calcSECRdensEstimate <- function(captureData, yearOI, mypathtoTWGrepo){
  
  library(secr)
  
  #check that boutID has already been assigned to captureData - if not - assign it
  if (!'boutID' %in% names(captureData)){
    #function to group trapping nights by bout - bout IDs reflect the date of the nearest new moon
    source(paste(mypathtoTWGrepo, '/functions/assignBoutIDs.R', sep = ''))
    captureData <- assignBoutIDs(captureData)
  }
  
  #secr needs trap relative location data in format: trapCoordinate | x | y in meters
  trapData <- read.csv(paste(mypathtoTWGrepo, '/dataFiles/trapCoordinates.csv', sep = ''), header = T)
  trapData$trap <- 1:nrow(trapData)
  trapsPerPlot <- read.traps(data = trapData, detector = "single")
  
  densResults <- NULL
  for (i in unique(captureData$siteID)){
    cd <- captureData %>% filter(siteID == i & year == yearOI & 
                                   taxonProtocolCategory == 'target' & taxonRank == 'species')
    #secr needs captureData in format: bout | tagID | night | trapCoordinate

    nightByPlot <- cd %>% distinct(boutID, collectDate, plotID)
    nightByPlot$boutplotID <- paste(nightByPlot$boutID, nightByPlot$plotID, sep = '_')
    if (nrow(cd) > 0){
      for (j in unique(nightByPlot$boutplotID)){
        nights <- nightByPlot %>% filter(boutplotID == j) 
        nights <- nights %>% distinct(plotID, collectDate)
        nights$night <- order(nights$collectDate)
        cdCapt <- cd %>% filter(boutID == nightByPlot$boutID[nightByPlot$boutplotID == j] & plotID == nightByPlot$plotID[nightByPlot$boutplotID == j]) %>% select(boutID, tagID, collectDate, trapCoordinate)
        cdCapt <- left_join(cdCapt, nights, by = 'collectDate')
        cdCapt <- left_join(cdCapt, trapData, by = 'trapCoordinate')
        cdCapt <- cdCapt %>% select(boutID, tagID, night, trap) %>% filter(tagID !='')
        names(cdCapt) <- c("session", "ID", "occasion", "trap")
        traps <- trapData %>% select(trap, x, y)
        temp <- make.capthist(cdCapt, trapsPerPlot)
        initialsigma <- RPSV(temp, CC = TRUE)
        fit <- secr.fit (temp, buffer = 4 * initialsigma, trace = FALSE)
        results <- predict(fit)
        results$plotID <- nightByPlot$plotID[nightByPlot$boutplotID == j]
        results$boutID <- nightByPlot$boutID[nightByPlot$boutplotID == j]
        densResults <- rbind(densResults, results)
      }
    }
  }  
}