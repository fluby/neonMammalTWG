subsetDataToFewerNights <- function(captureData, n){

  nightByPlot <- captureData %>% distinct(boutID, collectDate, plotID)
  nightByPlot$boutplotID <- paste(nightByPlot$boutID, nightByPlot$plotID, sep = '_')
  
  pathogenGrids1night <- captureData[1,]
  pathogenGrids2night <- captureData[1,]
  diversityGrids <- captureData[1,]
  for (i in unique(nightByPlot$boutplotID)){
    nights <- nightByPlot %>% filter(boutplotID == i) %>% summarize(nights = 
                                                                      n_distinct(collectDate))
    dat <- nightByPlot %>% filter(boutplotID == i)
    if (nights$nights > 1){
      trapNights <- sort(unique(dat$collectDate))
      if (1 %in% n){
        temp1 <- captureData %>% filter(boutID == unique(dat$boutID) & plotID == unique(dat$plotID) & 
                                          collectDate == trapNights[1])
        pathogenGrids1night <- rbind(pathogenGrids1night, temp1)
      }
      if (2 %in% n){
        temp2 <- captureData %>% filter(boutID == unique(dat$boutID) & plotID == unique(dat$plotID) & 
                                          (collectDate == trapNights[1] | 
                                             collectDate == trapNights[2]))
        pathogenGrids2night <- rbind(pathogenGrids2night, temp2)
      } 
    } else {
      temp3 <- captureData %>% filter(boutID == unique(dat$boutID) & plotID == unique(dat$plotID))
      diversityGrids <- rbind(diversityGrids, temp3)
    }
  }

  pathogenGrids1night <- pathogenGrids1night[-1,]
  diversityGrids <- diversityGrids[-1,]
  pathogenGrids2night <- pathogenGrids2night[-1,]
  
  if (nrow(pathogenGrids1night) > 0){
    captureDataOneNight <- rbind(pathogenGrids1night, diversityGrids)
  }
  if (nrow(pathogenGrids2night) > 0){
    captureDataTwoNight <- rbind(pathogenGrids2night, diversityGrids)
  }
  
  if (exists('captureDataOneNight') & exists('captureDataTwoNight')){
    results <- list(captureDataOneNight, captureDataTwoNight)
  } else if (exists('captureDataOneNight') & !exists('captureDataTwoNight')){
    results <- captureDataOneNight
  } else if (!exists('captureDataOneNight') & exists('captureDataTwoNight')){
    results <- captureDataTwoNight
  }
  
  return(results)
  
}