calcSECRdensEstimate <- function(captureData, yearOI){
  
  library(secr)
  
  #check that boutID has already been assigned to captureData - if not - assign it
  if (!'boutID' %in% names(captureData)){
    #function to group trapping nights by bout - bout IDs reflect the date of the nearest new moon
    source(paste(mypathtoTWGrepo, '/functions/assignBoutIDs.R', sep = ''))
    captureData <- assignBoutIDs(captureData)
  }
  
  densResults <- NULL
  for (i in unique(captureData$siteID)){
    cd <- captureData %>% filter(siteID == i & year == yearOI & 
                                   taxonProtocolCategory == 'target' & taxonRank == 'species')
    #secr needs captureData in format: bout | tagID | night | trapCoordinate
    #secr needs trap relative location data in format: trapCoordinate | x | y in meters
    if (nrow(cd) > 0){
      #first calculate estimates and observed by site
      commMatrix <- cd %>% group_by(taxonID) %>% summarize(nInds = n_distinct(tagID))
      comm <- commMatrix[,2:ncol(commMatrix)]
      if (sum(comm) > 10){
  
  
}