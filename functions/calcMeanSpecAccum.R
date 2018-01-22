calcMeanSpecAccum <- function(captureData, siteOI, dataset){
  library(vegan)
  cd <- captureData %>% filter(siteID == siteOI & taxonProtocolCategory == 'target' & taxonRank == 'species')
  indsperplot <- cd %>% group_by(year, plotID, taxonID) %>% summarize(nInds = n_distinct(tagID))
  spaResults <- NULL
  for (j in unique(indsperplot$year)){
    commMatrix <- indsperplot %>% filter(year == j) %>% spread(taxonID, nInds, fill = 0) 
    comm <- commMatrix[,3:ncol(commMatrix)]
    if (sum(comm) > 10 & nrow(comm) > 1 & ncol(comm) > 1){
      spa <- specaccum(comm, method = 'random')
      spa2 <- data_frame(plots = spa$sites, richness = spa$richness)
      spa2$year <- rep(j, nrow(spa2))
      spaResults <- rbind(spaResults, spa2)
    }
  }
  if (!empty(spaResults)){
    spaMean <- spaResults %>% group_by(plots) %>% summarise(meanS = mean(richness))
    spaMean$dataset <- rep(dataset, nrow(spaMean))
    spaMean$siteID <- rep(siteOI, nrow(spaMean))
    return(spaMean)
  }
}