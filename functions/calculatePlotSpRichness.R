calculatePlotSpRichness <- function(captureData, yearOI, dataset){
  
  library(SpadeR)
  for (i in unique(captureData$siteID)){
    cd <- captureData %>% filter(siteID == i & year == yearOI, 
                                 taxonProtocolCategory == 'target' & taxonRank == 'species')
    
    #first calculate estimates and observed by plot
    indsperplot <- cd %>% group_by(plotID, taxonID) %>% summarize(nInds = n_distinct(tagID))
    richResults <- NULL
    commMatrix <-indsperplot %>% spread(plotID, nInds, fill = 0)
    commMatrix <-  data.frame(commMatrix)
    comm <- commMatrix[,3:ncol(commMatrix)]
    if (sum(comm) > 10 & nrow(comm) > 1 & ncol(comm) > 1){
      sPerPlot <- indsperplot %>% group_by(plotID) %>% summarise(value = n_distinct(taxonID))
      sPerPlot$key <- rep('observedS', nrow(sPerPlot))
      richResults <- rbind(richResults, sPerPlot)
      for (i in 1:ncol(comm)){
        estimateS <- ChaoSpecies(comm[,i], "abundance", k=10, conf = 0.95)
        temp <- data.frame(estimateS$Species_table)
        results <- temp[which(row.names(temp) == "    iChao1 (Chiu et al. 2014)"),]
        results <- gather(results)
        results$plotID <- rep(names(comm)[i], nrow(results))
        richResults <- rbind(richResults, results)
      }
    }
  }
  # calculate means for each site
  if (!empty(richResults)){
    #richResults$siteID <- substr(richResults$plotID, 1, 4)
    #richMean <- richResults %>% group_by(siteID) %>% summarise(meanS = mean(richness))
    #spaMean$dataset <- rep(dataset, nrow(spaMean))
    
    return(richResults)
  }
}
}