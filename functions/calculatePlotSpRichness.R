calculatePlotSpRichness <- function(captureData, yearOI){
  
  library(SpadeR)
  
  richResults <- NULL
  for (i in unique(captureData$siteID)){
    cd <- captureData %>% filter(siteID == i & year == yearOI, 
                                 taxonProtocolCategory == 'target' & taxonRank == 'species')
    if (nrow(cd) > 0){
      #first calculate estimates and observed by plot
      indsperplot <- cd %>% group_by(plotID, taxonID) %>% summarize(nInds = n_distinct(tagID))
      commMatrix <-indsperplot %>% spread(plotID, nInds, fill = 0)
      commMatrix <-  data.frame(commMatrix)
      comm <- commMatrix[,2:ncol(commMatrix)]
      if (sum(comm) > 10 & nrow(comm) > 1 & ncol(comm) > 1){
        sPerPlot <- indsperplot %>% group_by(plotID) %>% summarise(value = n_distinct(taxonID))
        sPerPlot$key <- rep('observedS', nrow(sPerPlot))
        richResults <- rbind(richResults, sPerPlot)
        for (j in 1:ncol(comm)){
          tryCatch(
            {
            estimateS <- ChaoSpecies(comm[,j], "abundance", k=10, conf = 0.95)
            temp <- data.frame(estimateS$Species_table)
            results <- temp[which(row.names(temp) == "    iChao1 (Chiu et al. 2014)"),]
            results <- gather(results)
            results$plotID <- rep(names(comm)[j], nrow(results))
            richResults <- rbind(richResults, results)
            }, error =function(cond){
              message(cond)
              return(NA)
            }
          )
        }
      }
    }
  }

  # calculate means for each site
  if (!empty(richResults)){
    richResults$siteID <- substr(richResults$plotID, 1, 4)
    #richMean <- richResults %>% group_by(siteID) %>% summarise(meanS = mean(richness))
    #spaMean$dataset <- rep(dataset, nrow(spaMean))
    return(richResults)
  }
}

# library(SpadeR) - from user guide:
# #Type (1) abundance data (datatype = "abundance"): Input data consist of species (in rows) by community (in columns) matrix. The entries of each row are the observed abundances of a species in N communities.
# data(ChaoSpeciesData)
# results <- ChaoSpecies(ChaoSpeciesData$Abu,"abundance", k=10, conf = 0.95)
# temp <- data.frame(results$Species_table)
# temp$Estimate[which(row.names(temp) == "    iChao1 (Chiu et al. 2014)")]