calculateSiteSpRichness <- function(captureData, yearOI){
  
  library(SpadeR)
  
  richResults <- NULL
  for (i in unique(captureData$siteID)){
    cd <- captureData %>% filter(siteID == i & year == yearOI & 
                                 taxonProtocolCategory == 'target' & taxonRank == 'species')
    
    if (nrow(cd) > 0){
    #first calculate estimates and observed by site
      commMatrix <- cd %>% group_by(taxonID) %>% summarize(nInds = n_distinct(tagID))
      comm <- commMatrix[,2:ncol(commMatrix)]
      if (sum(comm) > 10){
        sPerSite <- commMatrix %>% summarise(value = n_distinct(taxonID))
        sPerSite$key <- rep('observedS', nrow(sPerSite))
        sPerSite$siteID = i
        richResults <- rbind(richResults, sPerSite)
        tryCatch(
            {
            estimateS <- ChaoSpecies(comm$nInds, "abundance", k=10, conf = 0.95)
            temp <- data.frame(estimateS$Species_table)
            results <- temp[which(row.names(temp) == "    iChao1 (Chiu et al. 2014)"),]
            results <- gather(results)
            results$siteID <- rep(i, nrow(results))
            richResults <- rbind(richResults, results)
            }, error =function(cond){
              message(cond)
              return(NA)
            }
        )
        }
      }
  }
  if (!empty(richResults)){
    return(richResults)
  }
}