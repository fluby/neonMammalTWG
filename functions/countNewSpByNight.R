countNewSpByNight <- function(spByPlot){

  results <- data_frame(boutID = as.character(), plotID = as.character(), 
                        night = as.integer(), newTaxa = as.integer())
  spByPlot$boutplotID <- paste(spByPlot$boutID, spByPlot$plotID, sep = '_')
  for (i in unique(spByPlot$boutplotID)){
      nights <- spByPlot %>% filter(boutplotID == i) %>% summarize(nights = n_distinct(collectDate))
      if (nights$nights > 1){
        dat <- spByPlot %>% filter(boutplotID == i)
        trapNights <- sort(unique(dat$collectDate))
        for (k in 2:nights$nights){
          if (k == 2){
            taxaNk_1 <- dat$taxonID[dat$collectDate == trapNights[k-1]]
            taxaNk <- dat$taxonID[dat$collectDate == trapNights[k]]
            newSp <- length(taxaNk[!taxaNk %in% taxaNk_1])
            temp1 <- cbind(unique(dat$boutID), unique(dat$plotID), k, newSp)
            results <- rbind(results, temp1)
          } else if (k ==3){
            taxaNk_1 <- unique(dat$taxonID[dat$collectDate == trapNights[1] | dat$collectDate == trapNights[2]])
            taxaNk <- dat$taxonID[dat$collectDate == trapNights[k]]
            newSp <- length(taxaNk[!taxaNk %in% taxaNk_1])
            temp2 <- cbind(unique(dat$boutID), unique(dat$plotID), k, newSp)
            results <- rbind(results, temp2)
          }
        }
      }
  }
  
  names(results) <- c('boutID', 'plotID', 'night', 'newTaxa')
  results$newTaxa <- as.integer(results$newTaxa)
  results$siteID <- substr(results$plotID, 1, 4)
  
  return(results)
}