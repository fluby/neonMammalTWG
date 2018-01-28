#functions taken from small mammal data quality test as part of NEON commissioning and then modified for multi-site efficiency
#number of captured individuals grouped by the specified grouping variables (e.g., siteID)
numCaptures <- function(captureData, groupingVariables){
  dat <- captureData %>% filter(!is.na(tagID) & tagID != '' & taxonProtocolCategory == 'target')
  datWithoutTagID <- captureData %>% filter((is.na(tagID) | tagID == '') & taxonProtocolCategory == 'target')
  resultsWithTagID <- ddply(dat, c(groupingVariables), summarize, n_distinct(tagID))
  resultsWithoutTagID <- ddply(datWithoutTagID, c(groupingVariables), nrow)
  if (nrow(resultsWithoutTagID) > 0){
    results <- full_join(resultsWithTagID, resultsWithoutTagID, by = groupingVariables)
    results <- results %>% mutate(totalN = rowSums(results[,c("n_distinct(tagID)", "V1")], na.rm = T)) %>% select(groupingVariables,totalN)
  } else {
    results <- resultsWithTagID %>% rename(totalN = `n_distinct(tagID)`) %>% select(groupingVariables,totalN)
  }
  return(results)
}

#tagIDs for each individual with at least one recapture at a given site
recapturedInds <- function(captureData, groupingVariables){
  dat <- captureData %>% filter(!is.na(tagID) & tagID != '' & taxonProtocolCategory == 'target') %>% mutate(tagYr = paste(tagID, year, sep = '_'))
  idsToExclude <- NULL
  for (i in unique(dat$tagID)){
      ufates <- unique(dat$fate[dat$tagID == i])
      if (length(ufates) == 1){
        if (!is.na(ufates) & ufates == 'dead'){
          idsToExclude <- c(idsToExclude, i)
        }}}
  dat2 <- dat %>% filter(!(tagID %in% idsToExclude))
  dat3 <- ddply(dat2, c(groupingVariables, 'tagYr'), count)
  dat4 <- dat %>% filter(tagYr %in% dat3$tagYr[dat3$n>1]) %>% select(groupingVariables, tagID)
  return(dat4)
}
