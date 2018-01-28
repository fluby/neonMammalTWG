#functions taken from small mammal data quality test as part of NEON commissioning and then modified for multi-site efficiency
#number of captured individuals grouped by the specified grouping variables (e.g., siteID)
numCaptures <- function(captureData, groupingVariables){
  dat <- captureData %>% filter(!is.na(tagID) & tagID != '' & taxonProtocolCategory == 'target')
  datWithoutTagID <- captureData %>% filter((is.na(tagID) | tagID == '') & taxonProtocolCategory == 'target')
  resultsWithTagID <- ddply(dat, c(groupingVariables), summarize, n_distinct(tagID))
  resultsWithoutTagID <- ddply(datWithoutTagID, c(groupingVariables), nrow)
  if (nrow(resultsWithoutTagID) > 0){
    results <- merge(resultsWithTagID, resultsWithoutTagID, by = groupingVariables)
    results <- results %>% mutate(totalN = `n_distinct(tagID)` + V1) %>% select(groupingVariables,totalN)
  } else {
    results <- resultsWithTagID %>% rename(totalN = `n_distinct(tagID)`) %>% select(groupingVariables,totalN)
  }
  return(results)
}

#tagIDs for each individual with at least one recapture at a given site
recapturedInds <- function(captureData, groupingVariables){
  dat <- captureData %>% filter(!is.na(tagID) & tagID != '' & taxonProtocolCategory == 'target')
  idsToExclude <- NULL
  for (i in unique(dat$tagID)){
    if (grepl('^O', i) | grepl('^0', i)){
      ufates <- unique(dat$fate[dat$tagID == i])
      if (length(ufates) == 1){
        if (ufates == 'dead'){
          idsToExclude <- c(idsToExclude, i)
        }}}}
  dat2 <- dat %>% filter(!(tagID %in% idsToExclude)) %>% count(tagID)
  return(dat %>% filter(tagID %in% dat2$tagID[dat2$n>1]) %>% select(groupingVariables, tagID))
}
