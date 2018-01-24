#number of captured individuals per site per year
numCaptures <- function(trappingData, siteOI){
  dat <- trappingData %>% filter(siteID == siteOI & trapStatus > 1 & !is.na(tagID) & tagID != '')
  datWithoutTagID <- trappingData %>% filter(siteID == siteOI & (trapStatus == 4 | trapStatus == 5) & (is.na(tagID) | tagID == ''))
  resultsWithTagID <- dat %>% group_by(year) %>% summarise(n= length(unique(tagID)))
  resultsWithoutTagID <- datWithoutTagID %>% group_by(year) %>% summarise(nWithoutTagID = length(tagID))
  if (nrow(resultsWithoutTagID) > 0){
    results <- merge(resultsWithTagID, resultsWithoutTagID, by = 'year')
    results <- results %>% mutate(totalN = n + nWithoutTagID) %>% select(year, totalN)
  } else {
    results <- resultsWithTagID %>% rename(totalN = n) %>% select(year, totalN)
  }
  return(sum(results$totalN))
}

#tagIDs and number of capture instances for each individual with at least one recapture at a given site
recapturedIndividuals <- function(trappingData, siteOI){
  dat <- trappingData %>% filter(siteID == siteOI & trapStatus > 1 & !is.na(tagID) & tagID != '')
  idsToExclude <- NULL
  for (i in unique(dat$tagID)){
    if (grepl('^O', i) | grepl('^0', i)){
      ufates <- unique(dat$fate[dat$tagID == i])
      if (length(ufates) == 1){
        if (ufates == 'dead'){
          idsToExclude <- c(idsToExclude, i)
        }}}}
  dat2 <- dat %>% filter(!(tagID %in% idsToExclude)) %>% count(tagID, date)
  dat3 <- dat2 %>% group_by(tagID) %>% summarise(captures = sum(n))
  return(dat3 %>% filter(captures > 1))
}

recapturedIndividualsBySpp <- function(trappingData, siteOI){
  dat <- trappingData %>% filter(siteID == siteOI & trapStatus > 1 & !is.na(tagID) & tagID != '')
  idsToExclude <- NULL
  for (i in unique(dat$tagID)){
    if (grepl('^O', i) | grepl('^0', i)){
      ufates <- unique(dat$fate[dat$tagID == i])
      if (length(ufates) == 1){
        if (ufates == 'dead'){
          idsToExclude <- c(idsToExclude, i)
        }}}}
  dat2 <- dat %>% filter(!(tagID %in% idsToExclude)) %>% group_by(taxonID) %>% count(tagID, date)
  dat3 <- dat2 %>% group_by(taxonID, tagID) %>% summarise(captures = sum(n))
  return(dat3 %>% filter(captures > 1))
}

#number of individuals at a given sites with at least one recapture instance
numberOfRecapturedIndividualsBySpp <- function(trappingData, siteOI){
  recaps <- recapturedIndividualsBySpp(trappingData, siteOI)
  recaps2 <- recaps %>% group_by(taxonID) %>% summarise(length(captures))
  return(recaps2)
}

numberOfRecapturedIndividuals <- function(trappingData, siteOI){
  recaps <- recapturedIndividuals(trappingData, siteOI)
  return(nrow(recaps))
}

#return tagIDs of recaptured individuals
recapturedIndividualIDs <- function(trappingData, siteOI){
  recapIDs <- recapturedIndividuals(trappingData, siteOI)[,1]
  return (recapIDs)
}

#return tagIDs of individuals with inconsistency of classification to genus, species, or sex
inconsistentClassifications <- function(trappingData, siteOI, categoryOI){ 
  recapIDs <- recapturedIndividuals(trappingData, siteOI)
  dat <- trappingData %>% filter(siteID == siteOI & tagID %in% recapIDs$tagID)
  if (categoryOI == 'genus'){
    temp <- dat %>% filter(tagID != '') %>% group_by(tagID) %>% summarise(genusIDs = length(unique(genus)))
    inconsistentIDs <- temp$tagID[which(temp$genusIDs > 1)]
  } else if (categoryOI == 'species'){
    dat2 <- dat %>% filter (taxonRank == 'species')
    temp <- dat2 %>% group_by(tagID) %>% summarise(speciesIDs = length(unique(scientificName)))
    inconsistentIDs <- temp$tagID[which(temp$speciesIDs > 1)]
  } else if (categoryOI == 'sex'){
    dat2 <- dat %>% filter(sex != 'U' & sex != '' & lifeStage != 'juvenile' & lifeStage != 'subadult')
    temp <- dat2 %>% group_by(tagID) %>% summarise(sexIDs = length(unique(sex)))
    inconsistentIDs <- temp$tagID[which(temp$sexIDs > 1)]
  }
  return(inconsistentIDs)
}  

#return records involving tagIDs with inconsistent classifications
inconsistentData <- function(trappingData, siteOI, categoryOI){ 
  inconsistentIDs <- inconsistentClassifications(trappingData, siteOI, categoryOI)
  dat <- trappingData %>% filter(siteID == siteOI & tagID %in% inconsistentIDs)
  dat <- dat %>% select(plotID, date, trapCoordinate, tagID, taxonID, identificationQualifier, sex, lifeStage, weight, fate)
  return(dat)
}

#return records involving tagIDs with inconsistent classifications
inconsistentInstances <- function(trappingData, siteOI, categoryOI){ 
  inconsistentIDs <- inconsistentClassifications(trappingData, siteOI, categoryOI)
  dat <- trappingData %>% filter(siteID == siteOI & tagID %in% inconsistentIDs)
  dat <- dat %>% select(plotID, date, trapCoordinate, tagID, genus, scientificName, taxonID, identificationQualifier, sex, lifeStage, weight, fate, taxonRank)
  if (categoryOI == 'genus'){
    temp <- dat %>% filter(tagID != '') %>% group_by(tagID, genus) %>% summarise(genusIDs = length(date))
  } else if (categoryOI == 'species'){
    temp <- dat %>% filter(tagID != '' & taxonRank == 'species') %>% group_by(tagID, scientificName) %>% summarise(speciesIDs = length(date))
  } else if (categoryOI == 'sex'){
    temp <- dat %>% filter(tagID != '' & sex != "U") %>% group_by(taxonID, tagID, sex) %>% summarise(sexIDs = length(date))
    return(temp)
  }
}
