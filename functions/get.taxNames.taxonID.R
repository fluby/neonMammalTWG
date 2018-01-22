get.taxNames.taxonID <- function(obj, mypathtoTWGrepo) {

  taxondata <- read.table(paste(mypathtoTWGrepo, '/dataFiles/mam_names_status_list.txt', sep =''), 
                        header = T, sep = '\t',
                        stringsAsFactors = FALSE, 
                        strip.white = TRUE, na.strings = '')

  obj$scientificName <- NA
  obj$commonName <- NA
  obj$genus <- NA
  obj$species <- NA
  obj$family <- NA
  obj$class <- NA
  obj$taxonProtocolCategory <- NA
  obj$taxonRank <- NA
  for (i in unique(obj$taxonID)) {
    if (i %in% taxondata$taxonID) {
      obj$scientificName[obj$taxonID == i] <- taxondata$scientificName[taxondata$acceptedTaxonID == i]
      obj$commonName[obj$taxonID == i] <- taxondata$vernacularName[taxondata$acceptedTaxonID == i]
      obj$genus[obj$taxonID == i] <- taxondata$genus[taxondata$acceptedTaxonID == i]
      obj$species[obj$taxonID == i] <- taxondata$specificEpithet[taxondata$acceptedTaxonID == i]
      obj$family[obj$taxonID == i] <- taxondata$family[taxondata$acceptedTaxonID == i]
      obj$class[obj$taxonID == i] <- taxondata$class[taxondata$acceptedTaxonID == i]
      obj$taxonProtocolCategory[obj$taxonID == i] <- taxondata$taxonProtocolCategory[taxondata$acceptedTaxonID == i]
      obj$taxonRank[obj$taxonID == i] <- taxondata$taxonRank[taxondata$acceptedTaxonID == i]
    }
  }
  
  return(obj)
  
}