getL1data <- function(dpID, sitesOI){
  
  library(httr)
  library(jsonlite)
  
  dp <- paste("http://data.neonscience.org/api/v0/products/", dpID, sep = '')
  request <- GET(dp)
  #httr package function to extract info from the json:
  request.content <- content(request, as = "parsed")
  request.text <- content(request, as = "text")
  
  avail <- fromJSON(request.text, simplifyDataFrame = T, flatten = T)
  avail
  
  dat.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  mam.perplotnight <- NA
  mam.pertrapnight <- NA
  for (j in sitesOI){
    site.urls <- dat.urls[grepl(j, dat.urls)]
    for (k in site.urls){
      dat.sitemo <- GET(k)
      dat.files <- fromJSON(content(dat.sitemo, as='text'))
      mam.plotnight <- read.delim(dat.files$data$files$url
                                  [intersect(grep("plotnight", dat.files$data$files$name), 
                                             grep("expanded", dat.files$data$files$name))], 
                                  sep = ",")
      mam.trapnight <- read.delim(dat.files$data$files$url
                                  [intersect(grep("trapnight", dat.files$data$files$name), 
                                             grep("expanded", dat.files$data$files$name))], 
                                  sep = ",")
      mam.perplotnight <- rbind(mam.perplotnight, mam.plotnight)
      mam.pertrapnight <- rbind(mam.pertrapnight, mam.trapnight)
    }
  }
  
  rm(mam.plotnight, mam.trapnight)
  
  captureData <- mam.pertrapnight %>% filter(trapStatus == "4 - more than 1 capture in one trap" | trapStatus == "5 - capture")
  return(captureData)
}