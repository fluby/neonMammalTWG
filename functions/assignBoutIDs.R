assignBoutIDs <- function(captureData){
  library(jsonlite)
  library(stringr)
  
  #can only query 99 moon phases at a time, so need to do 3 separate queries to get data from 2013 - 2018
  #query 1
  urlMoon <- 'http://api.usno.navy.mil/moon/phase?date=1/3/2013&nump=99'
  
  moonPhases <- fromJSON(urlMoon)
  moonPhaseByDate <- moonPhases$phasedata
  moonPhaseByDate$year <- substr(moonPhaseByDate$date, 1,4)
  moonPhaseByDate$mo <- substr(moonPhaseByDate$date, 6,8)
  moonPhaseByDate$mo2 <- NULL
  for (i in 1:nrow(moonPhaseByDate)){
    if(moonPhaseByDate$mo[i] == "Jan"){
      moonPhaseByDate$mo2[i] <- '01'
    } else if(moonPhaseByDate$mo[i] == "Feb"){
      moonPhaseByDate$mo2[i] <- '02'
    } else if(moonPhaseByDate$mo[i] == "Mar"){
      moonPhaseByDate$mo2[i] <- '03'
    } else if(moonPhaseByDate$mo[i] == "Apr"){
      moonPhaseByDate$mo2[i] <- '04'
    } else if(moonPhaseByDate$mo[i] == "May"){
      moonPhaseByDate$mo2[i] <- '05'
    } else if(moonPhaseByDate$mo[i] == "Jun"){
      moonPhaseByDate$mo2[i] <- '06'
    } else if(moonPhaseByDate$mo[i] == "Jul"){
      moonPhaseByDate$mo2[i] <- '07'
    } else if(moonPhaseByDate$mo[i] == "Aug"){
      moonPhaseByDate$mo2[i] <- '08'
    } else if(moonPhaseByDate$mo[i] == "Sep"){
      moonPhaseByDate$mo2[i] <- '09'
    } else  if(moonPhaseByDate$mo[i] == "Oct"){
      moonPhaseByDate$mo2[i] <- '10'
    } else  if(moonPhaseByDate$mo[i] == "Nov"){
      moonPhaseByDate$mo2[i] <- '11'
    } else  if(moonPhaseByDate$mo[i] == "Dec"){
      moonPhaseByDate$mo2[i] <- '12'
    } 
  }
  moonPhaseByDate$day <- substr(moonPhaseByDate$date, 10,11)
  moonPhaseByDate$date2 <- paste(moonPhaseByDate$year, '-', moonPhaseByDate$mo2, '-', moonPhaseByDate$day, sep = '')
  moonPhaseByDate$date3 <- as.Date(moonPhaseByDate$date2)
  newMoons1 <- moonPhaseByDate$date3[moonPhaseByDate$phase == "New Moon"]
  
  #query 2
  urlMoon <- 'http://api.usno.navy.mil/moon/phase?date=1/3/2015&nump=99'
  
  moonPhases <- fromJSON(urlMoon)
  moonPhaseByDate <- moonPhases$phasedata
  moonPhaseByDate$year <- substr(moonPhaseByDate$date, 1,4)
  moonPhaseByDate$mo <- substr(moonPhaseByDate$date, 6,8)
  moonPhaseByDate$mo2 <- NULL
  for (i in 1:nrow(moonPhaseByDate)){
    if(moonPhaseByDate$mo[i] == "Jan"){
      moonPhaseByDate$mo2[i] <- '01'
    } else if(moonPhaseByDate$mo[i] == "Feb"){
      moonPhaseByDate$mo2[i] <- '02'
    } else if(moonPhaseByDate$mo[i] == "Mar"){
      moonPhaseByDate$mo2[i] <- '03'
    } else if(moonPhaseByDate$mo[i] == "Apr"){
      moonPhaseByDate$mo2[i] <- '04'
    } else if(moonPhaseByDate$mo[i] == "May"){
      moonPhaseByDate$mo2[i] <- '05'
    } else if(moonPhaseByDate$mo[i] == "Jun"){
      moonPhaseByDate$mo2[i] <- '06'
    } else if(moonPhaseByDate$mo[i] == "Jul"){
      moonPhaseByDate$mo2[i] <- '07'
    } else if(moonPhaseByDate$mo[i] == "Aug"){
      moonPhaseByDate$mo2[i] <- '08'
    } else if(moonPhaseByDate$mo[i] == "Sep"){
      moonPhaseByDate$mo2[i] <- '09'
    } else  if(moonPhaseByDate$mo[i] == "Oct"){
      moonPhaseByDate$mo2[i] <- '10'
    } else  if(moonPhaseByDate$mo[i] == "Nov"){
      moonPhaseByDate$mo2[i] <- '11'
    } else  if(moonPhaseByDate$mo[i] == "Dec"){
      moonPhaseByDate$mo2[i] <- '12'
    } 
  }
  moonPhaseByDate$day <- substr(moonPhaseByDate$date, 10,11)
  moonPhaseByDate$date2 <- paste(moonPhaseByDate$year, '-', moonPhaseByDate$mo2, '-', moonPhaseByDate$day, sep = '')
  moonPhaseByDate$date3 <- as.Date(moonPhaseByDate$date2)
  newMoons2 <- moonPhaseByDate$date3[moonPhaseByDate$phase == "New Moon"]
  
  #query 3
  urlMoon <- 'http://api.usno.navy.mil/moon/phase?date=12/30/2016&nump=99'
  
  moonPhases <- fromJSON(urlMoon)
  moonPhaseByDate <- moonPhases$phasedata
  moonPhaseByDate$year <- substr(moonPhaseByDate$date, 1,4)
  moonPhaseByDate$mo <- substr(moonPhaseByDate$date, 6,8)
  moonPhaseByDate$mo2 <- NULL
  for (i in 1:nrow(moonPhaseByDate)){
    if(moonPhaseByDate$mo[i] == "Jan"){
      moonPhaseByDate$mo2[i] <- '01'
    } else if(moonPhaseByDate$mo[i] == "Feb"){
      moonPhaseByDate$mo2[i] <- '02'
    } else if(moonPhaseByDate$mo[i] == "Mar"){
      moonPhaseByDate$mo2[i] <- '03'
    } else if(moonPhaseByDate$mo[i] == "Apr"){
      moonPhaseByDate$mo2[i] <- '04'
    } else if(moonPhaseByDate$mo[i] == "May"){
      moonPhaseByDate$mo2[i] <- '05'
    } else if(moonPhaseByDate$mo[i] == "Jun"){
      moonPhaseByDate$mo2[i] <- '06'
    } else if(moonPhaseByDate$mo[i] == "Jul"){
      moonPhaseByDate$mo2[i] <- '07'
    } else if(moonPhaseByDate$mo[i] == "Aug"){
      moonPhaseByDate$mo2[i] <- '08'
    } else if(moonPhaseByDate$mo[i] == "Sep"){
      moonPhaseByDate$mo2[i] <- '09'
    } else  if(moonPhaseByDate$mo[i] == "Oct"){
      moonPhaseByDate$mo2[i] <- '10'
    } else  if(moonPhaseByDate$mo[i] == "Nov"){
      moonPhaseByDate$mo2[i] <- '11'
    } else  if(moonPhaseByDate$mo[i] == "Dec"){
      moonPhaseByDate$mo2[i] <- '12'
    } 
  }
  moonPhaseByDate$day <- substr(moonPhaseByDate$date, 10,11)
  moonPhaseByDate$date2 <- paste(moonPhaseByDate$year, '-', moonPhaseByDate$mo2, '-', moonPhaseByDate$day, sep = '')
  moonPhaseByDate$date3 <- as.Date(moonPhaseByDate$date2)
  newMoons3 <- moonPhaseByDate$date3[moonPhaseByDate$phase == "New Moon"]
  
  #combine results of 3 queries
  newMoons <- c(newMoons1, newMoons2, newMoons3)
  
  captureData$date <- as.Date(captureData$collectDate)
  captureData$mo <- substr(captureData$collectDate, 6,7)
  captureData$year <- substr(captureData$collectDate, 1,4)
  
  perPlotNighta <- cbind.data.frame(captureData$nightuid, captureData$plotID, captureData$date, captureData$year, captureData$mo)
  perPlotNight <- unique(perPlotNighta)
  names(perPlotNight) <- c('nightuid', 'plotID', 'date', 'year', 'mo')
  perPlotNight <- arrange(perPlotNight, plotID, date)
  perPlotNight$date <- as.Date(perPlotNight$date)
  perPlotNight$boutID <- NA
  for (i in 1:nrow(perPlotNight)){
    a <- abs(perPlotNight$date[i] - newMoons)
    b <- newMoons[a<=12]
    if (length(b) == 1){
      perPlotNight$boutID[i] <- as.character(b)
    } #else { print(i)}
  }
  
  # problems:
  perPlotNight$boutID[perPlotNight$plotID == 'LAJA_002' & perPlotNight$date == "2016-08-16"] <- "2016-08-02"
  perPlotNight$boutID[perPlotNight$plotID == 'LAJA_002' & perPlotNight$date == "2016-08-17"] <- "2016-08-02"
  perPlotNight$boutID[perPlotNight$plotID == 'LAJA_002' & perPlotNight$date == "2016-08-18"] <- "2016-08-02"
  perPlotNight$boutID[perPlotNight$plotID == 'LAJA_003' & perPlotNight$date == "2016-08-16"] <- "2016-08-02"
  perPlotNight$boutID[perPlotNight$plotID == 'LAJA_003' & perPlotNight$date == "2016-08-17"] <- "2016-08-02"
  perPlotNight$boutID[perPlotNight$plotID == 'LAJA_003' & perPlotNight$date == "2016-08-18"] <- "2016-08-02"
  perPlotNight$boutID[perPlotNight$plotID == 'LAJA_023' & perPlotNight$date == "2016-08-16"] <- "2016-08-02"
  perPlotNight$boutID[perPlotNight$plotID == 'LAJA_023' & perPlotNight$date == "2016-08-17"] <- "2016-08-02"
  perPlotNight$boutID[perPlotNight$plotID == 'LAJA_023' & perPlotNight$date == "2016-08-18"] <- "2016-08-02"
  perPlotNight$boutID[perPlotNight$plotID == 'LAJA_005' & perPlotNight$date == "2016-08-19"] <- "2016-08-02"
  perPlotNight$boutID[perPlotNight$plotID == 'LAJA_010' & perPlotNight$date == "2016-08-19"] <- "2016-08-02"
  perPlotNight$boutID[perPlotNight$plotID == 'LAJA_018' & perPlotNight$date == "2016-08-19"] <- "2016-08-02"
  perPlotNight$boutID[perPlotNight$plotID == 'OSBS_002' & perPlotNight$date == 2013-08-27] <- "2013-08-06"
  perPlotNight$boutID[perPlotNight$plotID == 'OSBS_002' & perPlotNight$date == 2013-08-28] <- "2013-08-06"
  perPlotNight$boutID[perPlotNight$plotID == 'OSBS_002' & perPlotNight$date == 2013-08-29] <- "2013-08-06"
  perPlotNight$boutID[perPlotNight$plotID == 'OSBS_004' & perPlotNight$date == 2013-08-27] <- "2013-08-06"
  perPlotNight$boutID[perPlotNight$plotID == 'OSBS_004' & perPlotNight$date == 2013-08-28] <- "2013-08-06"
  perPlotNight$boutID[perPlotNight$plotID == 'OSBS_004' & perPlotNight$date == 2013-08-29] <- "2013-08-06"
  perPlotNight$boutID[perPlotNight$plotID == 'HARV_016' & perPlotNight$date == "2013-06-21"] <- "2013-06-08"
  
  # uBouts <- unique(perPlotNight$boutID)
  # uPlots <- unique(perPlotNight$plotID)
  # b = 1
  # problems <- data_frame(plotID = as.character(), date = as.character(), boutID = as.numeric())
  # for (i in uBouts){
  #   for (j in uPlots){
  #     temp <- perPlotNight$date[perPlotNight$boutID == i & perPlotNight$plotID == j]
  #     temp <- as.Date(temp)
  #     if (TRUE %in% (diff(temp) > 10)){
  #       a <- diff(temp)
  #       date <- temp[which(a>10) + 1]
  #       date <- as.character(date)
  #       rowtemp <- cbind(j, date, i)
  #       problems <- rbind(problems,rowtemp)
  #     }
  #   }
  # }
  if ('TRUE' %in% is.na(perPlotNight$boutID)){
    print(paste('Function cannot complete, as the following records have null boutIDs: ', which(is.na(perPlotNight$boutID)), sep =''))
  } else {
    perPlotNight2 <- data.frame(cbind(perPlotNight$nightuid, perPlotNight$boutID))
    perPlotNight2 <- unique(perPlotNight2)
    names(perPlotNight2) <- c("nightuid", "boutID")
    captureDataWithBoutID <- merge(captureData, perPlotNight2, by = "nightuid", all.x = T)
    return(captureDataWithBoutID)
  }
}