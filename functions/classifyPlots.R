classifyPlots <- function(captureData){
  
  nightByPlot <- captureData %>% distinct(year, boutID, collectDate, plotID)
  nightByPlot <- nightByPlot %>% filter(year != '2013' & year != '2014')
  nightsByPlot <- nightByPlot %>% group_by(year, plotID, boutID) %>% summarize(nights = (length(collectDate)))
  avgNightsByPlot <- nightsByPlot %>% group_by(plotID) %>% summarize(meanN = round(mean(nights), 0))
  diversityPlots <- avgNightsByPlot$plotID[avgNightsByPlot$meanN == 1]
  pathogenPlots <- avgNightsByPlot$plotID[avgNightsByPlot$meanN > 1]
  
  return(list(diversityPlots, pathogenPlots))

}