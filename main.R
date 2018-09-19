## Import April 18 CC data and display initial plot
source('importData.R') # OUT: aprData timeseries, use aprData.namesdict for dictionary of column names
config <- read.table("config.txt", header=TRUE, sep="=", row.names=1, strip.white=TRUE, stringsAsFactors=FALSE)

## TS Explore [NOT USED, WILL NOT WORK]
plot(tsAprTotal)
tsAprTotal <- ts(aprData$TOTAL, frequency = 12, start = c(2008, 1), end = c(2018, 2))
  plot(tsAprTotal)
dcAprTotal <- decompose(tsAprTotal, type = "additive")
  plot(dcAprTotal)
tindex <- 1:length(aprData$TOTAL) # index times
fitApr <- ts(loess(aprData$TOTAL ~ tindex, span = 0.1)$fitted, frequency = 12, start = c(2008, 1))
plot(tsAprTotal); lines(fitApr, col  = 4)

## STRUCCHANGE // setup and useful functions
library(strucchange)
struccPlot <- function(tsData, brData, nBreak){
  # Displays chosen number of breaks on base timeseries datam with location error bars
  plot(tsData)
  lines(fitted(brData, breaks = nBreak), col = 4)
  lines(confint(brData, breaks = nBreak))
  grid(nx = NULL, ny = NA, col = "lightgray", lty = "dotted",
       lwd = par("lwd"))
}
brDates <- function(brData){
  # Takes numeric breakdates output (as e.g. 2012.25 for Apr '12) and converts to list of Month Year periods.
  # TODO rewrite this to avoid use of added 1m shift
  brData_long <- paste("01", trunc(12*(brData %% 1)), trunc(brData), sep = " ") %>% as.Date("%d %m %Y")
  brData_strings <- paste(month(brData_long+months(1), label = TRUE, abbr = FALSE), year(brData_long), sep = " ") # 1m shift is a fiddle because 2012.0 = January
  print("Structural breaks occur at: ")
  return(brData_strings)
}

## STRUCCHANGE// level breaks
column.choice <- config["offence.choice", "VALUE"] # Choose offence type (name.short) from aprData.namesdict and put into config.txt
brApr.level <- breakpoints(aprData[,column.choice] ~ 1, h = 0.1) # ~1 is LEVELS
summary(brApr.level); plot(brApr.level) # Choose nBreak here and put back into config [RELOAD]
nBreak.level <- config["nBreak.level", "VALUE"] %>% as.numeric()
struccPlot(aprData[,column.choice], brData = brApr.level, nBreak = nBreak.level)

## STRUCCHANGE // trend breaks
column.choice <- column.choice # Option to change offence here
tindex <- 1:length(aprData[,column.choice]) # Index times

trend_fit <- lm(tsAprTotal ~ tindex) # linear model
  summary(trend_fit) # check LR model is signif
  
brApr.trend <- breakpoints(aprData[,column.choice] ~ tindex, h = 0.1) # ~ t is TREND (LR)
summary(brApr.trend); plot(brApr.trend) # choose nBreak for min BIC [RELOAD CONFIG]
nBreak.trend <- config["nBreak.trend", "VALUE"] %>% as.numeric()
struccPlot(aprData[,column.choice], brData = brApr.trend, nBreak = nBreak.trend)
brDates(breakdates(brApr.trend, breaks = nBreak.trend))
breakdates(brApr.trend, breaks = nBreak.trend, format.times = TRUE)

## STRUCCHANGE // polynomial fitting breaks
# fit to 2o polynomial using lm(tsAprTotal ~ tt + I(tt^2))
# (I() here just preserves class of tt as is)
