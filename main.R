## Import April 18 CC data and display initial plot
source('importData.R') # OUT: aprData tibble, totalTEW/totalIO/TOTAL monthly Jan01-Feb18

## TS Explore
plot(tsAprTotal)
dcAprTotal <- decompose(tsAprTotal, type = "additive")
  plot(dcAprTotal)
tindex <- 1:length(aprData$TOTAL) # index times
fitApr <- ts(loess(aprData$TOTAL ~ tindex, span = 0.1)$fitted, frequency = 12, start = c(2008, 1))
plot(tsAprTotal); lines(fitApr, col  = 4)

## STRUCCHANGE // setup and useful functions
packages.install("strucchange")
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
brAprTotal_l <- breakpoints(tsAprTotal ~ 1, h = 0.1) # ~1 is LEVELS
summary(brAprTotal_l); plot(brAprTotal_l)
struccPlot(tsAprTotal, brData = brAprTotal_l, nBreak = 4)


## STRUCCHANGE // trend breaks
trend_fit <- lm(tsAprTotal ~ tindex) # linear model
  summary(trend_fit) # check LR model is signif
brAprTotal_t <- breakpoints(tsAprTotal ~ tindex, h = 0.1) # ~ t is TREND (LR)
summary(brAprTotal_t); plot(brAprTotal_t) # choose nBreak for min BIC
struccPlot(tsAprTotal, brData = brAprTotal_t, nBreak = 2)
brDates(breakdates(brAprTotal_t, breaks = 2))
breakdates(brAprTotal_t, breaks = 2, format.times = TRUE)

brAprTEW_t <- breakpoints(tsAprTEW ~ tindex, h = 0.1)
summary(brAprTEW_t); plot(brAprTEW_t)
struccPlot(tsAprTEW, brData = brAprTEW_t, nBreak=2)

## STRUCCHANGE // polynomial fitting breaks
# fit to 2o polynomial using lm(tsAprTotal ~ tt + I(tt^2))
# (I() here just preserves class of tt as is)
