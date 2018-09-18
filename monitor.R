## Exploration of 'live' monitoring using windowed data
# How soon can we spot the big jump at March 2013?
# Monitor through June 2012-June 2013 using 2010-2012 as history.
windowData <- window(tsAprTotal, start = c(2010, 1), end = c(2012,5))
tt <- 1:length(windowData)
me.mefp <- mefp(windowData ~ tt, type = "ME", alpha = 0.05)

# Now new data is added to windowData
windowData <- window(tsAprTotal, start = c(2010, 1), end = c(2013,12))
tt <- 1:length(windowData)
# Monitor on each new piece
me.mefp <- monitor(me.mefp) # By end of 2013 a change is detected
me.mefp
plot(me.mefp)
as.Date(windowData)[43] # Appears to be dated July 2013 (Not Mar)

# If we ran this live (point by point) each month June 2012 onwards, what would happen?
  # Reinitialise history window 2010.1 to 2012.5
  windowData <- window(tsAprTotal, start = c(2010, 1), end = c(2012,5))
  tt <- 1:length(windowData)
  me.mefp <- mefp(windowData ~ tt, type = "ME", alpha = 0.05)
  endDate = ymd(20120501)
  
  # Now increase the window month by month
for (i in 1:20){
  endDate <- endDate + months(1)
  newEndVec <- c(year(endDate), month(endDate))
  windowData <- window(tsAprTotal, start = c(2010, 1), end = newEndVec) # Update windowData
  tt <- 1:length(windowData) # Update length
  me.mefp <- monitor(me.mefp) # Monitor on change
  
  if (is.na(me.mefp["breakpoint"])){
    print(paste("[iter.",str_pad(i,2),"]", month(endDate, label = TRUE), year(endDate), "No Breakpoint Detected", sep = " "))
    plot(me.mefp)
  } else {
    print(paste("[iter.",str_pad(i,2),"]", month(endDate, label = TRUE), year(endDate), "Breakpoint Detected at", as.Date(windowData)[as.numeric(me.mefp["breakpoint"])], sep = " "))
    plot(me.mefp)
    break
  }
}


## Monitoring of FIRST DIFFS (rubbish)
dwindowData <- window(tsAprTotal, start = c(2010, 1), end = c(2012,5)) %>% diff()
plot(dwindowData)
tt <- 1:length(dwindowData)
me.mefp <- mefp(dwindowData ~ tt, type = "ME", alpha = 0.05)

dwindowData <- window(tsAprTotal, start = c(2010, 1), end = c(2013,12)) %>% diff()
plot(dwindowData)
tt <- 1:length(dwindowData)
me.mefp <- monitor(me.mefp)
me.mefp
plot(me.mefp)
