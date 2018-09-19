## Initial data import for strucchange

# Import and label data table
library(tidyverse); library(lubridate); library(stringr)
aprData <- s3tools::s3_path_to_full_df("alpha-bjml-personalpractice/changeDetection/alldataApr18.csv") %>% as.tibble()
aprData.namesdict <- data.frame("name.long" = colnames(aprData))
colnames(aprData) <- c("PeriodStart",
                       "violence.EW", "sexual.EW", "robbery.EW", "theft.EW", "damagearson.EW", "drug.EW", "poweapons.EW", "porder.EW", "misc.EW", "fraud.EW", "snm.EW",
                       "violence.IO", "sexual.IO", "robbery.IO", "theft.IO", "damagearson.IO", "drug.IO", "poweapons.IO", "porder.IO", "misc.IO", "fraud.IO", "snm.IO",
                       "SENTENCES", "APPEALS",
                       "TRIALS.EW", "TRIALS.IO", "TRIALS.TOTAL",
                       "GTOTAL")
aprData.namesdict <- cbind(aprData.namesdict, "name.short" = colnames(aprData))

# Work out parameters for the timeseries
aprData$PeriodStart <- paste("01", aprData$PeriodStart) %>% as.Date("%d %b-%y")
aprData.startv <- c(year(min(aprData$PeriodStart)), month(min(aprData$PeriodStart)))
aprData.endv <- c(year(max(aprData$PeriodStart)), month(max(aprData$PeriodStart)))

# Cut 'PeriodStart' and convert CC reciepts data to a timeseries
aprData <- aprData %>% select(-PeriodStart)
aprData <- ts(aprData, frequency = 12, start = aprData.startv, end = aprData.endv)