## Initial data import for strucchange

# Import and label data table
library(tidyverse); library(lubridate); library(stringr)
aprData <- s3tools::s3_path_to_full_df("alpha-bjml-personalpractice/changeDetection/totalsApr18.csv") %>% as.tibble()
colnames(aprData) <- c("Period", "totalTEW", "totalIO", "TOTAL")
head(aprData)

# Get Period into usable date format (defaulted to start of month)
aprData$Period <- str_c("01 ", aprData$Period) # for Date reading
aprData$PeriodStart <- as.Date(aprData$Period, "%d %b-%y")
aprData$Period <- substr(aprData$Period,4,10) # revert col. 1
head(aprData, 5); tail(aprData, 5)

# Initial plot
initGPlot <- ggplot(aprData, aes(PeriodStart, TOTAL)) + 
  geom_line() +
  labs(x = "Date", y = "Total Trials", title ="Total Trial Volume to April 2018")
plot(initGPlot)