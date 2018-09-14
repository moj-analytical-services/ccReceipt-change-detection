aprData <- s3tools::s3_path_to_full_df("alpha-bjml-personalpractice/changeDetection/totalsApr18.csv") %>% as.tibble()
colnames(aprData) <- c("Period", "totalTEW", "totalIO", "TOTAL")
head(aprData)

library(tidyverse); library(lubridate); library(stringr)


aprData$Period <- str_c("01 ", aprData$Period)
aprData$PeriodStart <- as.Date(aprData$Period, "%d %b-%y")
aprData$Period <- substr(aprData$Period,4,10)
head(aprData)

ggplot(aprData, aes(PeriodStart, TOTAL)) + 
  geom_line() +
  labs(x = "Date", y = "Total Trials", title ="Total Trial Volume to April 2018")
