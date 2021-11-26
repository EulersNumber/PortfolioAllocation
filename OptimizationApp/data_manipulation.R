library(tidyverse)
library(timeSeries)

# Data and manipulation ----

full_data <- read.csv2("monthly_data_1994.csv", header = T) %>% as.tibble()
colnames(full_data)

# filter out year and month columns
full_data_assets <- full_data %>% select(!c(ï..Y, M))

# convert to time series data for fPortoflio
full_data_ts <- as.timeSeries(full_data_assets)

full_data_ts <- full_data_ts %>% `setTime<-`(seq(as.Date("1994/1/1"), as.Date("2021/10/1"), by = "month"))
head(full_data_ts)[,1:5] # data is ready for optimization


# subset for data exploration
subset_2010 <- full_data %>% dplyr::filter(?..Y > 2009)
subset_2010_assets <- subset_2010 %>% select(!c(?..Y, M))
subset_2010_ts <- as.timeSeries(subset_2010_assets)
subset_2010_ts <- subset_2010_ts %>% `setTime<-`(seq(as.Date("2010/1/1"), as.Date("2004/12/1"), by = "month"))
subset_2010 %>% head()

