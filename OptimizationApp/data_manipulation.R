all_assets <- read.table("test_file.txt")
all_assets <- as.tibble(all_assets)

sample_data <- all_assets %>% 
  select(V1, V3, V21, V24, V40) %>% 
  rename(year = V1,
         US_stocs = V3,
         cash = V21,
         UST_10 = V24,
         comm = V40)



