library(tidyverse)

hosps <- read_tsv('data-raw/data/hosps.tsv')

atc <- read_rds('data-raw/data/atc.rds') %>%
  select(atc, drug = generic_name)

abx_sales <- read_rds('data-raw/data/abx_sales_raw.rds') %>%
  group_by(region, hospital, month = as.Date(cut(date, 'month')), atc) %>%
  summarise(ddd = sum(ddd),
            .groups = 'drop') %>%
  left_join(atc) %>%
  left_join(hosps) %>%
  relocate(hosp, .after = hospital)

usethis::use_data(abx_sales, overwrite = TRUE)
