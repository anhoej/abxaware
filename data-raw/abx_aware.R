library(tidyverse)

abx_aware <- read_csv2('data-raw/J01_aware.csv') %>%
  filter(str_detect(atc, '^J01'),
         adm_route %in% c('iv', NA)) %>%
  select(-adm_route) %>%
  distinct() %>%
  mutate(across(starts_with('aware'),
                factor,
                levels = c('reserve', 'watch', 'access')))


usethis::use_data(abx_aware, overwrite = TRUE)
