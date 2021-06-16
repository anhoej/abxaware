library(tidyverse)

abx_aware <- read_tsv('data-raw/j01_aware.tsv') %>%
  mutate(across(starts_with('aware'),
                factor,
                levels = c('reserve', 'watch', 'access')))


usethis::use_data(abx_aware, overwrite = TRUE)
