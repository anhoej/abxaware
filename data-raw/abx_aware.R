# library(tidyverse)
#
# abx_aware <- read_tsv('data-raw/j01_aware.tsv') %>%
#   mutate(across(starts_with('aware'),
#                 factor,
#                 levels = c('reserve', 'watch', 'access')))

#
# usethis::use_data(abx_aware, overwrite = TRUE)

library(tidyverse)
library(docxtractr)

awr_levels <- c('global reserve',
                'reserve',
                'watch',
                'access')

case <- function(x, level) {
  case_when(level == 3 ~ str_to_upper(x),
            level == 4 ~ str_to_sentence(x),
            level == 5 ~ str_to_lower(x),
            TRUE ~ x)
}

# Complete list of ATC codes and generic drug names
# source: https://bioportal.bioontology.org/ontologies/ATC
atc <- read_csv('data-raw/data/ATC.csv.gz') %>%
  mutate(atc = str_extract(`Class ID`, '\\w*$')) %>%
  filter(str_detect(atc, '^J01')) %>%
  select(atc,
         atc_level = `ATC LEVEL`,
         generic_name = `Preferred Label`) %>%
  mutate(generic_name = case(generic_name, atc_level)) %>%
  arrange(atc)

write_rds(atc, 'data-raw/data/atc.rds')

# WHO AWaRe classification
# source: https://www.who.int/publications/i/item/WHOEMPIAU2019.11
aware_who <- readxl::read_excel('data-raw/data/WHO-EMP-IAU-2019.11-eng-1.xlsx',
                                'AWaRe Classification 2019',
                                skip = 3) %>%
  transmute(atc = str_trim(`ATC code`),
            aware_who = tolower(Category),
            aware_who = factor(aware_who,
                               levels = awr_levels)) %>%
  filter(str_detect(atc, '^J01')) %>%
  group_by(atc) %>%
  slice_min(aware_who) %>%
  ungroup() %>%
  distinct()

# UK AWaRe classification
# source: https://academic.oup.com/jac/article/74/11/3384/5540739
aware_uk <- read_docx('data-raw/data/dkz321_supplementary_data.docx') %>%
  docx_extract_tbl() %>%
  transmute(atc = ATC.code,
            aware_uk = tolower(England.AWaRe),
            aware_uk = factor(aware_uk,
                              levels = awr_levels)) %>%
  filter(str_detect(atc, '^J01')) %>%
  group_by(atc) %>%
  slice_min(aware_uk) %>%
  ungroup() %>%
  distinct()

# DK AWaRe classification
# source: Jonas BB
aware_dk <- read_tsv('data-raw/data/aware_dk.tsv') %>%
  type_convert() %>%
  transmute(atc,
            aware_dk = factor(aware_dk, levels = awr_levels))

abx_aware <- atc %>%
  full_join(aware_who) %>%
  full_join(aware_uk) %>%
  full_join(aware_dk)


usethis::use_data(abx_aware, overwrite = TRUE)

# write_csv2(d, 'atc_aware.csv', na = '')
