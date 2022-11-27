library(tidyverse)

# internal ----------------------------------------------------------------

col_name_pattern_dnli <- read_rds("data-raw/data-dnli/col_name_pattern_dnli.rds")
col_name_dnli <- read_rds("data-raw/data-dnli/col_name_dnli.rds")
pattern_file_name_dnli <- read_rds("data-raw/data-dnli/pattern_file_name_dnli.rds")

usethis::use_data(
  col_name_pattern_dnli,
  col_name_dnli,
  pattern_file_name_dnli,
  internal = TRUE,
  overwrite = TRUE)
