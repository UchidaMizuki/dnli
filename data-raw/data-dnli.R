library(tidyverse)
library(fs)
library(readxl)
library(arrow)

# data-dnli ---------------------------------------------------------------

dir_create("data-raw/data-dnli")
download.file("https://nlftp.mlit.go.jp/ksj/gml/codelist/shape_property_table2.xlsx",
              "data-raw/data-dnli/shape_property_table2.xlsx",
              mode = "wb",
              quiet = TRUE)

col_name_pattern_dnli <- read_excel("data-raw/data-dnli/shape_property_table2.xlsx",
                                    sheet = "全データ",
                                    skip = 3,
                                    col_types = "text") |>
  rename_with(~ .x |>
                str_remove_all("\\s")) |>
  rename(pattern_file_name = `シェープファイル名（表記中のYYは年次、MMは月、PPは都道府県コード、AAは支庁コード、mmmmはメッシュコードを示します。）`,
         col_name_to = `属性名`,
         col_name_from = `属性コード`) |>
  select(pattern_file_name,
         col_name_to,
         col_name_from) |>
  fill(pattern_file_name) |>
  mutate(pattern_file_name = pattern_file_name |>
           str_split("\\n")) |>
  unnest(pattern_file_name) |>
  mutate(pattern_file_name = pattern_file_name |>
           str_remove("（.+）$") |>
           str_remove_all("\\s") |>
           str_replace_all(c("YY" = "\\\\d{2}",
                             "MM" = "\\\\d{2}",
                             "PP" = "\\\\d{2}",
                             "AA" = "\\\\d{2}",
                             "mmmm" = "\\\\d{4}"))) |>
  filter(pattern_file_name != "") |>
  relocate(pattern_file_name, col_name_from, col_name_to)

col_name_dnli <- col_name_pattern_dnli |>
  group_by(col_name_from) |>
  summarise(col_name_to = col_name_to |>
              str_c(collapse = "|"),
            .groups = "drop")

pattern_file_name_dnli <- col_name_pattern_dnli |>
  distinct(pattern_file_name) |>
  pull()

write_rds(col_name_pattern_dnli, "data-raw/data-dnli/col_name_pattern_dnli.rds")
write_rds(col_name_dnli, "data-raw/data-dnli/col_name_dnli.rds")
write_rds(pattern_file_name_dnli, "data-raw/data-dnli/pattern_file_name_dnli.rds")
