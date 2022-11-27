#' Digital national land information
#'
#' @export
dnli <- function(url, download_dir,
                 headless = TRUE) {
  check_paths_allowed(url)

  driver <- driver_selenium(download_dir = download_dir,
                            headless = headless)

  Sys.sleep(1)
  driver$get(url)

  Jmap <- driver$find_elements(By$CSS_SELECTOR, "ul#Jmap")
  if (!vec_is_empty(Jmap)) {
    Jmap <- Jmap[[1L]]
    areas <- Jmap$find_elements(By$CSS_SELECTOR, "area")
    pb <- progress::progress_bar$new(total = vec_size(areas))
    for (area in areas) {
      pb$tick()
      if (area$get_attribute("class") == "") {
        area$click()
      }
    }
  }

  table <- driver$find_element(By$CSS_SELECTOR, "table.mb30.responsive-table")
  table <- table$get_attribute("outerHTML") |>
    rvest::read_html() |>
    rvest::html_table() |>
    dplyr::first() |>
    tibble::add_column(menu_button = table$find_elements(By$CSS_SELECTOR, "a#menu-button")) |>
    dplyr::select(!"\u30c0\u30a6\u30f3\u30ed\u30fc\u30c9") |> # Download
    dplyr::rename(region = "\u5730\u57df",
                  datum = "\u6e2c\u5730\u7cfb",
                  year = "\u5e74\u5ea6",
                  file_size = "\u30d5\u30a1\u30a4\u30eb\u5bb9\u91cf",
                  file_name = "\u30d5\u30a1\u30a4\u30eb\u540d") |>
    dplyr::mutate(year = parse_year_ja(.data$year),
                  file_size = fs::as_fs_bytes(.data$file_size))

  stickyr::new_sticky_tibble(table,
                             cols = c("file_name", "menu_button"),
                             col_show = !"menu_button",
                             attrs = c("download_dir", "driver"),
                             download_dir = download_dir,
                             driver = driver,
                             class = "dnli",
                             class_grouped_df = "dnli",
                             class_rowwise_df = "dnli")
}

#' @export
collect.dnli <- function(x, ...,
                         unzip = TRUE) {
  x <- dplyr::ungroup(x)
  download_dir <- attr(x, "download_dir")

  driver <- attr(x, "driver")
  file <- fs::path(download_dir, x$file_name)
  menu_button <- x$menu_button

  # Skip user survey
  try({
    menu_button[[1]]$click()
    close_btn_X <- driver$find_element(By$CSS_SELECTOR, "div.close_btn_X")

    if (close_btn_X$is_displayed()) {
      close_btn_X$click()
      warn("Skipped user survey. Please fill out the user survey later.")
    }
  },
  silent = TRUE)

  pb <- progress::progress_bar$new(total = vec_size(file))
  purrr::walk2(file, menu_button,
               purrr::slowly(function(file, menu_button) {
                 pb$tick()

                 if (fs::file_exists(file)) {
                   fs::file_delete(file)
                 }

                 menu_button$click()
                 driver$switch_to$alert$accept()

                 check_file(file)

                 if (unzip) {
                   exdir <- file |>
                     stringr::str_extract("(?<=/)[^/]+(?=\\.zip$)")
                   exdir <- fs::path(download_dir, exdir)

                   unzip(file,
                         exdir = exdir)
                   fs::file_delete(file)
                 }
               }))

  driver$close()
  invisible()
}

#' @export
tbl_sum.dnli <- function(x) {
  out <- NextMethod()
  names(out)[[1L]] <- "Digital national land information"
  out
}

#' @export
read_dnli <- function(path, ...,
                      options = "ENCODING=shift-jis") {
  file <- path |>
    purrr::map(function(path) {
      if (fs::is_file(path)) {
        file <- path
      } else {
        file <- fs::dir_ls(path,
                           recurse = TRUE,
                           regexp = "\\.shp$")
      }

      file
    })
  file <- vec_c(!!!file)

  file |>
    purrr::map_dfr(function(file) {
      out <- sf::read_sf(file, ...,
                         options = options)

      file_name <- basename(file)

      loc <- stringr::str_detect(file_name, pattern_file_name_dnli)
      pattern_file_name <- vec_slice(pattern_file_name_dnli, loc)

      if (vec_is_empty(pattern_file_name)) {
        col_name <- vec_slice(col_name_dnli, col_name_dnli$col_name_from %in% names(out))

        if (!vec_is_empty(col_name)) {
          out <- out |>
            dplyr::rename_with(~ col_name$col_name_to,
                               col_name$col_name_from)
        }
      } else if (vec_size(pattern_file_name) == 1L) {
        col_name <- vec_slice(col_name_pattern_dnli, col_name_pattern_dnli$pattern_file_name == pattern_file_name)
        col_name <- vec_slice(col_name, col_name$col_name_from %in% names(out))

        if (!vec_is_empty(col_name)) {
          out <- out |>
            dplyr::rename_with(~ col_name$col_name_to,
                               col_name$col_name_from)
        }
      }

      out
    })
}
