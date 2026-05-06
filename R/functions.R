#' Function for filepathhandling for nurses stress data file
#'
#' @param file_path path to datafile
#'
#' @returns dataset
read <- function(file_path, max_rows = 10) {
  data <- file_path %>%
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = to_snake_case,
      n_max = max_rows,
    )
  return(data)
}

read_all <- function(filename) {
  files <- here::here("data-raw/nurses-stress/") |>
    fs::dir_ls(regexp = filename, recurse = TRUE)

  data <- files |>
    purrr::map(read) |>
    purrr::list_rbind(names_to = "file_path_id")

  return(data)
}

get_participant_id <- function(data, mutate) {
  data_with_id <- data %>%
    dplyr::mutate(
      id = str_extract(
        file_path_id,
        pattern = "/stress/[:alnum:]{2}/"
      ) %>%
        stringr::str_remove("/stress/") %>%
        stringr::str_remove("/"),
      .before = file_path_id
    ) %>%
    dplyr::select(-file_path_id)
  return(data_with_id)
}
