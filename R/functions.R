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

#' For time handling
#'
#' @param data
#' @param mutate
#'
#' @returns
#' @export
#'
#' @examples
summarise_by_datetime <- function(data) {
  summarised_data <- data %>%
    # Fill in below with the code we just wrote.
    dplyr::mutate(
      collection_datetime = lubridate::round_date(
        collection_datetime,
        unit = "minute"
      )
    ) %>%
    dplyr::summarise(
      dplyr::across(
        tidyselect::where(is.numeric),
        list(mean = mean, sd = sd, median = median)
      ),
      .by = c(id, collection_datetime)
    )
  return(summarised_data)
}


#' Tidy survey data
#'
#' @param data
#'
#' @returns
#' @export
#'
#' @examples
tidy_survey_dates <- function(data) {
  tidied <- data %>%
    dplyr::mutate(
      date = lubridate::mdy(date),
      start_datetime = lubridate::as_datetime(paste(date, start_time)),
      end_datetime = lubridate::as_datetime(paste(date, end_time)),
      datetime_id = start_datetime,
      .before = start_time
    ) %>%
    dplyr::select(-c(date, start_time, end_time, duration))
  return(tidied)
}

#' Longer function
#'
#' @param data
#'
#' @returns
#' @export
#'
#' @examples
survey_to_long <- function(data) {
  longer<-data %>%
    dplyr::select(
      id, datetime_id, start_datetime,
      end_datetime
    ) %>%
    tidyr::pivot_longer(c(
      start_datetime,
      end_datetime
    ), names_to = NULL, values_to = "collection_datetime") %>%
    dplyr::group_by(pick(-collection_datetime)) %>%
    tidyr::complete(collection_datetime = seq(min(collection_datetime),
                                              max(collection_datetime),
                                              by = 60
    )) %>%
    dplyr::ungroup()
  return(longer)
}
