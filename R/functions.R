#' Function for filepathhandling for nurses stress data file
#'
#' @param file_path path to datafile
#'
#' @returns dataset
read <- function(file_path, max_rows = 100) {
  data <- file_path %>%
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = to_snake_case,
      n_max = max_rows,
    )
  return(data)
}
