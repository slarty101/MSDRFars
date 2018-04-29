# Building R Packages - Week 4 Assignment
# Simon West
#'filename: fars_summarize_data.R
#'
#' This function summarises data taken from the US National Highway Traffic Safety Administration's
#' [Fatality Analysis Reporting System](https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)).
#' The fars_data_load functions (make_filename, fars_read & fars_read_years) import the data.
#'
#'@title fars_summarize_years
#'
#'@description Summarizes accident data by month for selcted year(s).
#'
#'@param years selected year(s) to summarize.
#'
#'@return A list of integers representing the months and accident counts for the selected year.
#'
#'@importFrom dplyr bind_rows group_by summarize
#'@importFrom magrittr %>%
#'@importFrom tidyr spread
#'
#'@examples
#'fars_summarize_years(2013)
#'
#'Error conditions: Years where there are no data will result in error.
#'fars_summarize_years(3100)
#'
#'Error in grouped_df_impl(data, unname(vars), drop) :
#'Column `year` is unknown
#'
#'@export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
