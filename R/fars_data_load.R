# Building R Packages - Week 4 Assignment
# Simon West
#'filename: fars_data_load.R
#'
#' These functions read in data taken from the US National Highway Traffic Safety Administration's
#' [Fatality Analysis Reporting System](https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)).
#'
#'@title fars_read
#'
#'@description Reads in file to data variable using the read_csv function and creates a data frame tibble summarising the #'contents.
#'
#'@param filename A filename (and path to file from working directory if required) to be read in using the function.
#'
#'@return This function returns a dataframe tibble to the console.
#'
#'@importFrom readr read_csv
#'@importFrom dplyr tbl_df
#'
#'@examples
#'fars_read("directory/filename")
#'
#'fars_read("extdata/accident_2013.csv.bz2")
#'
#'
#'Error conditions:
#'fars_read(missing_file)
#'
#'
#'Incorrect filepath or missing file will result in "file 'filename' does not exist" error.
#'
#'@export
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'@title make_filename
#'
#'@description Helper function. Reads input, converts it to an interger and then creates a character vector incorporating
#'the input value to use as a filename using the sprintf function.
#'
#'@param year for the filename.
#'
#'@return character vector
#'
#'@examples
#'make_filename(2001)
#'> "accident_2001.csv.bz2"
#'
#'Error conditions:
#'make_filename(tulip)
#'
#'non numeric inputs will cause errors.
#'
#'@export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#'@title fars_read_years
#'
#'@description Reads the selected file into a dataframe then groups the data by year and month. Uses make_filename function to #'allow the user to select the year for the data rather than inputting the whole filename.
#'
#'@param years year(s) to be summarized
#'
#'@return a dataframe with the data sorted by year and month.
#'
#'@importFrom dplyr mutate select
#'@importFrom magrittr %>%
#'
#'@examples
#'fars_read_years(2013)
#'fars_read_years(c(2013:2015))
#'
#'Error conditions: Invalid year will result in error.
#'fars_read_years(2001)
#'>[[1]]
#'NULL
#'
#'Warning message:
#'  In value[[3L]](cond) : invalid year: 2001
#'
#'@export

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
