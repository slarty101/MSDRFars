# Building R Packages - Week 4 Assignment
# Simon West
#'filename: fars_plot_data.R
#'
#'@title fars_map_state
#'
#'@description Generates a map of the state selected with the accident locations plotted using the latitude & longitude data for #'the selected year
#'
#'@param years the year to be plotted
#'@param state the state number to be used
#'
#'@return a map of the selected state with the accident data plotted
#'
#'@importFrom dplyr filter
#'@importFrom maps map
#'@importFrom graphics points
#'
#'@examples
#'fars_map_state(30, 2013)
#'
#'Error conditions:
#'\dontrun {fars_map_state(3,2013)}
#'
#'> Error in fars_map_state(3, 2013) : invalid STATE number: 3
#'
#'@export
#'
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
