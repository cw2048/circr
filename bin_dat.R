#' @title bin_data
#'
#' @param data the activity data to bin
#' @param minutes_per_bin number of minutes per bin
#'
#' @return the data, after binning
#'
#' @description function to bin data time-wise
#'
#' @importFrom dplyr %>%
#'
#' @export
#'
#' @examples
#' f <- file.path(system.file(package = 'actogrammr'), 'testdata')
#' d <- read_clock_lab_file(file_name = list.files(path = f, full.names = TRUE)[1])
#' b <- bin_data(data = d, minutes_per_bin = 6)

bin_dat <- function(data,
                     minutes_per_bin) {
  
  stopifnot(minutes_per_bin %in% c(2, 3, 4, 5, 6, 10, 12, 15, 20, 30))
  
  #file_name <- hour <- bin <- act <- light <- 'fake_global_for_CRAN'
  
  data %>%
    dplyr::mutate(bin = as.integer((Minutes - 1) %/% minutes_per_bin)) %>%
    dplyr::group_by(filename, Date, Hour, bin, Cage, treat) %>%
    dplyr::summarise(bin_act = sum(Hops),
                     #need to add a numerical value for bin_light to sum
                     bin_light = sum(light)) %>%
    dplyr::ungroup() %>%
    return()
  
}
