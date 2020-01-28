#' Compute the parameters
#'
#' @usage readData(path = ".", estention = c("none", "ANA"))
#' @param path The path to folder containing the data files
#' @param estention The files estention identified the type of program produced the output
#' @example
#' f <- dir(system.file("data", package = "Rwaves"))
#' readData(f, "ANA")
#' @export
#' @author Marco Chiapello <chiapello.m@gmail.com>
#' @keywords IO, file
#' @import utils stats dplyr
rwaves <- function(x) UseMethod("rwaves"){
    x %>% 
        dplyr::group_by(File) %>%
        tidyr::nest()
}

