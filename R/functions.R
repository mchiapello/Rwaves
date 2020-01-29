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
#' @import utils stats dplyr magrittr
rwaves <- function(x) UseMethod("rwaves")

rwaves <- function(x){
    tmp <- x %>%
        dplyr::group_by(File) %>%
        tidyr::nest()

    tmp %>%
        dplyr::mutate(f1 = purrr::map(data, ~ff1(.x, 1))) %>%
        dplyr::mutate(f2 = purrr::map(data, ~ff2(.x, 1))) %>%
        dplyr::mutate(f3 = purrr::map(data, ~ff3(.x, 1))) %>%
        dplyr::mutate(f14 = purrr::map(data, ~ff14(.x))) %>%
        dplyr::mutate(f24 = purrr::map(data, ~ff24(.x))) %>%
        tidyr::unnest(f1:f24)




}

ff1 <- function(x, d = 1){
    x %>%
        dplyr::count(waveforms) %>%
        dplyr::filter(waveforms == d) %>%
        dplyr::select(f1 = n)
}

ff2 <- function(x, d = 1){
    x$cum <- c(x$time[1], diff(x$time))
    x %>%
       dplyr::filter(waveforms == d) %>%
       dplyr::group_by(waveforms) %>%
       dplyr::summarize(Sum = sum(time)) %>%
       dplyr::select(f2 = Sum)
}

ff3 <- function(x, d = 1){
    x$cum <- c(x$time[1], diff(x$time))
    x %>%
        dplyr::filter(waveforms == d) %>%
        dplyr::slice(2) %>%
        dplyr::select(f3 = cum)
}

ff14 <- function(x){
    temp <- x$waveforms
    temp2 <- vector()
    for(i in 1:(length(tmp)-2)){
        if(tmp[i] == 1 & tmp[i+2] == 1){
            tmp2 <- c(tmp2, 1)
        }
    }
    sum(tmp2)
#     tibble(f4 = sum(tmp2))
}

ff24 <- function(x){
    x$cum <- c(x$time[1], diff(x$time))
    temp <- x %>%
       dplyr::filter(waveforms == 1) %>%
       dplyr::group_by(waveforms) %>%
       dplyr::summarize(Sum = sum(cum)) %>%
       dplyr::pull(Sum)
   x$time[nrow(x)] - temp
}
