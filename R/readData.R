#' Read data in form a folder
#'
#' @usage readData(path = ".", estention = c("ANA"))
#' @param path The path to folder containing the data files
#' @param estention The files estention identified the type of program produced the output
#' @example
#' f <- dir(system.file("data", package = "Rwaves"))
#' readData(f, "ANA")
#' @return an object of class \code{ana}
#' @export
#' @author Marco Chiapello <chiapello.m@gmail.com>
#' @keywords IO, file
#' @import methods utils stats readr
readData <- function(infile, type = c("ANA")) UseMethod("readData")

readData <- function(infile, type = c("ANA")){
    type <- match.arg(type)
    if (type == "none"){
        stop('You need to specify the data type!')
    }
    x <- suppressMessages(read_csv(infile))
    ## Genaral checks
    if (any(is.na(x))) {
            stop("The datasets contains NAs")
    }
    if (dim(x)[2] < 3){
        x <- NULL
        stop("Incorrect dimentions: the dataset should contain at least 3 columns")
    }
    ## Trouvelot specific checks
    if (type == "trouvelot"){
