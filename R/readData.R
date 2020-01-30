#' Read data in from a folder
#'
#' @usage readData(path = ".", estention = c("none", "ANA"))
#' @param path The path to folder containing the data files
#' @param estention The files estention identified the type of program produced the output
#' @examples 
#' f <- system.file("extdata", package = "Rwaves")
#' x <- readData(f, estention = "ANA")
#' @export
#' @author Marco Chiapello <chiapello.m@gmail.com>
#' @keywords IO, file
#' @import utils stats readr fs
readData <- function(path = ".", estention = c("none", "ANA")) UseMethod("readData")

readData <- function(path = ".", estention = c("none", "ANA")){
    estention <- match.arg(estention)
    if (estention == "none"){
        stop('You need to specify the data estention!')
    }
    f <- fs::dir_ls(path, regex = "converted")
    x <- suppressMessages(purrr::map_df(f, readr::read_delim, delim = "\t",
                                        col_names = FALSE, .id = "File"))
    ## Genaral checks
    if (any(is.na(x))) {
            stop("The datasets contains NAs")
    }
    if (ncol(x) != 4){
        x <- NULL
        stop("Incorrect dimentions: the dataset should contain at 4 columns")
    }
    ## Modify the table
    ### Remove the fourth column
    x  <- x[-ncol(x)]
    ### Rename the columns
    names(x) <- c("File", "waveforms", "time")
    return(x)
}
