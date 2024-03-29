#' Read data in from a folder
#'
#' @usage readData(path = ".", estention = c("none", "tsv", "ANA"))
#' @param path The path to folder containing the ANA data files or the name of the tsv file containing all the data
#' @param estention The files estention identified the type of program produced the output
#' @examples 
#' f <- system.file("extdata", package = "Rwaves")
#' x <- readData(f, estention = "ANA")
#' @export
#' @author Marco Chiapello <chiapello.m@gmail.com>
#' @keywords IO, file
#' @import utils readr fs purrr
readData <- function(path = ".", estention = c("none",  "tsv", "ANA")) UseMethod("readData")

readData <- function(path = ".", estention = c("none", "tsv", "ANA")){
    estention <- match.arg(estention)
    File <- time <- waveforms <- NULL
    if (estention == "none"){
        stop('You need to specify the data estention!')
    }
    if (estention == "tsv") {
        x <- readr::read_delim(path, delim = "\t")
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
        # Check that the last number in waveforms column is 99
        x$waveforms <- as.numeric(x$waveforms)
        x$time <- as.numeric(x$time)
        tmp <- x %>% dplyr::group_by(File) %>% 
                dplyr::top_n(1, time) 
        if (all(tmp %>% dplyr::pull(waveforms) != 99)){
            stop(paste0("The last number of each dataset should be 99.\n",
                        "The file(s) ", tmp %>% dplyr::filter(waveforms != 99) %>% dplyr::pull(File),
                        " do(es) not contain 99 as last waveform score"))
        } 
    }
    if (estention == "ANA"){
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
        # Check that the last number in waveforms column is 99
        x$waveforms <- as.numeric(x$waveforms)
        x$time <- as.numeric(x$time)
        tmp <- x %>% dplyr::group_by(File) %>% 
                dplyr::top_n(1, time) 
        if (any(tmp %>% dplyr::pull(waveforms) != 99)){
            stop(paste0("The last number of each dataset should be 99.\n",
                        "The file(s) ", tmp %>% dplyr::filter(waveforms != 99) %>% dplyr::pull(File),
                        " do(es) not contain 99 as last waveform score"))
        } 
    }
    return(x)
}
