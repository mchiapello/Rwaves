#' Original data
#'
#' A dataset containing the data for 6 Samples
#' The scoring system is ANA
#'
#' @format A data frame with 1800 rows and 3 variables:
#' \describe{
#'   \item{File}{Sample identifier}
#'   \item{waveforms}{Waveform scoring}
#'   \item{time}{waveforms duration}
#' }
#' @source PhD Matteo Ripamonti
"original"

#' Compute data
#'
#' A dataset containing the processed data
#' The scoring system is ANA
#'
#' @format A data frame with 6 rows and 14 variables:
#' \describe{
#'   \item{File}{Sample identifier}
#'   \item{data}{Contains the original data}
#'   \item{f1_1}{total number of 1}
#'   \item{f2_1}{total duration of 1}
#'   \item{f3_1}{duration of the 2nd 1 wave}
#'   \item{f14}{number of probes}
#'   \item{f24}{total recording time - total duration of 1}
#'   \item{f2_2}{total duration of 2}
#'   \item{f2_2}{total duration of 6}
#'   \item{f1_7}{total number of 7}
#'   \item{f115_2}{%probtimeinC 2}
#'   \item{f115_6}{%probtimeinC 6}
#'   \item{f115_7}{%probtimeinC 7}
#' }
#' @source PhD Matteo Ripamonti
"processed"

