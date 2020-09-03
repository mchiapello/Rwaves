#' Compute the parameters
#'
#' @usage rwaves(x)
#' @param x it is the object read using the rwaves::readData function
#' @examples
#' rwaves(original)
#' @export
#' @author Marco Chiapello <chiapello.m@gmail.com>
#' @importFrom tidyr  nest
#' @importFrom tidyr  unnest
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom dplyr tibble
#' @importFrom dplyr left_join
#' @importFrom dplyr inner_join
#' @importFrom dplyr pull
#' @importFrom dplyr tbl_df
#' @importFrom dplyr funs
#' @importFrom dplyr vars
#' @importFrom dplyr contains
#' @importFrom dplyr slice
#' @importFrom dplyr slice_tail
rwaves <- function(x) UseMethod("rwaves")

rwaves <- function(x){
    ###########################################################################
    # VARIABLES
    waveforms <- cum <- Sum <- File <- f1 <- f117 <- `:=` <- n <- time <- NULL
    index1 <- index2 <- id <- sv <- d <- f24 <- f91 <- f95 <- f201 <- NULL
    id2 <- res <- NULL
    ###########################################################################
    # FORMULA
        # total number of "X"
    ff1 <- function(x, d = 1){
        newname <- paste0("f1_", d)
        out <- x %>%
            dplyr::count(waveforms) %>%
            dplyr::filter(waveforms == d) %>%
            dplyr::select(n) %>%
            dplyr::rename(!!newname := n)
       if(nrow(out) == 0){
           out[1, 1] <- 0
       }
       return(out)
    }
    # total duration of "X"
    ff2 <- function(x, d = 1){
        newname <- paste0("f2_", d)
        x$cum <- c(diff(x$time), x$time[length(x$time)])
        out <- x %>%
           dplyr::filter(waveforms == d) %>%
           dplyr::group_by(waveforms) %>%
           dplyr::summarize(Sum = sum(cum)) %>%
           dplyr::select(Sum) %>%
           dplyr::rename(!!newname := Sum)
       if(nrow(out) == 0){
           out[1, 1] <- 0
       }
       return(out)
    }
    # duration of the 2nd "1" wave
    ff3 <- function(x, d = 1){
        newname <- paste0("f3_", d)
        x$cum <- c(diff(x$time), x$time[length(x$time)])
        out <- x %>%
            dplyr::filter(waveforms == d) %>%
            dplyr::slice(2) %>%
            dplyr::select(cum) %>%
            dplyr::rename(!!newname := cum)
       if(nrow(out) == 0){
           out[1, 1] <- 0
       }
       return(out)
    }
    # number of probes
    #' everything between two "1" waves is a probe; it is possible that at the end
    #' of the recording a probe is still ongoing, so there is no "1" wave after
    ff14 <- function(x){
        newname <- paste0("f", 14)
        temp <- x$waveforms
        if(temp[length(temp) -1] == 1){
            dplyr::tibble(!!newname := (sum(temp == 1) - 1))
        } else{
            dplyr::tibble(!!newname := sum(temp == 1))
        }
    }
    # total recording time - total duration of 1
    ff24 <- function(x){
        newname <- paste0("f", 24)
        x$cum <- c(diff(x$time), x$time[length(x$time)])
        temp <- (x %>% filter(waveforms != 99) %>% dplyr::pull(cum) %>% sum()) - x %>%
           dplyr::filter(waveforms == 1) %>%
           dplyr::group_by(waveforms) %>%
           dplyr::summarize(Sum = sum(cum)) %>%
           dplyr::pull(Sum)
       dplyr::tibble(!!newname := temp)
    }
    # %probtimeinC
    ff115 <- function(x, d = 2){
        newname <- paste0("f115_", d)
        x$cum <- c(diff(x$time), x$time[length(x$time)])
        tt <- (x %>% filter(waveforms != 99) %>% dplyr::pull(cum) %>% sum()) - x %>%
           dplyr::filter(waveforms == 1) %>%
           dplyr::group_by(waveforms) %>%
           dplyr::summarize(Sum = sum(cum)) %>%
           dplyr::pull(Sum)
        temp <- x %>%
           dplyr::filter(waveforms == d) %>%
           dplyr::group_by(waveforms) %>%
           dplyr::summarize(Sum = sum(cum)) %>%
           dplyr::pull(Sum)
       if(length(temp) == 0){
           out <- dplyr::tibble(!!newname := 0)
       } else {
           out <- dplyr::tibble(!!newname := temp / tt * 100)
       }
       return(out)
  }
   # total number of 5
    ff89 <- function(x){
        newname <- paste0("f89_5")
        out <- x %>%
    dplyr::mutate(index1 = dplyr::case_when(waveforms == 5 ~ 1,
                              waveforms %in% c(2, 99) ~ 0,
                              TRUE ~ 3)) %>%
    dplyr::mutate(index1 = ifelse(index1 == 3, NA, index1)) %>%
    tidyr::fill(index1) %>%
    dplyr::mutate(index1 = ifelse(is.na(index1), 0, index1)) %>%
    dplyr::mutate(index2 = ifelse(index1 == 1 & dplyr::lead(index1 == 0), 1, 0)) %>%
    dplyr::summarise(Sum = sum(index2, na.rm = TRUE)) %>%
    dplyr::rename(!!newname := Sum)
       if(nrow(out) == 0){
           out[1, 1] <- 0
       }
       return(out)
    }
    # Number of waveform 5 longer of 10 minutes
    ff90 <- function(x){
        newname <- paste0("f90_5")
        x$cum <- c(diff(x$time), x$time[length(x$time)])
        out <- x %>%
    dplyr::mutate(index1 = dplyr::case_when(waveforms == 5 ~ 1,
                              waveforms %in% c(2, 99) ~ 0,
                              TRUE ~ 3)) %>%
    dplyr::mutate(index1 = ifelse(index1 == 3, NA, index1)) %>%
    tidyr::fill(index1) %>%
    dplyr::mutate(index1 = ifelse(is.na(index1), 0, index1)) %>%
    dplyr::mutate(id = LETTERS[replace(with(rle(index1),
                                     rep(cumsum(values), lengths)), index1 == 0, NA)]) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(sv = sum(cum)) %>%
    dplyr::filter(!is.na(id),
           sv >= 600) %>%
    dplyr::count() %>%
    dplyr::rename(!!newname := n)
       if(nrow(out) == 0){
           out[1, 1] <- 0
       }
       return(out)
    }
    # mean duration of one "5"
    ff92 <- function(x){
        newname <- paste0("f92_5")
        x$cum <- c(diff(x$time), x$time[length(x$time)])
        out <- x %>%
    dplyr::mutate(index1 = dplyr::case_when(waveforms == 5 ~ 1,
                              waveforms %in% c(2, 99) ~ 0,
                              TRUE ~ 3)) %>%
    dplyr::mutate(index1 = ifelse(index1 == 3, NA, index1)) %>%
    tidyr::fill(index1) %>%
    dplyr::mutate(index1 = ifelse(is.na(index1), 0, index1)) %>%
    dplyr::mutate(id = LETTERS[replace(with(rle(index1),
                                     rep(cumsum(values), lengths)), index1 == 0, NA)]) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(sv = sum(cum)) %>%
    dplyr::filter(!is.na(id)) %>%
    dplyr::summarise(n = mean(sv)) %>%
    dplyr::rename(!!newname := n)
       if(nrow(out) == 0 | is.nan(out$f92_5)){
           out[1, 1] <- 0
       }
       return(out)
    }
    # duration of the longest "5"
    ff93 <- function(x){
        newname <- paste0("f93_5")
        x$cum <- c(diff(x$time), x$time[length(x$time)])
        out <- x %>%
    dplyr::mutate(index1 = dplyr::case_when(waveforms == 5 ~ 1,
                              waveforms %in% c(2, 99) ~ 0,
                              TRUE ~ 3)) %>%
    dplyr::mutate(index1 = ifelse(index1 == 3, NA, index1)) %>%
    tidyr::fill(index1) %>%
    dplyr::mutate(index1 = ifelse(is.na(index1), 0, index1)) %>%
    dplyr::mutate(id = LETTERS[replace(with(rle(index1),
                                     rep(cumsum(values), lengths)), index1 == 0, NA)]) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(sv = sum(cum)) %>%
    dplyr::filter(!is.na(id)) %>%
    dplyr::filter(sv == max(sv)) %>%
    dplyr::select(n = sv) %>%
    dplyr::rename(!!newname := n)
       if(nrow(out) == 0){
           out[1, 1] <- 0
       }
       return(out)
    }
    # total duration of "3", "4" and "5"
    ff96 <- function(x){
        newname <- paste0("f96_345")
        x$cum <- c(diff(x$time), x$time[length(x$time)])
        out3 <- x %>%
           dplyr::filter(waveforms == 3) %>%
           dplyr::group_by(waveforms) %>%
           dplyr::summarize(Sum = sum(cum)) %>%
           dplyr::pull(Sum)
        out4 <- x %>%
           dplyr::filter(waveforms == 4) %>%
           dplyr::group_by(waveforms) %>%
           dplyr::summarize(Sum = sum(cum)) %>%
           dplyr::pull(Sum)
        out5 <- x %>%
            dplyr::mutate(index1 = dplyr::case_when(waveforms == 5 ~ 1,
                                      waveforms %in% c(2, 99) ~ 0,
                                      TRUE ~ 3)) %>%
            dplyr::mutate(index1 = ifelse(index1 == 3, NA, index1)) %>%
            tidyr::fill(index1) %>%
            dplyr::mutate(index1 = ifelse(is.na(index1), 0, index1)) %>%
            dplyr::mutate(id = LETTERS[replace(with(rle(index1),
                                             rep(cumsum(values), lengths)), index1 == 0, NA)]) %>%
            dplyr::group_by(id) %>%
            dplyr::summarise(sv = sum(cum)) %>%
            dplyr::filter(!is.na(id)) %>%
            dplyr::summarise(Sum = sum(sv)) %>%
            dplyr::pull(Sum)
       out <- tibble(!!newname := sum(out3, out4, out5))
       if(nrow(out) == 0){
           out[1, 1] <- 0
       }
       return(out)
    }
        # Total duration of "5"
    ff91 <- function(x){
        newname <- paste0("f91")
        x$cum <- c(diff(x$time), x$time[length(x$time)])
        out <- x %>%
            dplyr::mutate(index1 = dplyr::case_when(waveforms == 5 ~ 1,
                                      waveforms %in% c(2, 99) ~ 0,
                                      TRUE ~ 3)) %>%
            dplyr::mutate(index1 = ifelse(index1 == 3, NA, index1)) %>%
            tidyr::fill(index1) %>%
            dplyr::mutate(index1 = ifelse(is.na(index1), 0, index1)) %>%
            dplyr::mutate(id = LETTERS[replace(with(rle(index1),
                                             rep(cumsum(values), lengths)), index1 == 0, NA)]) %>%
            dplyr::group_by(id) %>%
            dplyr::summarise(sv = sum(cum)) %>%
            dplyr::filter(!is.na(id)) %>%
            dplyr::summarise(Sum = sum(sv)) %>%
            dplyr::select(Sum) %>%
            dplyr::rename(!!newname := Sum)
       if(nrow(out) == 0){
           out[1, 1] <- 0
       }
       return(out)
    }
        # Potential E2 index
    ff95 <- function(x){
        newname <- paste0("f95")
        out <- tibble(ff91(x) / ff24(x)) %>%
            dplyr::rename(!!newname := f91)
       if(nrow(out) == 0){
           out[1, 1] <- 0
       }
       return(out)
    }
        # % of probing time spent in 5
    ff119 <- function(x){
        newname <- paste0("f119")
        out <- tibble(ff95(x) * 100) %>%
            dplyr::rename(!!newname := f95)
       if(nrow(out) == 0){
           out[1, 1] <- 0
       }
       return(out)
    }
        # mean frequency of 12 during 5
    ff200 <- function(x){
        newname <- paste0("f200")
        x$cum <- c(diff(x$time), x$time[length(x$time)])
        out <- x %>%
            dplyr::mutate(index1 = dplyr::case_when(waveforms == 5 ~ 1,
                                      waveforms %in% c(2, 99) ~ 0,
                                      TRUE ~ 3)) %>%
            dplyr::mutate(index1 = ifelse(index1 == 3, NA, index1)) %>%
            tidyr::fill(index1) %>%
            dplyr::mutate(index1 = ifelse(is.na(index1), 0, index1)) %>%
            dplyr::mutate(id = LETTERS[replace(with(rle(index1),
                                             rep(cumsum(values), lengths)), index1 == 0, NA)]) %>%
            dplyr::count(id, waveforms) %>%
            dplyr::filter(!is.na(id), waveforms == 12) %>%
            dplyr::summarise(Sum = sum(n)) %>%
            dplyr::select(Sum) %>%
            dplyr::mutate(Sum = Sum / as.numeric(ff91(x))) %>%
            dplyr::rename(!!newname := Sum)
        if(nrow(out) == 0){
           out[1, 1] <- 0
       }
       return(out)
    }
        # % of time spent in 12 during 5, Matteo's attempt
    ff201 <- function(x){
      newname <- paste0("f201")
      x$cum <- c(diff(x$time), x$time[length(x$time)])
      out <- x %>%
        dplyr::mutate(index1 = dplyr::case_when(waveforms == 5 ~ 1,
                                                waveforms %in% c(2, 99) ~ 0,
                                                TRUE ~ 3)) %>%
        dplyr::mutate(index1 = ifelse(index1 == 3, NA, index1)) %>%
        tidyr::fill(index1) %>%
        dplyr::mutate(index1 = ifelse(is.na(index1), 0, index1)) %>%
        dplyr::mutate(id = LETTERS[replace(with(rle(index1),
                                                rep(cumsum(values), lengths)), index1 == 0, NA)]) %>% 
        dplyr::filter(!is.na(id)) %>% 
        dplyr::mutate(index2 = dplyr::case_when(waveforms == 12 ~ 1,
                                                waveforms == 5 ~ 0,
                                                TRUE ~ 3)) %>%
        dplyr::mutate(index2 = ifelse(index2 == 3, NA, index2)) %>%
        dplyr::mutate(index2 = ifelse(is.na(index2), 0, index2)) %>%
        dplyr::mutate(id2 = LETTERS[replace(with(rle(index2),
                                                 rep(cumsum(values), lengths)), index2 == 0, NA)]) %>% 
        dplyr::filter(!is.na(id2)) %>% 
        dplyr::group_by(index2) %>%
        dplyr::summarise(sv = sum(cum)) %>% 
        dplyr::mutate(Sum = sv / as.numeric(ff91(x))*100) %>% 
        dplyr::select(Sum) %>%
        dplyr::rename(!!newname := Sum)
      if(nrow(out) == 0){
        out[1, 1] <- 0
      }
      return(out)
    }
        # Total duration of nonphloematic phase
    ff98 <- function(x){
        newname <- paste0("f98")
        out <- dplyr::tibble(Sum = as.numeric(ff24(x)) - as.numeric(ff96(x))) %>%
            dplyr::rename(!!newname := Sum)
        if(nrow(out) == 0){
           out[1, 1] <- 0
       }
       return(out)
    }
        # % of probing time spent in E1
    ff119E <- function(x){
        newname <- paste0("f119E")
        out <- dplyr::tibble(Sum = (as.numeric(ff96(x)) / as.numeric(ff24(x))) * 100) %>%
            dplyr::rename(!!newname := Sum)
        if(nrow(out) == 0){
           out[1, 1] <- 0
       }
       return(out)
    }
        # Time from the beginning of E1 to the end of the EPG record
    ff107 <- function(x){
        newname <- paste0("f107")
        f4 <- x %>%
            dplyr::filter(waveforms == 4) %>%
            dplyr::slice(1) %>%
            dplyr::pull(time)
        out <- dplyr::tibble(Sum = (as.numeric(x %>% dplyr::slice(nrow(x)) %>% pull(time)) - 
                                    f4)) %>%
            dplyr::rename(!!newname := Sum)
        if(nrow(out) == 0){
           out[1, 1] <- 0
       }
       return(out)
    }
    # Time from 1st probe (="2") to 1st E2 ("=5")
    ff112 <- function(x){
      newname <- paste0("f112")
      x$cum <- c(diff(x$time), x$time[length(x$time)])
      tmp <- x %>%
        dplyr::filter(waveforms == 2) %>%
        dplyr::slice(1) %>% 
        dplyr::pull(time)
      tmpb <- x %>% 
        dplyr::filter(waveforms == 5) %>%
        dplyr::slice(1) %>% 
        dplyr::pull(time)
      out <- dplyr::tibble(res := tmpb - tmp) %>% 
        dplyr::rename(!!newname := res)
      if(nrow(out) == 0){
        out[1, 1] <- 0
      }
      return(out)
    }
    # Time from 1st probe (="2") to 1st sustained E2 (> 600 seconds)
    ff109 <- function(x){
      newname <- paste0("f109")
      x$cum <- c(diff(x$time), x$time[length(x$time)])
      tmp <- x %>%
        dplyr::filter(waveforms == 2) %>%
        dplyr::slice(1) %>% 
        dplyr::pull(time)
      tmpb <- x %>%
        dplyr::mutate(index1 = dplyr::case_when(waveforms == 5 ~ 1,
                                                waveforms %in% c(2, 99) ~ 0,
                                                TRUE ~ 3)) %>%
        dplyr::mutate(index1 = ifelse(index1 == 3, NA, index1)) %>%
        tidyr::fill(index1) %>%
        dplyr::mutate(index1 = ifelse(is.na(index1), 0, index1)) %>%
        dplyr::mutate(id = LETTERS[replace(with(rle(index1),
                                                rep(cumsum(values), lengths)), index1 == 0, NA)]) %>% 
        dplyr::filter(id == as.character(x %>%
                                           dplyr::mutate(index1 = dplyr::case_when(waveforms == 5 ~ 1,
                                                                                   waveforms %in% c(2, 99) ~ 0,
                                                                                   TRUE ~ 3)) %>%
                                           dplyr::mutate(index1 = ifelse(index1 == 3, NA, index1)) %>%
                                           tidyr::fill(index1) %>%
                                           dplyr::mutate(index1 = ifelse(is.na(index1), 0, index1)) %>%
                                           dplyr::mutate(id = LETTERS[replace(with(rle(index1),
                                                                                   rep(cumsum(values), lengths)), index1 == 0, NA)]) %>%
                                           dplyr::group_by(id) %>%
                                           dplyr::summarise(sv = sum(cum)) %>%
                                           dplyr::filter(!is.na(id),
                                                         sv >= 600) %>% 
                                           dplyr::select(id) %>% 
                                           dplyr::slice(1))) %>%
        dplyr::slice(1) %>% 
        dplyr::pull(time)
      out <- dplyr::tibble(res := tmpb - tmp) %>% 
        dplyr::rename(!!newname := res)
      if(nrow(out) == 0){
        out[1, 1] <- 0
      }
      return(out)
    }
    
    # waveform at 8 different hours
    ff1hour <- function(x, hour = 3600){
      newname <- paste0("f1hour_", hour)
      x$cum <- c(diff(x$time), x$time[length(x$time)])
      out <- x %>% 
        dplyr::filter(time <= hour) %>% 
        dplyr::slice_tail(1) %>% 
        dplyr::pull(waveforms)
      out <- dplyr::tibble(res := out) %>% 
        dplyr::rename(!!newname := res)
      if(nrow(out) == 0){
        out[1, 1] <- 0
      }
      return(out)
    }

    ###########################################################################
    # FUNCTION
    ## Intermediate table
    tmp <- x %>%
      dplyr::group_by(File) %>%
      tidyr::nest()
    ## Final table
    tmp %>%
      dplyr::mutate(f1 = purrr::map(data, ~ff1(.x, 1))) %>%
      dplyr::mutate(f2 = purrr::map(data, ~ff2(.x, 1))) %>%
      dplyr::mutate(f3 = purrr::map(data, ~ff3(.x, 1))) %>%
      dplyr::mutate(f14 = purrr::map(data, ~ff14(.x))) %>%
      dplyr::mutate(f24 = purrr::map(data, ~ff24(.x))) %>%
      dplyr::mutate(f29 = purrr::map(data, ~ff2(.x, 2))) %>%
      dplyr::mutate(f57 = purrr::map(data, ~ff1(.x, 7))) %>%
      dplyr::mutate(f58 = purrr::map(data, ~ff2(.x, 7))) %>%
      dplyr::mutate(f75 = purrr::map(data, ~ff1(.x, 4))) %>%
      dplyr::mutate(f78 = purrr::map(data, ~ff2(.x, 4))) %>%
      dplyr::mutate(f89 = purrr::map(data, ~ff89(.x))) %>%
      dplyr::mutate(f90 = purrr::map(data, ~ff90(.x))) %>%
      dplyr::mutate(f91 = purrr::map(data, ~ff91(.x))) %>%
      dplyr::mutate(f92 = purrr::map(data, ~ff92(.x))) %>%
      dplyr::mutate(f93 = purrr::map(data, ~ff93(.x))) %>%
      dplyr::mutate(f95 = purrr::map(data, ~ff95(.x))) %>%
      dplyr::mutate(f96 = purrr::map(data, ~ff96(.x))) %>%
      dplyr::mutate(f98 = purrr::map(data, ~ff98(.x))) %>%
      dplyr::mutate(f107 = purrr::map(data, ~ff107(.x))) %>%
      dplyr::mutate(f109 = purrr::map(data, ~ff109(.x))) %>%
      dplyr::mutate(f112 = purrr::map(data, ~ff112(.x))) %>%
      dplyr::mutate(f115 = purrr::map(data, ~ff115(.x, 2))) %>%
      dplyr::mutate(f117 = purrr::map(data, ~ff115(.x, 7))) %>%
      dplyr::mutate(f118 = purrr::map(data, ~ff115(.x, 4))) %>%
      dplyr::mutate(f119 = purrr::map(data, ~ff119(.x))) %>%
      dplyr::mutate(f119E = purrr::map(data, ~ff119E(.x))) %>%
      dplyr::mutate(f200 = purrr::map(data, ~ff200(.x))) %>%
      dplyr::mutate(f201 = purrr::map(data, ~ff201(.x))) %>%
      dplyr::mutate(f1hour = purrr::map(data, ~ff1hour(.x, 3600))) %>%
      dplyr::mutate(f2hour = purrr::map(data, ~ff1hour(.x, 7200))) %>%
      dplyr::mutate(f3hour = purrr::map(data, ~ff1hour(.x, 10800))) %>%
      dplyr::mutate(f4hour = purrr::map(data, ~ff1hour(.x, 14400))) %>%
      dplyr::mutate(f5hour = purrr::map(data, ~ff1hour(.x, 18000))) %>%
      dplyr::mutate(f6hour = purrr::map(data, ~ff1hour(.x, 21600))) %>%
      dplyr::mutate(f7hour = purrr::map(data, ~ff1hour(.x, 25200))) %>%
      dplyr::mutate(f8hour = purrr::map(data, ~ff1hour(.x, 28800))) %>%
      tidyr::unnest(f1:f201)
}

