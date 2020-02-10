#' split interval by 1 month
#' @param from Date
#' @param to Date
split_interval <- function(from, to){
  d1 <- floor_date(from, unit="month")
  d2 <- ceiling_date(to, unit="month")
  x <- seq.Date(d1, d2, "1 month")
  ret <- seq_len(length(x) - 1) %>%
    purrr::map(function(i){
      interval(x[i], x[i+1])
    })
  ret
}

#' wait for given time
#' @param sec time in second
wait <- function(sec){
  pb <- progress_bar$new(format = "(:spin) waiting [:bar] :elapsed", total = sec + 1, clear = FALSE, show_after = 0)
  for (i in 1:(sec + 1)) {
    pb$tick()
    Sys.sleep(1)
  }
}

#' save internal data
save_sysdata <- function(){
  stations <- get_station_list()
  usethis::use_data(stations, internal = TRUE)
}
