#' generate body for POST
#'
read_jma_body <- function(from, to, phpsessid){
  list(
    "stationNumList" = "[\"a0593\"]",
    "aggrgPeriod" = 9,
    "elementNumList" = "[[\"201\",\"\"],[\"101\",\"\"]]",
    "interAnnualFlag" = 1,
    "ymdList" = str_glue("[\"{year(from)}\",\"{year(to)}\",\"{month(from)}\",\"{month(to)}\",\"{day(from)}\",\"{day(from)}\"]"),
    "optionNumList" = "[]",
    "downloadFlag" = "true",
    "rmkFlag" = 1,
    "disconnectFlag" = 1,
    "youbiFlag"= 0,
    "fukenFlag" = 0,
    "kijiFlag" = 0,
    "huukouFlag" = 0,
    "csvFlag" = 1,
    "jikantaiFlag" = 0,
    "jikantaiList" = "[1,24]",
    "ymdLiteral" = 1,
    "PHPSESSID" = phpsessid
  )
}

#' read csv data from jma web site
#' @param from Date
#' @param to Date
#' @param phpsessid php session id obtained by `get_phpsessionid()`
#'@return csv data (character), can be processed with `readr::read_csv()`
#' @examples
#' library("lubridate")
#' library("readr")
#'
#' phpsessid <- get_phpsessionid()
#'
#' # Site: Fukuchiyama, Kyoto
#' # Interval: from 2018-01-01 to 2018-02-01
#' # Data: Temperature and Precipitation
#'
#' # lubridate::ymd()
#' f <- read_jma(ymd("2018-01-01"), ymd("2018-02-01"), phpsessid)
#'
#' # parse csv using readr
#' col_names <- c("Datetime", "Temp", "Temp_exists",
#' "Temp_quality", "Rain", "Rain_exists", "Rain_qality")
#' col_types <- cols(col_datetime("%Y/%m/%d %H:%M:%S"),
#' col_double(), col_integer(), col_integer(),
#' col_double(), col_integer(), col_integer())
#' d <- read_csv(f, col_names=col_names, col_types=, skip=5)
#' head(d)
#'
#' @export
read_jma <- function(from, to, phpsessid){
  url <- "http://www.data.jma.go.jp/gmd/risk/obsdl/show/table"
  body <- read_jma_body(from, to, phpsessid)
  resp <- httr::POST(url, body = body, encode = "form")
  csv <- resp %>%
    content(as = "text", encoding="SJIS")
  csv
}
