#' get php sessoion id
#'
#' @export
get_phpsessionid <- function(){
  url <- "http://www.data.jma.go.jp/gmd/risk/obsdl/index.php"
  resp <- httr::GET(url)
  sessionid <- resp %>%
    pluck("content") %>%
    xml2::read_html() %>%
    selectr::querySelector("input#sid") %>%
    xml2::xml_attr("value")
  sessionid
}
