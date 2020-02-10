#' parse xml node representing a prefecture
#' @param node xml node containing div.prefecture
parse_pref <- function(node){
  name <- node %>% xml_text()
  prid <- node %>%
    querySelector("input[name=prid]") %>%
    xml_attr("value") %>%
    as.integer()
  ret <- list(prid) %>% set_names(name)
  ret
}

#' get list of prefectures
get_pref_list <- function(){
  url <- "http://www.data.jma.go.jp/gmd/risk/obsdl/top/station"
  resp <- POST(url, body = list(pd = 0), encode = "form")
  body <- resp %>%
    content(type="text/html", encoding="utf-8")
  ret <- body %>%
    querySelectorAll("div.prefecture") %>%
    map(.f=parse_pref) %>%
    flatten()
  ret
}


#' which items are measured
#' @param item String; which items are measured
kansoku <- function(item){
  list(
    "rain" = str_sub(item, 1, 1) == "1",
    "wind" = str_sub(item, 2, 2) == "1",
    "temp" = str_sub(item, 3, 3) == "1",
    "sun" = str_sub(item, 4, 4) == "1",
    "snow" = str_sub(item, 5, 5) == "1"
  )
}



#' predicate function to determin whether a node has class "stmark"
#' @param node xml node containg div.station
is_stmark <- function(node){
  x <- node %>% xml_attr("class") %>% str_split(pattern = " ") %>% pluck(1)
  p <- "stmark" %in% x
  !p
}

#' parse xml node representing a station
#' @param node xml node containg div.station without div.stmark
parse_station <- function(node){
  title <- xml_attr(node, "title")
  stid <- node %>%
    querySelector("input[name=stid]") %>%
    xml_attr("value")
  stname <- node %>%
    querySelector("input[name=stname]") %>%
    xml_attr("value")
  prid <- node %>%
    querySelector("input[name=prid]") %>%
    xml_attr("value")
  kansoku <- node %>%
    querySelector("input[name=kansoku]") %>%
    xml_attr("value") %>%
    kansoku()
  ret <- list(list("title"=title,
                   "stid" = stid,
                   "prid" = prid,
                   "kansoku" = list(kansoku))) %>% set_names(stname)
  ret
}


#' get list of stations in a prefecture
#' @param pd String representing each prefecture
get_stations_of_pd <- function(pd){
  url <- "http://www.data.jma.go.jp/gmd/risk/obsdl/top/station"
  resp <- POST(url, body = list(pd = pd), encode = "form")
  body <- resp %>%
    content(type="text/html", encoding="utf-8")
  ret <- body %>%
    querySelectorAll("div.station") %>%
    keep(.p=is_stmark) %>%
    map(.f=parse_station) %>%
    flatten()
  ret
}

#' get station list
get_station_list <- function(){
  prefectures <- get_pref_list()
  stations <- map(prefectures, get_stations_of_pd)
  stations
}

