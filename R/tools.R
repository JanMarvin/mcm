## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' extract quoted strings by ''
#' @param x character vector
#' @importFrom magrittr "%>%"
#' @importFrom stringi stri_extract_all_regex
extr <- function(x) {
  x %>% gsub(pattern = "\\\\'", replacement = "", x = .) %>%
    stri_extract_all_regex(., "(?<=').*?(?=')") %>% unlist()
}

#' extract quoted strings by ()
#' @param x character vector
#' @importFrom magrittr "%>%"
#' @importFrom stringi stri_extract_all_regex
extr2 <- function(x) {
  x %>% stri_extract_all_regex(., "(?<=\\().*?(?=\\))") %>% unlist()
}

#' convert content to prices
#' @param x character vector to convert to numeric
#' @importFrom magrittr "%>%"
#' @import rlang
prc <- function(x) {

  # convert price to numeric # euro sign
  is_price <- x %>% grepl(pattern = "\u20ac", x = .) %>% sapply(., isTRUE) %>%
    unlist()
  is_miss  <- x %>% grepl(pattern = "-") | x %>% grepl(pattern = "N/A")

  # strsplit (some cards have price per unit)
  x[is_price] %<>% sapply(.,
                          FUN = function(x) strsplit(x, "\u20ac")[[1]][1]) %>%
    trimws()
  x[is_miss] <- NA

  x %<>%  sub(",", ".", ., fixed = TRUE) %>% as.numeric()

  x
}

#' check if page is loaded
#' @param x default remDr driver
#' @importFrom magrittr "%>%"
#' @importFrom xml2 read_html
pageloaded <- function(x = remDr) {
  x$getPageSource() %>%  unlist() %>% read_html() %>%
    grepl(pattern = "searchFor") %>% isTRUE()
}

#' extract script information from website
#' @param sc a javascript containing data
#' @importFrom magrittr "%>%"
extrsc <- function(sc) {

  # split on ;var to remove some nonsense
  sc <- strsplit(sc, ";var ") %>% unlist()

  # select the chartData part (should be two)
  sc <- sc[grepl("chartData =", sc)]

  # remove parentheses
  sc <- gsub(pattern = "\"", "", sc)

  # split to get the data parts
  sc <- strsplit(sc, ":\\[")[[1]]

  # remove everything after a closing bracket
  sc <- gsub("\\].*","",sc)[c(2,4,5)]


  # convert the data parts to vectors
  dates <- sc[1] %>% strsplit(., ",") %>% unlist() %>% as.Date("%d.%m.%y")
  reals <- sc[2] %>% strsplit(., ",") %>% unlist() %>% as.numeric()
  preds <- sc[3] %>% strsplit(., ",") %>% unlist() %>% as.numeric()

  # return
  sc <- data.frame(dates = dates, real = reals, pred = preds)

  sc
}
