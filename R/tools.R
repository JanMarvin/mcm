## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' extract quoted strings by ''
#' @param x character vector
#' @importFrom magrittr "%>%"
#' @importFrom stringi stri_extract_all_regex
#' @export
extr <- function(x) {
  x %>% gsub(pattern = "\\\\'", replacement = "", x = .) %>%
    stri_extract_all_regex(., "(?<=').*?(?=')") %>% unlist()
}

#' extract quoted strings by ()
#' @param x character vector
#' @importFrom magrittr "%>%"
#' @importFrom stringi stri_extract_all_regex
#' @export
extr2 <- function(x) {
  x %>% stri_extract_all_regex(., "(?<=\\().*?(?=\\))") %>% unlist()
}

#' convert content to prices
#' @param x character vector to convert to numeric
#' @importFrom magrittr "%>%"
#' @import rlang
#' @export
prc <- function(x) {

  # convert price to numeric
  is_price <- x %>% grepl(pattern = "\u20ac") # euro sign
  is_miss  <- x %>% grepl(pattern = "-") | x %>% grepl(pattern = "N/A")

  x[is_price] %<>% substr(., 1, nchar(.)-2)
  x[is_miss] <- NA

  x %<>%  sub(",", ".", ., fixed = TRUE) %>% as.numeric()

  x
}