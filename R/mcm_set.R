#' searc mcm for card sets
#' @param name uniquely identifiing set name
#' @param lang not yet implemented
#' @examples
#' \dontrun{
#' # card <- mcm_set("Rivals")
#' # card
#' }
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom RSelenium rsDriver
#' @importFrom rvest html_node html_nodes html_text html_attr
#' @importFrom xml2 read_html
#' @export
mcm_set <- function(name, lang = "en") {
  eCaps <- list(chromeOptions = list(
    args = c('--headless', '--disable-gpu', '--window-size=1280,800')
  ))
  rD <- rsDriver(extraCapabilities = eCaps, check = FALSE, verbose = FALSE,
                 chromever = NULL, geckover = NULL,
                 phantomver = NULL, iedrver = NULL)

  # rD <- rsDriver()
  remDr <- rD[["client"]]

  remDr$navigate(paste0("https://www.cardmarket.com/", lang,
                        "/Magic/MainPage/advancedSearch"))

  webElem <- remDr$findElement(using = 'name', value = "idExpansion")

  webElem$sendKeysToElement(list(name, key = "enter"))

  webElem <- remDr$findElement(using = 'name', value = "cardName")
  webElem$sendKeysToElement(list(key = "enter"))
  pages <- list()

  done <- FALSE
  i    <- 0

  while (!done) {

    i <- i + 1


    url <- webElem$getCurrentUrl()
    file <- webElem$getPageSource()


    page <- file[[1]] %>% read_html()



    img <- page %>% html_nodes(css = "td:nth-child(1) .icon") %>%
      html_attr("onmouseover") %>% extr()
    erw_typ <- page %>% html_nodes(css = ".expansionIcon") %>%
      html_attr("onmouseover") %>%  extr()
    is_rare <- page %>% html_nodes(css = "td+ td .icon") %>%
      html_attr("onmouseover") %>% extr()
    engName <- page %>% html_nodes(css = ".col_3") %>%
      html_text()
    cardsAv <- page %>% html_nodes(css = ".col_4") %>%
      html_text() %>% as.integer()
    price   <- page %>% html_nodes(css = ".col_5") %>%
      html_text() %>% prc()

    url <- page %>% html_nodes(css = "td:nth-child(4) a") %>%
      html_attr("href")

    # is_rare = factor(x = is_rare,
    #                  labels = c("Common", "Land", "Mythic", "Rare", "Special",
    #                             "Token", "Tip Card", "Time Shifted", "Uncommon",
    #                             "Masterpiece"),
    #                  levels = seq(0,144,16))


    z <- data.frame(
      img = img,
      edition = erw_typ,
      rare = is_rare,
      en = engName,
      avail = cardsAv,
      price = price,
      url = url,
      stringsAsFactors = FALSE
    )

    pages[[i]] <- z


    nextpage <- FALSE
    nextpage   <- page %>% html_node(css = ".ml-5")  %>% as.character() %>%
      unique() %>% grepl(pattern = "Next page")
    nextpage %<>% isTRUE()



    if (nextpage) {
      remDr$findElement(using = "css", "span.icon.ml-5")$clickElement()
    } else {
      done <- TRUE
    }

  }


  remDr$quit()
  rm(rD)
  gc()


  page <- do.call("rbind", pages)

  page

}
