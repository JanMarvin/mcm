#' search mcm for cards
#' @param name name of card to look for
#' @param lang language en or de
#' @examples
#' \dontrun{
#' card <- mcm("Black Lotus")
#' card
#'
#' card <- mcm("Avatar der Entschlossenen", lang = "de")
#' card
#' }
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom RSelenium rsDriver
#' @importFrom rvest html_node html_nodes html_text html_attr html_table
#' @importFrom xml2 read_html
#' @export
mcm <- function(name, lang = "en") {
  eCaps <- list(chromeOptions = list(
    args = c('--headless', '--disable-gpu', '--window-size=1280,800')
  ))
  rD <- rsDriver(extraCapabilities = eCaps, check = FALSE, verbose = FALSE,
                 chromever = NULL, geckover = NULL,
                 phantomver = NULL, iedrver = NULL)
  remDr <- rD[["client"]]

  remDr$navigate(paste0("https://www.cardmarket.com/", lang, "/Magic"))

  webElem <- remDr$findElement(using = 'name', value = "searchFor")

  webElem$sendKeysToElement(list(name, key = "enter"))

  pages <- list()

  done <- FALSE
  i    <- 0

  while (!done) {

    i <- i + 1

    url <- webElem$getCurrentUrl()
    file <- webElem$getPageSource()

    page <- file[[1]] %>% read_html()

    if (lang == "en") {
      td_engName <- "td:nth-child(5) a"
      td_valType <- "td:nth-child(6) a"
      td_cardsAv <- "td:nth-child(7)"
      td_price   <- "td:nth-child(8)"
      crd <- "Singles"
      np  <- "Next page"
    }
    if (lang == "de") {
      td_gerName <- "td:nth-child(5) a"
      td_engName <- "td:nth-child(6) a"
      td_valType <- "td:nth-child(7) a"
      td_cardsAv <- "td:nth-child(8)"
      td_price   <- "td:nth-child(9)"
      crd <- "Einzelkarten"
      np  <- "N\u00E4chste Seite"
    }


    good <- url %>% unlist() %>% grepl("showSearchResult", x = .) %>%  isTRUE()

    if (good) {

      img <- page %>% html_nodes(css = "td:nth-child(1) .icon") %>%
        html_attr("onmouseover") %>% extr()
      erw_typ <- page %>% html_nodes(css = ".expansionIcon") %>%
        html_attr("onmouseover") %>%  extr()
      numbers <- page %>% html_nodes(css = "td:nth-child(3)") %>%
        html_text() %>% as.character()
      is_rare <- page %>% html_nodes(css = "td+ td .icon") %>%
        html_attr("onmouseover") %>% extr()
      if (lang == "de") {
        gerName <- page %>% html_nodes(css = td_gerName) %>%
          html_text()
      }
      engName <- page %>% html_nodes(css = td_engName) %>%
        html_text()
      valType <- page %>% html_nodes(css = td_valType) %>%
        html_text()
      cardsAv <- page %>% html_nodes(css = td_cardsAv) %>%
        html_text() %>% as.integer()
      price   <- page %>% html_nodes(css = td_price) %>%
        html_text() %>% prc()

      url <- page %>% html_nodes(css = td_engName) %>%
        html_attr("href")


      # is_rare = factor(x = is_rare,
      #                  labels = c("Common", "Land", "Mythic", "Rare", "Special",
      #                             "Token", "Tip Card", "Time Shifted", "Uncommon",
      #                             "Masterpiece"),
      #                  levels = seq(0,144,16))


      is_card <- valType == crd

      if (lang == "de") {
        name <- data.frame(
          de = gerName[is_card],
          en = engName[is_card],
          stringsAsFactors = FALSE)
      }

      if (lang == "en") {
        name <- data.frame(
          en = engName[is_card],
          stringsAsFactors = FALSE)
      }

      z <- data.frame(
        img = img[is_card],
        edition = erw_typ[is_card],
        no = numbers[is_card],
        rare = is_rare[is_card],
        type = valType[is_card],
        avail = cardsAv[is_card],
        price = price[is_card],
        url = url[is_card],
        stringsAsFactors = FALSE
      )
      z <- cbind(z, name)

      pages[[i]] <- z

    } else { # end good

      # search table put us to a card instead of a search result table. to be
      # able to use mcm_card for mcm_vec output, we have to build our own
      # table

      name <- page %>% html_node(".active span") %>%  html_text()

      # card information (double not required)
      is_rare <- page %>% html_node(card_is_rare) %>%
        html_attr("onmouseover") %>% extr()

      numbers <- page %>% html_node(card_numbers) %>%
        html_text() %>% as.character()

      erw_typ <- page %>% html_nodes(card_erw_typ) %>%
        html_attr("title") %>% extr2() %>% paste(collapse = ", ")

      # get price and foil information
      avTable <- page %>% html_node(card_avTable) %>% html_table()
      avTable$X2 %<>% prc()

      rownames(avTable) <- avTable$X1
      avTable$X1 <- NULL
      avTable %<>% t() %>% as.data.frame()

      price <- page %>%
        html_nodes(css = card_first_price) %>%
        html_text() %>% prc()

      url <- url %>% unlist() %>%  gsub("https://www.cardmarket.com", "", x=.)

      img <- page %>% html_node(card_img) %>% html_attr("src")

      z <- data.frame(
        img = img,
        edition = erw_typ,
        no = numbers,
        rare = is_rare,
        type = crd,
        avail = as.integer(avTable["Available items:"]),
        price = price,
        url = url,
        name = name,
        stringsAsFactors = FALSE
      )
      nam <- names(z)
      nam[nam == "name"] <- lang
      names(z)  <- nam

      pages[[i]] <- z

    }


    nextpage <- FALSE
    nextpage   <- page %>% html_node(css = ".ml-5")  %>% as.character() %>%
      unique() %>% grepl(pattern = np)
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
