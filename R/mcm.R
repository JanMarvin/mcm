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


#' search mcm for cards
#' @param name name of card to look for
#' @param lang language en or de
#' @examples
#' \dontrun{
#' card <- mcm("Avatar der Entschlossenen")
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
        html_text()
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

      # create our own table

      # card information (double not required)
      is_rare <- page %>% html_node(".infoTableSingles .cell_0_1 .icon") %>%
        html_attr("onmouseover") %>% extr()

      numbers <- page %>% html_node(".infoTableSingles .cell_1_1") %>%
        html_text() %>% as.character()

      erw_typ <- page %>% html_nodes(css = ".infoTableSingles .expansionIcon") %>%
        html_attr("title") %>% extr2() %>% paste(collapse = ", ")

      # get price and foil information
      avTable <- page %>% html_node("table.availTable") %>% html_table()
      avTable$X2 %<>% prc()

      rownames(avTable) <- avTable$X1
      avTable$X1 <- NULL
      avTable %<>% t() %>% as.data.frame()

      price <- page %>%
        html_nodes(css = "#articleTableDividerRow+ tr .algn-r") %>%
        html_text() %>% prc()

      url <- url %>% unlist() %>%  gsub("https://www.cardmarket.com", "", x=.)

      img <- page %>% html_node("#prodImageId") %>% html_attr("src")

      z <- data.frame(
        img = img,
        edition = erw_typ,
        no = numbers,
        rare = is_rare,
        type = crd,
        avail = as.integer(avTable["Available items:"]),
        price = price,
        url = url,
        stringsAsFactors = FALSE
      )
      z <- cbind(z, name)

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


#' searc mcm for card details
#' @param x mcm() result
#' @param lang language "en" or "de"
#' @param ... for txtProgressBar
#' @examples
#' \dontrun{
#' # card <- mcm("Avatar der Entschlossenen")
#' # card
#'
#' val <- mcm_card(card)
#' val
#' }
#' @importFrom dplyr bind_rows
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom rvest html_node html_nodes html_text html_attr html_table
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom xml2 read_html
#' @export
mcm_card <- function(x, lang = "en", ...) {

  if (lang == "en") {
    tb_rulesText <- "#rulesText1"
  }
  if (lang == "de") {
    tb_rulesText <- "#rulesText3"
  }


  nums <- seq_along(x$url)

  pages <- list()

  pb <- txtProgressBar(..., style = 3)

  # get each card
  for (i in nums) {
    setTxtProgressBar(pb, i/max(nums))

    url <- paste0("https://www.cardmarket.com", x$url[i])

    page <- url %>% read_html()

    # card information (double not required)
    is_rare <- page %>% html_node(".infoTableSingles .cell_0_1 .icon") %>%
      html_attr("onmouseover") %>% extr()

    # set search provides no numbers
    if(is.null(x$no))
      x$no <- NA

    if (!is.na(x$no[i])) {
      numbers <- page %>% html_node(".infoTableSingles .cell_1_1") %>%
        html_text() %>% as.character()
    } else {
      numbers <- NA
    }

    erw_typ <- page %>% html_nodes(css = ".infoTableSingles .expansionIcon") %>%
      html_attr("title") %>% extr2() %>% paste(collapse = ", ")

    # get price and foil information
    avTable <- page %>% html_node("table.availTable") %>% html_table()
    avTable$X2 %<>% prc()

    rownames(avTable) <- avTable$X1
    avTable$X1 <- NULL
    avTable %<>% t()



    # rules
    rules <- page %>% html_node(tb_rulesText) %>% html_text()

    # prepare output
    z <- data.frame(
      url = x$url[i],
      is_rare,
      numbers,
      editions = erw_typ,
      avTable,
      rules,
      stringsAsFactors = FALSE
    )

    pages[[i]] <- z
  }
  close(pb)

  # full information is not always available
  page <- bind_rows(pages)

  page
}


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
      html_text()
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
