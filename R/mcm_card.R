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
#' @importFrom xml2 read_html xml_missing
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
  p_dev <- list()

  pb <- txtProgressBar(..., style = 3)

  # get each card
  for (i in nums) {
    setTxtProgressBar(pb, i/max(nums))

    url <- paste0("https://www.cardmarket.com", x$url[i])

    # start while loop
    has_table <- xml_missing()

    # try to prevent html_table on xml_missing
    # most likely a wrongfully imported page. re-read the file. If this is not
    # enough, reading should be stopped at this variable
    while (is.na(has_table)) {
      page <- url %>% read_html()
      has_table <- page %>% html_node("table.availTable")
    }

    # car name
    name <- page %>% html_node(".active span") %>%  html_text()

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
    has_table <- page %>% html_node("table.availTable")


    avTable <- page %>% html_node("table.availTable") %>%
      html_table()
    avTable$X2 %<>% prc()

    rownames(avTable) <- avTable$X1
    avTable$X1 <- NULL
    avTable %<>% t()


    hasnoall <- page %>% html_node("#foil") %>% is.na()

    hasnofoil <- page %>% html_node("#foil") %>% is.na()

    sc_nonfoil <- sc_foil <- NA

    if (!hasnofoil) {
      sc_foil <- page %>% html_node("#foil script") %>%
        html_text() %>% extrsc()
    }

    if (!hasnoall) {
      sc_nonfoil <- page %>% html_node("#nonfoil script") %>%
        html_text() %>% extrsc()
    }

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

    p_dev[[name]] <- list(nonfoil = sc_nonfoil,
                          foil = sc_foil)

    pages[[i]] <- z
  }
  close(pb)

  # full information is not always available
  page <- bind_rows(pages)
  attr(page, "pdev")  <- p_dev

  page
}
