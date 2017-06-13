rr <- function(){
    page.url <- 'http://change-link.com/exchange-rates/currency-converter/'
    table.id <- '#ContentPlaceHolderDefault_SidebarZone_Item4_AllRates_8_RatesGridView'

    data.table <- rvest::html_node(xml2::read_html(page.url), css = table.id)
    data.rows <- rvest::html_nodes(data.table, 'tr')

    data.rows <- data.rows[-1]
    tmpdf <-  dplyr::bind_rows(lapply(data.rows, function(curr.row) {
        td <-  html_nodes(curr.row, 'td')
        data.frame(currency.name = rvest::html_text(td[2]),
                   buy.val = rvest::html_text(td[3]),
                   sell.val = rvest::html_text(td[4]),
                   stringsAsFactors = F)
    }))
    return(tmpdf)
}
getcurrency <- function(curr.name)  dplyr::filter(rr(), grepl(curr.name, currency.name, ignore.case = T))

get_rate <- function(curr) c(paste(c('name:', 'buy:', 'sell:'), getcurrency(curr)), sep='', collapse = ', ')

# getrate('united states dollar')
# getrate('rand')
