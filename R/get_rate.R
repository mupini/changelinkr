rr <- function(){
    page.url <- 'http://change-link.com/exchange-rates/currency-converter/'
    table.id <- '#ContentPlaceHolderDefault_SidebarZone_Item4_AllRates_8_RatesGridView'

    data.rows <- read_html(page.url) %>% html_node(css = table.id) %>% html_nodes('tr')
    data.rows <- data.rows[-1]
    tmpdf <- data.rows %>% lapply(function(curr.row) {
        td <- curr.row %>% html_nodes('td')
        data.frame(currency.name = td[2] %>% html_text, buy.val = td[3] %>% html_text, sell.val = td[4] %>% html_text, stringsAsFactors = F)
    }) %>% bind_rows
    tmpdf %>% return
}
getcurrency <- function(curr.name) rr() %>% filter(grepl(curr.name, currency.name, ignore.case = T))

get_rate <- function(curr) c('name:', 'buy:', 'sell:') %>% paste(getcurrency(curr) %>% c, sep='', collapse = ', ')

# getrate('united states dollar')
# getrate('rand')
