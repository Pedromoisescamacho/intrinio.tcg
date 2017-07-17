#`  company_info function returns general information about a particular company as presented by intrinio. The company has to be followed by intrinio
#'
#' @description This function only needs the username and API key since it is only a function as reference for you to know the avaiable companies to download information.
#' @apicredits for each function run there will be 1 credit used.
#'

company_info <- function(identifier = "AAPL") {
        library(jsonlite);library(httr); library(reshape)
        base <- "https://api.intrinio.com/companies?identifier="
        call <- paste0(base, identifier)
        #getting company information
        tp <- GET(call, authenticate(username, password, type = "basic"))
        z <- suppressMessages(unlist(content(tp, as = "text")))
        list <- suppressMessages(fromJSON(z))
        list
}
