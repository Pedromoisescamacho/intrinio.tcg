#`  companies i.e. Returns information for all companies covered by Intrinio. the class returned is a list composed of a table and a integer. The former is the information of all the companies and the latter is the api_credits consumed by the function. Usefull to know all the companies that we have information from.
#'
#' @description This function only needs the username and API key since it is only a function as reference for you to know the avaiable companies to download information.
#'
#'

companies <- function(api_credits = FALSE) {
        library(jsonlite);library(httr); library(reshape)
        base <- "https://api.intrinio.com/companies"
        #getting the first page of the call
        tp <- GET(base, authenticate(username, password, type = "basic"))
        z <- suppressMessages(unlist(content(tp, as = "text")))
        list <- suppressMessages(fromJSON(z))
        #creating the rest of the calls for the rest of the pages.
        pages <- 2:list$total_pages
        calls <- sapply(pages, function(x) {paste0(base,"?page_number=",x)})
        #makin the calls for the rest of the values
        df <- data.frame()
        table_list <- lapply(calls, function(y) {
                tp2 <- GET(y, authenticate(username, password, type = "basic"))
                z2 <- suppressMessages(unlist(content(tp2, as = "text")))
                table <- suppressMessages(fromJSON(z2))[[1]]
        })
        #putting all the tables together
         finaltable<- rbind(list[[1]],do.call(rbind, table_list))

        if (api_credits == TRUE) {
                result <- list(table = finaltable, apicredits = list$total_pages)
                }
        else print(finaltable)
}
