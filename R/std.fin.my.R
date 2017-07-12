#' std.fin.my = standardized financials multiple years. Returns professional-grade historical financial data. This data is standardized, cleansed and verified to ensure the highest quality data sourced directly from the XBRL financial statements.
#'
#' @ticker a valid ticker symbol covered by intrinio.
#' @statement type of statement to retrieve. Options are "income_statement", "balance_sheet", "cash_flow_statement" or "calculations".
#' @period.quantity amount of periods to retrieve data back. The higher value alow by intrinio is 10.
#' @username Your user name in intrinio. You can find this information on your personal information on intrinio.
#' @password Your passoword of API key given by intrinio. You can find this information on your personal information on intrinio.
#' @return Returns a list as table with the tag name and the information for a given year.
#' @examples
#' data_avarage("AAPL", "cash_flow_statement", "FY", 3) #returns a the income statement for Apple Inc for the last 5 years
#'
#'

std.fin.my <- function(ticker = "AAPL",
                         statement = "income_statement",
                         period_type = "FY",
                         period.quantity = 5,
                         username = username,
                         password = password) {

        library(jsonlite); library(httr); library(reshape)
        base <- "https://api.intrinio.com/financials/standardized?identifier=" #called "endpoint" on the API document
        seq <- 0:(period.quantity-1)
        call <- paste(base, ticker, "&statement=", statement, "&type=", period_type,"&sequence=", sep  = "")
        call_list <- lapply(seq, function(x) {paste(call, x, sep = "")})
        lists <- lapply(call_list, function(y) {
                tp <- GET(y, authenticate(username, password, type = "basic"))
                z <- suppressMessages(unlist(content(tp, as = "text")))
                list <- fromJSON(z, flatten = FALSE)
                values <- list[[1]]
                #names(values) <- list[[1]][,1]
                values$period <- substr(y,nchar(y),nchar(y))
                values
        })
        #obtaining the fiscal years' details such as year or quarter
        base2 <- "https://api.intrinio.com/fundamentals/standardized?identifier="
        call2 <- paste(base2, ticker, "&statement=", statement, "&type=", period_type, sep ="")
        tp2 <- GET(call2, authenticate(username, password, type = "basic"))
        z2 <- suppressMessages(unlist(content(tp2, as = "text")))
        fp1 <- fromJSON(z2)
        fp <- paste(fp1[[1]][1:period.quantity,1],fp1[[1]][1:period.quantity,4], sep = "")

        #compilando la tabla
        table <- do.call(rbind, lists)
        table2 <- cast(table, tag ~ period)
        #row.names(table) <- names(lists[[1]])
        colnames(table2) <- c("tag", fp)
        table2
}
