fundamentals <- function(ticket = "AAPL", 
                         statement = "income_statement", 
                         period_type = "FY",
                         period.quantity = 5) {
        
        library(jsonlite); library(httr); library(reshape)
        base <- "https://api.intrinio.com/financials/standardized?identifier="
        username <- "56e432e5f2755204fc7dca8dbea46c35" 
        password <- "6c2293b7d2772de2693255dffaea1011"
        seq <- 0:(period.quantity-1)
        call <- paste(base, ticket, "&statement=", statement, "&type=", period_type,"&sequence=", sep  = "")
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
        #sacando los periodos fiscales que se extrajeron
        base2 <- "https://api.intrinio.com/fundamentals/standardized?identifier="
        call2 <- paste(base2, ticket, "&statement=", statement, "&type=", period_type, sep ="")
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
