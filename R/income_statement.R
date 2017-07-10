income_statement <- function(ticket = "AAPL",  
                         period_type = "FY",
                         period.quantity = 5) {
        
        library(jsonlite); library(httr); 
        base <- "https://api.intrinio.com/financials/standardized?identifier="
        username <- "56e432e5f2755204fc7dca8dbea46c35" 
        password <- "6c2293b7d2772de2693255dffaea1011"
        income_tags <- c("operatingrevenue", "totaloperatingincome", "totalinterestincome", "netincome")
        seq <- 0:(period.quantity-1)
        call <- paste(base, ticket, "&statement=income_statement", "&type=", period_type,"&sequence=", sep  = "")
        call_list <- lapply(seq, function(x) {paste(call, x, sep = "")})
        lists <- lapply(call_list, function(y) {
                tp <- GET(y, authenticate(username, password, type = "basic"))
                z <- suppressMessages(unlist(content(tp, as = "text")))
                list <- fromJSON(z, flatten = FALSE)
                values <- list[[1]]
                #names(values) <- list[[1]][,1]
                values2 <- values[values$tag %in% income_tags,]
                values2
        })
        #sacando los periodos fiscales que se extrajeron
        base2 <- "https://api.intrinio.com/fundamentals/standardized?identifier="
        call2 <- paste(base2, ticket, "&statement=income_statement", "&type=", period_type, sep ="")
        tp2 <- GET(call2, authenticate(username, password, type = "basic"))
        z2 <- suppressMessages(unlist(content(tp2, as = "text")))
        fp1 <- fromJSON(z2)
        fp <- paste(fp1[[1]][1:period.quantity,1],fp1[[1]][1:period.quantity,4], sep = "")
        
        #compilando la tabla
        table <- as.data.frame(matrix(unlist(lists), length(lists[[1]])))
        row.names(table) <- names(lists[[1]])
        colnames(table) <- fp
        #table
        lists
}
