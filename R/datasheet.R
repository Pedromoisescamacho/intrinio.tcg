datasheet <- function(ticket = "AAPL", period.quantity = 5, unit = 1000000) {
        suppressMessages(library(dplyr))
        #defining the tags we want to select
        income_tags <- c("operatingrevenue", "totaloperatingincome", "totalinterestexpense", "netincome")
        cf_tags <- c("depreciationexpense", "netcashfromoperatingactivities", "paymentofdividends")
        cal_tags <- "capex"
        #poniendo los tags como data frame para entonces poder hacer el join bien
        income_tags <- income_tags %>% as.data.frame(); names(income_tags) <- c("tag")
        cf_tags <- cf_tags %>% as.data.frame(); names(cf_tags) <- c("tag")
        cal_tags <- cal_tags %>% as.data.frame(); names(cal_tags) <- c("tag")
        #loading the function fundamentals to retrieve the financial information
        source("fundamentals.R", local = TRUE)
        #income data
        income <- fundamentals(ticket, statement = "income_statement", period_type = "FY", period.quantity = period.quantity)
        incomef <- suppressMessages(left_join(income_tags,income))
        #cash flow data
        cf <- fundamentals(ticket, statement = "cash_flow_statement", period_type = "FY", period.quantity = period.quantity)
        cff <- suppressMessages(left_join(cf_tags, cf))
        #capex (we have to download all the calculations anyway)
        cal <- fundamentals(ticket, statement = "calculations", period_type = "FY", period.quantity = period.quantity)
        calf <- suppressMessages(left_join(cal_tags, cal))
        #puting together the previous tables
        table <- rbind(incomef,cff,calf)
        table2 <- sapply(table[,2:(period.quantity+1)], as.numeric)
        table3 <- as.data.frame(table2, table[,1])
        table3[period.quantity:1]/unit
}
