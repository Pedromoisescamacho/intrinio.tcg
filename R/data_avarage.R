#' calculate the avarage change of different financial items 
#' 
#' @ticker a valid ticker symbol from Intrinio.
#' @period.quantity amount of periods to calculate it from.
#' @return Returns a list containing a table with the growth, average growth, tax, capex and divident rates
#' @examples
#' data_avarage("AAPL", 5, 1000000) #retures a list for Apple Inc
#' 


data_average <- function(ticker = "AAPL", period.quantity = 5) {
        source("datasheet.R", local = TRUE)
        #definiendo operadores especiales
        `%+%` <- function(e1, e2) {e1[is.na(e1)] <- 0; e2[is.na(e2)] <- 0; return(e1+ e2)} #definiendo operador que cambia NA por 0
        `%-%` <- function(e1, e2) {e1[is.na(e1)] <- 0; e2[is.na(e2)] <- 0; return(e1- e2)}
        #datasheet call
        table4 <- datasheet(ticker, period.quantity, 1000000)
        #sacando los datos para los calculos
        operatingrevenue <- table4["operatingrevenue",]
        totaloperatingincome <- table4["totaloperatingincome",]
        totalinterestincome <- table4["totalinterestincome",]
        netincome <- table4["netincome",]
        capex <- table4["capex",]
        dividends <- table4["paymentofdividends",]
        #tax, capex and dividend rate
        tax <- 1- (netincome/(totaloperatingincome %-% totalinterestincome)) ; rownames(tax) <- "tax_rate"
        capex_rate <- abs(capex) / operatingrevenue ; rownames(capex_rate) <- "capex_rate"
        dividend_rate <- abs(dividends) / netincome ; rownames(dividend_rate) <- "dividend_rate"
        #obtaining growth rates
        dif <- t(apply(table4,1,diff))
        base <- as.matrix(table4[,1:(ncol(table4)-1)])
        colnames(base) <- NULL
        rownames(base) <- NULL
        growth <- dif/base
        #obtaining mean of growth rate
        ave <- apply(growth, 1, mean)
        ave2 <- cbind(growth, average = ave)
        growth_ave <- list(Growth_rates = growth, ave_growth = ave, tax_rate = tax, capex_rate = capex_rate, dividend_rate = dividend_rate)
        growth_ave
}
