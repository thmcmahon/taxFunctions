#' Make a function to calculate marginal income tax rates
#'
#' This closure returns functions that can be easily customised to test
#' different marginal income tax rates.
#'
#' @param rates Numeric vector. Personal income tax rates.
#' @param brackets Numeric vector. Personal income tax brackets.
#' @param super Logical. Whether this is a superannuation tax calculator.
#'
#' @return A function to calculate tax rates.
#'
#' @examples
#' # Make a tax calculator using the 2012-13 income tax rates and brackets
#' pit_12_13 <- make_tax_function(rates = c(0, .19, .325, .37, .45),
#'                                brackets = c(18200,37000,80000,180000,Inf))
make_tax_function <- function(rates, brackets, super = FALSE) {
    function(income) {
        income_by_bracket <- diff(c(0, pmin(income, brackets)))
        tax <- sum(income_by_bracket * rates)
        # If it's a superannuation tax then we need to get nine per cent
        ifelse(super == FALSE, return(tax), return(tax * .09))
    }
}