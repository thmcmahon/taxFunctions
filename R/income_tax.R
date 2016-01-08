#' Calculate income or superannuation tax liability. Defaults to 2012-13 tax
#' scales.
#' 
#' @param income Numeric. Gross income.
#' @param rates Numeric vector. Rate for each bracket.
#' @param brackets Numeric vector. Top of each tax bracket.
#' @param super Logical. Whether to calculate superannuation tax or income tax
#'   liability.
#' 
#' @examples 
#' # Calculate tax liability for taxpayer with gross income of 50,000
#' income_tax(50000)
income_tax <- function(income,
                       rates = c(0, .19, .325, .37, .45),
                       brackets = c(18200,37000,80000,180000,Inf),
                       super = FALSE) {
    tax <- mcalc(income, rates = rates, brackets = brackets, category = "tax")
    # If it's a superannuation tax then we need to get nine per cent
    ifelse(super == FALSE, return(tax), return(tax * .09))
}