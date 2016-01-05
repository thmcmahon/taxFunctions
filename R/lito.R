#' Calculate the low income tax offset. This is deducted from non-refundable
#' tax offset that reduces individuals' tax owed.
#'
#' @param income Numeric. Gross income.
#' @param amount Numeric. The untapered low income tax offset amount.
#' @param taper_brackets Numeric vector.
#'
#' @examples
#' lito(18000)
lito <- function(income, amount = 445, taper_brackets = c(37000, 66667),
                 taper_rate = .015) {
    if (income > max(taper_brackets)) {
        lito <- 0
    } else if (income < min(taper_brackets)) {
        lito <- amount
    } else {
        lito <- amount - (income - min(taper_brackets)) * taper_rate
    }
    if (lito <= 0 & refundable == FALSE) {
        lito <- 0
    }
    return(lito)
}
