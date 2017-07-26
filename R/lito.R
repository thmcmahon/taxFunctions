#' Calculate the low income tax offset. This is deducted from non-refundable
#' tax offset that reduces individuals' tax owed.
#'
#' @param income Numeric. Gross income.
#' @param value Numeric. The full value of the offset.
#' @param taper_start Numeric. Income level from which to start tapering.
#'
#' @export
#'
#' @examples
#' lito(18000)
lito <- function(income, value = 445, taper_start = 37000,
                  taper_rate = .015) {
  if (income < taper_start) {
    return(value * -1)
  } else {
    tapered_lito <- (value - (income - taper_start) * taper_rate) * -1
    if (tapered_lito < 0) {
      return(tapered_lito)
    } else {
      return(0)
    }
  }
}
