#' Calculate the low income tax offset. This is deducted from non-refundable tax
#' offset that reduces individuals' tax owed.
#'
#' @param income Numeric. Gross income.
#' @param value Numeric. The full value of the offset.
#' @param taper_start Numeric. Income level from which to start tapering.
#' @param taper_rate Numeric. Rate at which the offset tapers
#' @param second_taper_start Numeric. Income level at which the second taper
#'   starts
#' @param second_taper_rate Numeric. Rate at which the second taper starts.
#'
#' @export
#'
#' @examples
#' lito(18000)
lito <- function(income, value = 445, taper_start = 37000,
                 taper_rate = .015, second_taper_start = NA,
                 second_taper_rate = NA) {
  
  if (is.na(second_taper_start)) {
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
  } else {
    if (income < taper_start) {
      return(value * -1)
    } else if (income >= taper_start & income <= second_taper_start) {
      tapered_lito <- (value - (income - taper_start) * taper_rate) * -1
    } else if (income > second_taper_start) {
      first_tapered_value <- value - (value - (second_taper_start - taper_start) * taper_rate)
      tapered_lito <- ((value - first_tapered_value) - (income - second_taper_start) * second_taper_rate) * -1
    }
    if (tapered_lito < 0) {
      return(tapered_lito)
    } else {
      return(0)
    }
  }
}
