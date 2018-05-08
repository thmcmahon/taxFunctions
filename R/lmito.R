#' Low and Middle Income Tax Offset
#'
#' @param income Numeric. Gross income.
#' @param initial_value Numeric. The value of the first step of the offset.
#' @param increased_value Numeric. THe value of the second step of the offset.
#' @param taper_one_start Numeric. The point at which the offset tapers up.
#' @param taper_two_start Numeric. The point at which the offset tapers down.
#' @param taper_in_rate Numeric. The rate at which the offset tapers up.
#' @param taper_out_rate Numeric. The rate at which the offset tapers out.
#'
#' @export
#'
#' @examples
#' lmito(75000)
lmito <- function(income,
                  initial_value = 200, increased_value = 530,
                  taper_one_start = 37000, taper_two_start = 90000,
                  taper_in_rate = .03, taper_out_rate = .015) {
  if (income < taper_one_start) {
    return(initial_value * -1)
  } else if (income >= taper_one_start & income <= taper_two_start) {
    tapered_in_lmito <- (initial_value + (income - taper_one_start) * taper_in_rate)
    if (tapered_in_lmito < increased_value) {
      return(tapered_in_lmito * -1)
    } else {
      return(increased_value * -1)
    }
  } else if (income >= taper_two_start) {
    tapered_out_lmito <- (increased_value - (income - taper_two_start) * taper_out_rate) * -1
    if (tapered_out_lmito < 0) {
      return(tapered_out_lmito)
    } else {
      return(0)
    }
  } else {
    return(0)
  }
}