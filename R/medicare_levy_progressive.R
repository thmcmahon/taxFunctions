#' Progessive medicare levy
#'
#' @param x Income
#' @param cutoff Where does the progressive step kick in
#' @param base_rate Normal rate
#' @param increase_rate Extra rate
#' @param ... Additional parameters to medicare levy function
#'
#' @return Progressive medicare levy owed in dollars
#' @export
#'
#' @examples
#' ml_progressive(50000)
ml_progressive <- function(x, cutoff = 87000, base_rate = .02,
                           increase_rate = .025, ...) {
  if (x <= cutoff) {
    medicare_levy(x, rate = base_rate, ...)
  } else if (x > cutoff) {
    medicare_levy(x, rate = increase_rate, ...)
  } else {
    stop("A cutoff and income must be specified.")
  }
}