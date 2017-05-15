#' HECS repayment calculator
#'
#' @param income HECS Repayment Income
#' @param debt Outstanding balance
#' @param brackets 
#' @param rates 
#'
#' @description 
#' \code{hecs} calculates the amount of HECS to be paid in a financial year
#' 
#' @details
#' HECS repayment income is different to your taxable income. It is calculated as:
#' \itemize{
#'   \item{your taxable income for an income year, plus}
#'   \item{your total net investment losses, plus}
#'   \item{any total reportable fringe benefit amounts shown on your PAYG payment summary; plus}
#'   \item{reportable super contributions; and}
#'   \item{any exempt foreign employment income from the current income year.}
#' }
#' 
#' @return The amount of HECS repayments in a financial year
#' @export
#'
#' @examples
#' 
hecs <- function(income, debt, 
                 brackets = c(0, 51957, 57730, 64307, 70882, 74608, 80198, 86856, 91426, 100614, 107214),
                 rates = c(0, .020, .040, .045, .050, .055, .060, .065, .070, .075, .080)) {
  stopifnot(length(rates) == length(brackets))
  repayment <- income * rates[findInterval(income, brackets)]
  if (repayment > debt) {
    debt
  } else {
    repayment
  }
}
