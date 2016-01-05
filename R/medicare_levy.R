#' Calculate the medicare levy owed.
#'
#' The medicare levy is not a marginal tax, it's a flat two per cent tax
#' with exemptions for low income earners. If you're below the lowest
#' 'shading bracket' then you're exempt from the medicare levy, between the
#' lowest shading bracket and the top shading bracket the levy is a 10 %
#' marginal tax of income over the bottom shading bracket. Once you get over
#' the top shading bracket it becomes a flat 2 per cent tax.
#'
#' @param income Numeric. Gross income.
#' @param levy_rate Numeric. Rate at which the medicare levy is paid.
#' @param shading_brackets Numeric vector. Income level at which the
#'   medicare levy is 'shaded' or phased in.
#' @param shading_rates Numeric vector. The rates at which the medicare levy
#'   is 'shaded' or phased in.
#'
#' @return Numeric, the amount of medicare levy owed
#'
#' @examples
#' medicare_levy(20000)
medicare_levy <- function(income, levy_rate = .02,
                          shading_brackets = c(20896,26121),
                          shading_rates = c(0,.1)) {

    if (income > max(shading_brackets)) {
        # No low income exemption = flat tax
        levy <- income * levy_rate
    } else {
        # If you're below the partial threshold then income up to the bottom
        # threshold is exempt and income above the bottom threshold is taxed at
        # 10 per cent.
        income_by_bracket <- diff(c(0, pmin(income, shading_brackets)))
        levy <- sum(income_by_bracket * shading_rates)
    }
    return(levy)
}