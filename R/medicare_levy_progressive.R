#' Check whether an income should be tapered
#'
#' @param x Income
#' @param lower_bound Level of the lower bound
#' @param rate Medicare Levy Rate
#' @param taper_rate Low income taper rate
#'
#' @return Logical. Whether the income is below the maximum taper threshold
check_upper_bound <- function(x, lower_bound, rate = .02, taper_rate = .1) {
  tapered_levy <- (x - lower_bound) * taper_rate
  standard_levy <- x * rate
  if (tapered_levy > standard_levy) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Calculate the amount of medicare levy owed
#'
#' @param x Income of the person for whom the levy is being calulated
#' @param rate Medicare levy rate
#' @param taper_rate How fast should the medicare levy low income reduction taper out
#' @param lower_bound_single The start of the taper out of the low income reduction (single)
#' @param lower_bound_couple The start of the taper out of the low income reduction (family)
#' @param spouse_income Income of the person's spouse
#' @param n_children Number of children
#' @param children_reduction How much should the lower_bound_couple increase by for each child
#' @param rel_status Relationship status, must either be 'single' or 'couple'
#'
#' @return Amount of medicare levy owed
#' @export
#'
#' @examples
#' medicare_levy(100000)
#' medicare_levy(44000, spouse_income = 0, rel_status = 'couple', lower_bound_single = 20896,
#'               lower_bound,couple = 35261, children_reduction = 3238)
medicare_levy <- function(x, rate = .02, taper_rate = .1, lower_bound_single = 21335,
                          lower_bound_couple = 36001, spouse_income = 0, n_children = 0,
                          children_reduction = 3306, rel_status = 'single') {
# Error checking -----------------------------------------------------------------------------------
  if (!is.numeric(x)) stop('x must be numeric')
  if (x < 0) stop('x must be a non-negative number')
  if (!(rel_status == 'single' | rel_status == 'couple')) {
    stop('rel_status must either be single or couple')
  }
  if (rel_status == 'single' & (spouse_income > 0 | n_children > 0)) {
    # If there is a spouse with income or children, then it must be a couple
    rel_status <- 'couple'
  }
# Singles ------------------------------------------------------------------------------------------
  x_above_upper_bound <- check_upper_bound(x, lower_bound = lower_bound_single)
  tapered_medicare_levy <- (x - lower_bound_single) * taper_rate


# Couples ------------------------------------------------------------------------------------------
  if (rel_status == 'couple') {
    hh_income <- x + spouse_income
    lower_bound_couple <- lower_bound_couple + (children_reduction * n_children)
    hh_above_upper_bound <- check_upper_bound(hh_income, lower_bound = lower_bound_couple)
    reduction_amount <- (lower_bound_couple * .02) - ((hh_income - lower_bound_couple) * .08)
    # If the household income is above the tapered family low income reduction then the standard
    # medicare levy applies
    if (hh_above_upper_bound == FALSE) {
      spouse_above_upper_bound <- check_upper_bound(spouse_income, lower_bound = lower_bound_single)
      x_share_red_amount <- (x / hh_income) * reduction_amount
      if (x > lower_bound_single & spouse_income < lower_bound_single) {
        # If the primary earns over the single taper threshold, but their spouse does not, the
        # spouse pays no levy, the primary pays the levy otherwise payable reduced by a 'reduction
        # amount'. This seems completely arbritrary.
        tapered_medicare_levy <- (x * rate) - reduction_amount
      } else if (x_above_upper_bound == TRUE & spouse_above_upper_bound == TRUE) {
        # If both the primary and their spouse earn greater than the individual taper thresholds but
        # are still under the household threshold, the amount payable is the share of the proportion
        # of income over the threshold
        x_share <- (x / hh_income) * (hh_income - lower_bound_couple)
        tapered_medicare_levy <- x_share * taper_rate
      } else if (x_above_upper_bound == TRUE &
                 (spouse_income > lower_bound_single & spouse_above_upper_bound == FALSE)) {
        # These next two do the same thing, but for the situation where the primary is over the
        # single income threshold, the other for the contrary.
        #
        # In this case the reduction amount is apportioned on the basis of each spouse's
        # contribution to the family income. If the reduction amount as apportioned exceeds the levy
        # otherwise payable by one spouse, the excess goes in reduction of the levy payable by the
        # other.
        spouse_share_red_amount <- (spouse_income / hh_income) * reduction_amount
        spouse_surplus_red_amount <- (spouse_income - lower_bound_single) * taper_rate -
                                      spouse_share_red_amount
        tapered_medicare_levy <- (x * rate) - (x_share_red_amount - spouse_surplus_red_amount)
      } else if (spouse_above_upper_bound == TRUE &
                 (x > lower_bound_single & x_above_upper_bound == FALSE)) {
        tapered_medicare_levy <- (x - lower_bound_single) * taper_rate + x_share_red_amount
      }
    }
  }

# Final calcs --------------------------------------------------------------------------------------
  if (x < lower_bound_single) {
    return(0)
  } else if (tapered_medicare_levy < 0) {
    return(0)
  } else if (tapered_medicare_levy < (x * rate)) {
    return(tapered_medicare_levy)
  } else {
    return(round(x * rate, 1))
  }
}
