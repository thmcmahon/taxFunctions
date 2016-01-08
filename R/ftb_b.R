ftb_b <- function(incomes, youngest_child_age,
                  youngest_child_student = TRUE,
                  taper_brackets = c(5402,Inf),
                  taper_rates = c(0, .2)) {
  # If the highest income earner earns more than 100k then you're ineligible
  if (length(incomes) > 2) {
    stop("Maximum of two incomes for each parent. Polygamy is illegal.")
  }
  if (max(incomes) > 100000) {
    return(0)
  } else {
    # FTB eligibility depends on age of youngest child and whether they're
    # going to school
    if (youngest_child_age < 5) {
      amount <- 4339.85
    } else if (youngest_child_age >= 5 && youngest_child_age <= 15) {
      amount <- 3139
    } else if (youngest_child_age > 15 && youngest_child_age <= 18 &&
               youngest_child_student == TRUE) {
      amount <- 3139
    } else {
      return(0)
    }
    # Marginal calculation is based on the lower of the two incomes
    mcalc(min(incomes), benefit = amount, brackets = taper_brackets,
          rates = taper_rates, category = "transfer")
  }
}
  