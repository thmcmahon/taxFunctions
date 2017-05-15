medicare_levy_surcharge <- function(mls_income, output_income, spouse_income = 0, rel_status = "single",
                                    bracket_single = c(0, 90000, 105000, 140000, Inf),
                                    bracket_family = c(0, 180000, 210000, 280000, Inf),
                                    rates = c(0, .01, .0125, .015)) {
  if (rel_status == "single") {
    output_income * rates[findInterval(mls_income, bracket_single)]
  }
}

