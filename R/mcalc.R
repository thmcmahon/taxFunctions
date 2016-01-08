mcalc <- function(income, benefit = 0, brackets, rates,
                  category = c("tax", "transfer")) {
  # Calculate a marginal tax that increases or benefit that tapers
  income_by_bracket <- diff(c(0, pmin(income, brackets)))
  if (match.arg(category) == "tax") {
    return(sum(income_by_bracket * rates))
  } else if (match.arg(category) == "transfer") {
    return(benefit - sum(income_by_bracket * rates))
  }
}