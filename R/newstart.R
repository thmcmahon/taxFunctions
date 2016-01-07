newstart <- function(income,
                     partner_income = 0,
                     brackets = c(102,252,Inf),
                     rate = c(0,.5,.6),
                     category = 
                       c("single, no children",
                         "single, with children",
                         "single, 60+ or 9 months continous payment",
                         "partnered",
                         "single principal carer granted exemption from MOR"))
  {
  # Todo
  #
  # This does not account 
  # 
  # Newstart has a number of different categories with different payment rates
  if (match.arg(category) == "single, no children") {
    amount <- 523.4
  } else if (match.arg(category) == "single, with children") {
    amount <- 566.3
  } else if (match.arg(category) == "single, 60+") {
    amount <- 566.3 
  } else if (match.arg(category) == "partnered") {
    amount <- 472.6
  } else if (match.arg(category) == "single principal carer") {
    amount <- 731.2
  }
  # Taper test
  if (match.arg(category != "partnered")) {
    income_by_bracket <- diff(c(0, pmin(income, brackets)))
    return(amount - sum(income_by_bracket * rate))
  } else {
    income_by_bracket <- diff(c(0, pmin(sum(income, partner_income), brackets)))
    return(amount - sum(income_by_bracket * rate))
  }
}