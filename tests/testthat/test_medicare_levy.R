test_that("medicare levy calculates correct amounts for singles", {
  expect_equal(medicare_levy(0), 0)
  expect_equal(medicare_levy(100000), 2000)
  expect_equal(medicare_levy(22000), 66.5)
  expect_equal(medicare_levy(21335), 0)
  expect_equal(medicare_levy(21336), .1)
  expect_equal(medicare_levy(26668), 533.3)
  expect_equal(medicare_levy(26669), 533.4)
  expect_error(medicare_levy(-1), 'x must be a non-negative number')
  expect_error(medicare_levy('1'), 'x must be numeric')
})

test_that("medicare levy calculates correct amounts for couples", {
  ml_broken_calc <- function(x, spouse_income = 0, n_children = 0, rel_status = 'couple') {
    # For some reason the ATO calculator uses the wrong thresholds. Well played ATO.
    medicare_levy(x = x, spouse_income = spouse_income, n_children = n_children,
                  rel_status = rel_status, lower_bound_couple = 36000,
                  lower_bound_single = 21335, children_reduction = 3306)
  }
  expect_equal(ml_broken_calc(50000), 1000, tolerance = .1)
  expect_equal(ml_broken_calc(37000), 99.90, tolerance = .1)
  expect_equal(ml_broken_calc(36000), 0, tolerance = .1)
  expect_equal(ml_broken_calc(0, spouse_income = 60000), 0)
  expect_equal(ml_broken_calc(36002), .1, tolerance = .1)
  expect_equal(ml_broken_calc(20000, spouse_income = 16002), 0)
  expect_equal(ml_broken_calc(28900, spouse_income = 9000), 9.9, tolerance = .1)
  expect_equal(ml_broken_calc(20000, spouse_income = 50000), 0)
  expect_equal(ml_broken_calc(40000), 399.9, tolerance = .1)
  expect_equal(ml_broken_calc(23000, spouse_income = 28000), 166.5, tolerance = .1)
  expect_equal(ml_broken_calc(24000, spouse_income = 28000), 266.5, tolerance = .1)
  expect_equal(ml_broken_calc(24000, spouse_income = 20000), 186.4, tolerance = .1)
})

test_that("master tax guide tests pass", {
  ml_14_15 <- function(x, spouse_income = 0, n_children = 0, rel_status = 'single') {
    medicare_levy(x = x, spouse_income = spouse_income, n_children = n_children,
                  rel_status = rel_status, lower_bound_single = 20896, lower_bound_couple = 35261,
                  children_reduction = 3238)
  }
  expect_equal(ml_14_15(23000), 210.4)
  expect_equal(ml_14_15(44000, spouse_income = 0, rel_status = 'couple', n_children = 2), 226.3)
  expect_equal(ml_14_15(29000, spouse_income = 27000, rel_status = 'couple', n_children = 4),
               403.25, tolerance = .05)
  expect_equal(ml_14_15(25000, spouse_income = 17000, n_children = 1, rel_status = 'couple'), 10.10)
  expect_equal(ml_14_15(28000, spouse_income = 21000, n_children = 3, rel_status = 'couple'), 0)
})
