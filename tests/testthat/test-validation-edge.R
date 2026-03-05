test_that("CompMargRates validates input lengths.", {
  y0 <- c(10, 20)
  n0 <- c(100, 200)
  y1 <- c(15)
  n1 <- c(100)
  expect_error(
    CompMargRates(y0 = y0, n0 = n0, y1 = y1, n1 = n1),
    "same length"
  )
})

test_that("CompMargRates validates n0 and n1 positive.", {
  y0 <- c(0)
  n0 <- c(0)
  y1 <- c(0)
  n1 <- c(100)
  expect_error(
    CompMargRates(y0 = y0, n0 = n0, y1 = y1, n1 = n1),
    "positive"
  )
})

test_that("CompMargRates validates event counts in range.", {
  y0 <- c(10, 20)
  n0 <- c(100, 200)
  y1 <- c(15, 250)
  n1 <- c(100, 200)
  expect_error(
    CompMargRates(y0 = y0, n0 = n0, y1 = y1, n1 = n1),
    "0 <= y0 <= n0"
  )
})

test_that("CompMargRates validates alpha in (0, 1).", {
  y0 <- c(10)
  n0 <- c(100)
  y1 <- c(15)
  n1 <- c(100)
  expect_error(
    CompMargRates(y0 = y0, n0 = n0, y1 = y1, n1 = n1, alpha = 0),
    "alpha must be"
  )
  expect_error(
    CompMargRates(y0 = y0, n0 = n0, y1 = y1, n1 = n1, alpha = 1),
    "alpha must be"
  )
})

test_that("CompMargRates validates reps when boot or perm is TRUE.", {
  y0 <- c(10)
  n0 <- c(100)
  y1 <- c(15)
  n1 <- c(100)
  expect_error(
    CompMargRates(y0 = y0, n0 = n0, y1 = y1, n1 = n1, boot = TRUE, reps = 0),
    "reps must be"
  )
})

test_that("CompMargRates errors when exclude_double_zero removes all strata.", {
  y0 <- c(0, 0)
  n0 <- c(50, 50)
  y1 <- c(0, 0)
  n1 <- c(50, 50)
  expect_error(
    CompMargRates(
      y0 = y0, n0 = n0, y1 = y1, n1 = n1,
      exclude_double_zero = TRUE
    ),
    "No strata remaining"
  )
})

test_that("RiskRatio handles p0 = 0 without error.", {
  y0 <- c(0)
  n0 <- c(100)
  y1 <- c(5)
  n1 <- c(100)
  out <- RiskRatio(y0 = y0, n0 = n0, y1 = y1, n1 = n1)
  expect_s3_class(out, "data.frame")
  expect_equal(out$Stat, "RiskRatio")
  expect_true(is.na(out$Est) || !is.finite(out$Est) || is.infinite(out$Est))
})

test_that("OddsRatio handles boundary rates without error.", {
  y0 <- c(0)
  n0 <- c(100)
  y1 <- c(0)
  n1 <- c(100)
  out <- OddsRatio(y0 = y0, n0 = n0, y1 = y1, n1 = n1)
  expect_s3_class(out, "data.frame")
  expect_equal(out$Stat, "OddsRatio")
})
