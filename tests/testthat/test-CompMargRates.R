test_that("CompMargRates returns margRates object with expected slots.", {
  y0 <- c(283, 682, 145)
  n0 <- c(683, 2604, 1034)
  y1 <- c(95, 298, 89)
  n1 <- c(324, 1279, 501)
  out <- CompMargRates(
    y0 = y0,
    n0 = n0,
    y1 = y1,
    n1 = n1,
    alpha = 0.05,
    boot = FALSE,
    perm = FALSE
  )
  expect_s4_class(out, "margRates")
  expect_true(validObject(out))
  expect_equal(nrow(out@Rates), 2L)
  expect_equal(nrow(out@RD), 1L)
  expect_equal(nrow(out@RR), 1L)
  expect_equal(nrow(out@OR), 1L)
  expect_equal(nrow(out@Perm), 0L)
})

test_that("CompMargRates with boot and perm runs without error.", {
  y0 <- c(20, 40)
  n0 <- c(100, 200)
  y1 <- c(25, 50)
  n1 <- c(100, 200)
  set.seed(1)
  out <- CompMargRates(
    y0 = y0,
    n0 = n0,
    y1 = y1,
    n1 = n1,
    alpha = 0.05,
    boot = TRUE,
    perm = TRUE,
    reps = 50
  )
  expect_s4_class(out, "margRates")
  expect_true(nrow(out@Perm) >= 1L)
})

test_that("CompMargRates exclude_double_zero drops all-zero strata.", {
  y0 <- c(0, 10)
  n0 <- c(50, 100)
  y1 <- c(0, 15)
  n1 <- c(50, 100)
  out <- CompMargRates(
    y0 = y0,
    n0 = n0,
    y1 = y1,
    n1 = n1,
    exclude_double_zero = TRUE
  )
  expect_s4_class(out, "margRates")
  expect_equal(nrow(out@Rates), 2L)
})

test_that("print.margRates runs without error.", {
  y0 <- c(10, 20)
  n0 <- c(100, 200)
  y1 <- c(15, 25)
  n1 <- c(100, 200)
  out <- CompMargRates(y0, n0, y1, n1)
  expect_no_error(capture_output(print(out)))
})
