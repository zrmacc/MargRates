test_that("BootEvents returns list with y0 and y1 of correct length.", {
  y0 <- c(10, 20)
  n0 <- c(100, 200)
  y1 <- c(15, 25)
  n1 <- c(100, 200)
  set.seed(1)
  out <- MargRates:::BootEvents(y0, n0, y1, n1)
  expect_named(out, c("y0", "y1"))
  expect_length(out$y0, 2L)
  expect_length(out$y1, 2L)
  expect_true(all(out$y0 >= 0 & out$y0 <= n0))
  expect_true(all(out$y1 >= 0 & out$y1 <= n1))
})

test_that("StatsBoot returns data.frame with expected structure.", {
  y0 <- c(20, 40)
  n0 <- c(100, 200)
  y1 <- c(25, 50)
  n1 <- c(100, 200)
  set.seed(1)
  out <- MargRates:::StatsBoot(
    y0 = y0,
    n0 = n0,
    y1 = y1,
    n1 = n1,
    reps = 30
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
  expect_true("Lower" %in% names(out))
  expect_true("Upper" %in% names(out))
  expect_true("P" %in% names(out))
})

test_that("PermEvents preserves totals.", {
  y0 <- c(10, 20)
  n0 <- c(100, 200)
  y1 <- c(15, 25)
  n1 <- c(100, 200)
  set.seed(1)
  out <- MargRates:::PermEvents(y0, n0, y1, n1)
  expect_equal(out$y0 + out$y1, y0 + y1)
  expect_equal(out$n0, n0)
  expect_equal(out$n1, n1)
})

test_that("TestNull returns data.frame with permutation p-values.", {
  y0 <- c(20, 40)
  n0 <- c(100, 200)
  y1 <- c(25, 50)
  n1 <- c(100, 200)
  set.seed(1)
  out <- MargRates:::TestNull(
    y0 = y0,
    n0 = n0,
    y1 = y1,
    n1 = n1,
    reps = 30
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
  expect_true(all(out$P >= 0 & out$P <= 1))
})
