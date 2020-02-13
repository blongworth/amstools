context("Data Reduction")

context("Large Blank Correction")

from <- as.Date('2016-01-01')
to <- as.Date('2016-01-06')
sys <- 'both'
data <- getQCData(from, to, sys, getcurrents = TRUE)

test_that("LBC returns correct LBC Fm", {
        expect_equal(doLBC(1, 0, 1), 1)
        expect_equal(doLBC(0, 0, 1), 0)
})

test_that("LBC returns correct LBC Fm error", {
  expect_equal(doLBCerr(1, 0, 1, 0, 0), 0) # sqrt 0
})

context("Mass Balance Correction")

test_that("MBC returns correct MBC Fm", {
  expect_equal(doMBC(1, 0, 1, 1), 2)
  expect_equal(doMBC(0, 0, 1, 1), 0)
})

test_that("MBC returns correct MBC Fm Err", {
  expect_equal(doMBCerr(1, 0, 1, 1, 0, 0, 0, 0), 0) # sqrt 0
})
