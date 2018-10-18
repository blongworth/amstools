context("Database connection")

#test_that("conNOSAMS returns db connection", {
#	expect_true(conNOSAMS())
#})

from <- '01-01-2016'
to <- '01-06-2016'
sys <- 'both'
data <- getStandards(from, to, sys, getcurrents = TRUE)

context("getStandards")
test_that("getStandards returns good data", {
	expect_true(is.data.frame(data))
  expect_equal(nrow(data), 13)
})

test_that("getStandards sys argument works", {
  expect_equal(nrow(getStandards(from, to, sys = "USAMS",
                            getcurrents = FALSE)), 13)
  expect_equal(nrow(getStandards(from, to, sys = "CFAMS",
                            getcurrents = FALSE)), 0)
})

test_that("getStandards works with rec argument", {
  expect_equal(getStandards(from, to, sys,
                            getcurrents = FALSE, rec = 34149)[,1], 198420)
  expect_equal(getStandards(from, to, sys,
                            getcurrents = FALSE,
                            rec = c(34148, 34149))[,1], c(198420, 198441))
})

test_that("getStandards works with osg argument", {
  expect_equal(getStandards(from, to, sys,
                            getcurrents = FALSE,
                            osg = 146514)[,1],
               198405)
  expect_equal(getStandards(from, to, sys,
                            getcurrents = FALSE,
                            osg = c(146514, 146202))[,1],
               c(198405, 198406))
})

context("numRun")
test_that("numRun returns all runs in a date range", {
  expect_equal(numRun(from, to, sys)[1],
               121)
  expect_equal(numRun(from, to, "USAMS")[1],
               40)
})

