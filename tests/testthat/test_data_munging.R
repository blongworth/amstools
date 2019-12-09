context("Data munging")

context("getQCData")

from <- '01-01-2016'
to <- '01-06-2016'
sys <- 'both'
data <- getQCData(from, to, sys, getcurrents = TRUE)

test_that("getQCData returns good data", {
	expect_true(is.data.frame(data))
  expect_equal(nrow(data), 12)
})

test_that("getQCData sys argument works", {
  expect_equal(nrow(getQCData(from, to, sys = "USAMS",
                            getcurrents = FALSE)), 12)
  expect_equal(nrow(getQCData(from, to, sys = "CFAMS",
                            getcurrents = FALSE)), 0)
})
test_that("getQCData works with rec argument", {
  expect_equal(as.numeric(getQCData(from, to, sys,
                            getcurrents = FALSE, rec = 34149)[,1]), 198420)
  expect_equivalent(sort(unlist(getQCData(from, to, sys,
                            getcurrents = FALSE,
                            rec = c(34148, 34149))[,1])), c(198420, 198441))
})

test_that("getQCData works with osg argument", {
  expect_equal(as.numeric(getQCData(from, to, sys,
                            getcurrents = FALSE,
                            osg = 146514)[,1]),
               198405)
  expect_equivalent(sort(unlist(getQCData(from, to, sys,
                            getcurrents = FALSE,
                            osg = c(146514, 146202))[,1])),
               c(198405, 198406))
})

