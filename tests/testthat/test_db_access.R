context("Database functions")

test_that("conNOSAMS returns db connection", {
	expect_equal(class(conNOSAMS()), "RODBC")
})

from <- '01-01-2016'
to <- '01-06-2016'
sys <- 'both'

test_that("getStandards returns datatable", {
	expect_true(is.data.frame(getStandards(from, to, sys, getcurrents = TRUE)))
})
RODBC::odbcCloseAll()
