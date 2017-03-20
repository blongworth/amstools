context("Database functions")

test_that("conNOSAMS returns db connection", {
	expect_equal(class(conNOSAMS()), "RODBC")
})

from <- '01-01-2017'
to <- '01-10-2017'
sys <- 'both'

test_that("getStandards returns datatable", {
	expect_true(is.data.frame(getStandards(from, to, sys, getcurrents = FALSE)))
})
RODBC::odbcCloseAll()
