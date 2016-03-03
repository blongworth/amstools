context("Database functions")

test_that("conNOSAMS returns db connection", {
	expect_equal(class(conNOSAMS()), "RODBC")
})

RODBC::odbcCloseAll()
