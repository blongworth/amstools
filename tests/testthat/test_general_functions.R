  context("intrErr")

test_that("invalid args detected", {
	expect_error(intrErr(iris, iris))
  expect_error(intrErr("red", "blue"))
})

test_that("correct values returned", {
  expect_equal(intrErr(c(0,1,2), c(0,1,2)), c(1,0,NA))
})
