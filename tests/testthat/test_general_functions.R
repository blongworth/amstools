  context("intrErr")

test_that("invalid args detected", {
	expect_error(intrErr(iris, iris))
  expect_error(intrErr("red", "blue"))
})

test_that("correct values returned", {
  expect_equal(intrErr(c(0,1,2), c(0,1,2)), c(1,0,NA))
})

context("d14c")


test_that("invalid args detected", {
	expect_error(d14c(iris, iris))
  expect_error(d14c("red", "blue"))
})

test_that("Delta zero returned for modern", {
  expect_equal(d14c(1, 1950), 0)
})

test_that("Delta -1000 returned for dead", {
  expect_equal(d14c(0, 2015), -1000)
})
