context("intrErr")

test_that("invalid args detected", {
	expect_error(intrErr(iris, iris))
  expect_error(intrErr("red", "blue"))
})

test_that("correct values returned", {
  expect_equal(intrErr(c(0,1,2), c(0,1,2)), c(1,0,NA))
})

context("totErr")

test_that("invalid args detected", {
	expect_error(totErr(iris, iris, iris))
  expect_error(totErr("red", "blue", "fee"))
})

test_that("correct values returned", {
  expect_equal(totErr(c(0,1,1), c(0,0,1)), c(0,1,1.414214), tolerance = 0.002)
  expect_equal(totErr(c(0,1,1), c(0,0,1), .5), c(0,1,1.118034), tolerance = 0.002)
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


context("dctofm")

test_that("invalid args detected", {
	expect_error(dctofm(iris, iris))
  expect_error(dctofm("red", "blue"))
})

test_that("Modern returned for delta 0", {
  expect_equal(dctofm(0, 1950), 1)
})

test_that("Delta -1000 returns dead", {
  expect_equal(dctofm(-1000, 2015), 0)
})


context("rcage")

test_that("invalid args detected", {
	expect_error(rcage(iris))
  expect_error(rcage("red"))
})

test_that("Zero age returned for modern", {
  expect_equal(rcage(1), 0)
})

test_that("Inf returned for dead", {
  expect_equal(rcage(0), Inf)
})
