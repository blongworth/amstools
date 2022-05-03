test_that("reading BATS files works", {
  test_data <- read_bats(test_path("testdata/36C220426SAT.xls"))
  expect_equal(class(test_data),
               c("tbl_df", "tbl", "data.frame"))
})
