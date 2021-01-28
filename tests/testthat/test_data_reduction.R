context("Data Reduction")

data <- getWheel("CFAMS011421") %>%
  select(wheel_pos, sample_name, norm_ratio,
         int_err, ext_err, fm_corr, sig_fm_corr,
         lg_blk_fm, sig_lg_blk_fm, fm_mb_corr, sig_fm_mb_corr,
         blank_fm, sig_blank_fm, tot_mass, sig_tot,
         blank_mass, sig_blank_mass) %>%
  mutate(max_err = pmax(int_err, ext_err))

context("Large Blank Correction")

test_that("LBC returns correct LBC Fm", {
        expect_equal(doLBC(1, 0, 1), 1)
        expect_equal(doLBC(0, 0, 1), 0)
        # ceylon should be no correction, but snics_results shows different cor val
        expect_equal(doLBC(data[1,3], data[1,8], 1.0398), data[1,3])
        # C-2 true value is different at 0.000001 level
        expect_equal(doLBC(data[5,3], data[5,8], 1.0398), data[5,6])
        # TIRI-I
        expect_equal(doLBC(data[6,3], data[6,8], 1.0398), data[6,6])

})

test_that("LBC works with vectors", {
        expect_equal(doLBC(c(1, 0), c(0, 0), c(1,1)), c(1, 0))
        expect_equal(doLBC(data[,3], data[,8], 1.0398), data[,6])

})

test_that("LBC returns correct LBC Fm error", {
  expect_equal(doLBCerr(fmmeas = 1, fmblank = 0, fmstd = 1, fmmeaserr = 0, fmblankerr = 0), 0) # sqrt 0
  # ceylon should be no correction, but snics_results shows different cor val
  expect_equal(doLBCerr(data[1,3], data[1,8], 1.0398, data[1,18], data[1,9]), data[1,7])
  # C-2
  expect_equal(doLBCerr(data[5,3], data[5,8], 1.0398, data[5,18], data[5,9]), data[5,7])
  # TIRI-I
  expect_equal(doLBCerr(data[6,3], data[6,8], 1.0398, data[6,18], data[6,9]), data[6,7])
})

test_that("LBCerr works with vectors", {
  expect_equal(doLBCerr(data[,3], data[,8], 1.0398, data[,18], data[,9]), data[,7])
})

context("Mass Balance Correction")

# LBC leads to negative fm's, so testing inputs is tricky
# How should we handle these cases, data-wise?
test_that("doMBC checks for valid input", {
  expect_error(doMBC("blue", 0, 0, 1))
  #expect_error(doMBC(-1, 0, 0, 1))
  #expect_error(doMBC(1, 0, 0, 0))
  #expect_error(doMBC(0, 0, 1, 1))
})

test_that("doMBC returns NA when values are missing", {
  expect_equal(is.na(doMBC(data[1,6], data[1,12], data[1,14], data[1,16])), TRUE)

})

test_that("MBC returns correct MBC Fm", {
  expect_equal(doMBC(fmmeas = 1, fmblank = 1, massmeas = 1, massblank = 0), 1)
  expect_equal(doMBC(1, 0, 2, 1), 2)
  expect_equal(doMBC(data[5,6], data[5,12], data[5,14], data[5,16]), data[5,10])
  expect_equal(doMBC(data[6,6], data[6,12], data[6,14], data[6,16]), data[6,10])
})

test_that("MBC works with vectors", {
  expect_equal(doMBC(c(1,1), c(1,0), c(1,2), c(0,1)), c(1,2))
  expect_equal(doMBC(data[,6], data[,12], data[,14], data[,16]), data[,10])
})
test_that("MBC returns correct MBC Fm Err", {
  expect_equal(doMBCerr(1, 0, 1, 0, 0, 0, 0, 0), 0) # sqrt 0
  expect_equal(doMBCerr(data[5,6], data[5,12], data[5,14], data[5,16],
                     data[5,7], data[5,13], data[5,15], data[5,17]),
               data[5,11])
  expect_equal(doMBCerr(data[6,6], data[6,12], data[6,14], data[6,16],
                     data[6,7], data[6,13], data[6,15], data[6,17]),
               data[6,11])
})

test_that("MBCErr works with vectors", {
  expect_equal(doMBCerr(data[,6], data[,12], data[,14], data[,16],
                     data[,7], data[,13], data[,15], data[,17]),
               data[,11])
})

