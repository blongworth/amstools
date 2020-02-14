context("Data Reduction")

data <- getWheel("CFAMS020620") %>%
        select(wheel_pos, sample_name, norm_ratio, 
                int_err, ext_err, fm_corr, sig_fm_corr,
                lg_blk_fm, sig_lg_blk_fm, fm_mb_corr,
                sig_fm_mb_corr, blank_fm, sig_blank_fm,
                blank_mass, sig_blank_mass) %>%
        mutate(max_err = pmax(int_err, ext_err))
# need to join dc13 for mass of sample?

context("Large Blank Correction")

test_that("LBC returns correct LBC Fm", {
        expect_equal(doLBC(1, 0, 1), 1)
        expect_equal(doLBC(0, 0, 1), 0)
        # ceylon should be no correction, but snics_results shows different cor val
        expect_equal(doLBC(data[1,3], data[1,8], 1.0398), data[1,3]) 
        # C-8 OC true value is different at 0.000001 level
        expect_equal(doLBC(data[5,3], data[5,8], 1.0398), data[5,6]) 
        # Small C-2 (lgblk from database)
        expect_equal(doLBC(data[7,3], data[7,8], 1.0398), data[7,6]) 

})

test_that("LBC returns correct LBC Fm error", {
  expect_equal(doLBCerr(1, 0, 1, 0, 0), 0) # sqrt 0
        # ceylon should be no correction, but snics_results shows different cor val
        expect_equal(doLBCerr(data[1,3], data[1,8], 1.0398, data[1,-1], data[1,9]), data[1,7]) 
        # C-8 OC true value is different at 0.000001 level
        expect_equal(doLBCerr(data[5,3], data[5,8], 1.0398, data[5,-1], data[5,9]), data[5,7]) 
        # Small C-2 (lgblk from database)
        expect_equal(doLBCerr(data[7,3], data[7,8], 1.0398, data[7,-1], data[7,9]), data[7,7]) 
})

context("Mass Balance Correction")

test_that("MBC returns correct MBC Fm", {
  expect_equal(doMBC(1, 0, 1, 1), 2)
  expect_equal(doMBC(0, 0, 1, 1), 0)
        expect_equal(doMBC(data[1,6], data[1,12], data[1,14], data[1,9]), data[1,7]) 
  
})

test_that("MBC returns correct MBC Fm Err", {
  expect_equal(doMBCerr(1, 0, 1, 1, 0, 0, 0, 0), 0) # sqrt 0
})

