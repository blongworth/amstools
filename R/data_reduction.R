# Functions to assist in AMS data reduction

# TODO:
# Propagate normalization error
# Find standards
# Produce normalized data for wheel per target


#' Calculate per-block fields
#'
#' @param data A data table of raw AMS data
#'
#' @return The data table with fields for corrected 14/12,
#' internal error, and d13C added
#' @export
#'
#' @examples
calcRaw <- function(data) {
	data %>%
	  mutate(
	    cor1412 = doCor1412(he1412, he1312),
	    sig1412 = calcSig1412(CntTotH, CntTotS, CntTotGT, cor1412),
	    d13c = calcd13c(he1312)
	  )
}


#' Correct raw 14/12 for fractionation using 13/12 ratio
#'
#' @param he1412 Raw measured 14/12
#' @param he1312 Raw measured 13/12
#'
#' @return
#' @export
#'
#' @examples
doCor1412 <- function(he1412, he1312) {
	he1412 / he1312 ^ 2
}

# Calc internal error for a measurement
calcSig1412 <- function(CntTotGT, cor1412, CntTotH = 1, CntTotS = 1) {
RelErrSq <- (CntTotH - CntTotS) * CntTotH ^ 2 / CntTotS ^ 4 +
             CntTotH ^ 2 / CntTotGT / CntTotS ^ 2
cor1412 * sqrt(RelErrSq)
}

# Calculate d13C
calcd13c <- function(he1312) {
	1000 * (he1312 / 1.12372 -1)
}

## Normalize

#' Normalize a run using the measured and consensus value of a standard
#'
#' @param sample Ratio to be normalized.
#' @param standard Measured ratio of the standard.
#' @param stdrat Consensus value of the standard.
#'
#' @return The normalized ratio of the sample.
#' @export
#'
normRun <- function(sample, standard, stdrat = 1.0398) {
  sample/standard * stdrat
}

#' Calculate external (deviation of runs) error for a target
#'
#' @param normRat A vector of normalized ratios for a target
#'
#' @return The external error
#' @export
#'
normRunExtErr <- function(normRat) {
  n <- length(normRat)
  meanRat <- mean(normRat)
  sqrt((sum((normRat - meanRat)^2))/(n * (n - 1)))
}

#' Calculate internal (counting) error for a target.
#'
#' @param counts A vector of counts for each run of a target.
#'
#' @return The internal error
#' @export
#'
normRunIntErr <- function(counts) {
  1 / sqrt(sum(counts))
}

#' Mean ratio for a target, weighted by error
#'
#' @param normRat A vector of normalized ratios.
#' @param normRatErr A vector of errors in measurements of the ratio.
#'
#' @return The weighted mean ratio for the target.
#' @export
#'
meanRat <- function(normRat, normRatErr) {
  (sum(normRat/normRatErr^2)/sum(1/normRatErr^2))
}


# Find mean of stds
normStds <- function(cor1412std, defstd) {
  # both vectors, same length or 1 element vector if all standards are same
  mean(cor1412std/defstd)
}

# Normalize to mean of standards
norm1412 <- function(cor1412, meanstd) {
  cor1412/meanstd
}


## Blank correction

#' Apply large blank
#'
#' @param fmmeas Normalized Fm of the sample
#' @param fmblank Normalized Fm of the blank
#' @param fmstd Accepted Fm of the standard used for normalization
#'
#' @return
#' @export
#'
#' @examples
doLBC <- function(fmmeas, fmblank, fmstd) {
	fmmeas - fmblank * (fmstd - fmmeas) / fmstd
}

#' Determine error in blankfm
#'
#' Uses the "error floor" method from SNICSer
#'
#' @param blankfm A vector of normalized Fm's of blanks.
#'
#' @return An error for the given blanks.
#' @export
#'
blankErr <- function(blankfm) {
  mfm <- mean(blankfm)
  sd <- sd(blankfm)
  ifelse(sd > mfm / 2, sd, mfm / 2)
}

#' Propagate large blank error
#'
#' @param fmmeas Normalized Fm of the sample(s)
#' @param fmblank Normalized Fm of the blank
#' @param fmstd Accepted Fm of the standard used for normalization
#' @param fmmeaserr Measurement error of the sample(s)
#' @param fmblankerr Measurement error of the blank
#'
#' @return A vector of propagated errors.
#' @export
#'
doLBCerr <- function(fmmeas, fmblank, fmstd, fmmeaserr, fmblankerr) {
  # Check inputs: fmblank, fmstd, fmblankerr should be scalar, others dbl.
	sqrt(fmmeaserr ^ 2 * (1 + fmblank / fmstd) ^ 2 +
	     fmblankerr ^ 2 * ((fmmeas - fmstd) / fmstd) ^ 2)
}


#' Apply mass balance blank correction
#'
#' Note, use total mass - blank mass for mass of the sample
#' if appropriate
#'
#' @param fmmeas Normalized Fm of the sample(s)
#' @param fmblank Normalized Fm of the blank
#' @param massmeas Mass of the sample(s)
#' @param massblank Mass of the blank
#'
#' @return A vector of mass balance corrected Fm.
#' @export
#'
doMBC <- function(fmmeas, fmblank, massmeas, massblank) {
  # Check inputs
  # LBC leads to negative fm's, so testing inputs is tricky
  # How should we handle these cases, data-wise?
  stopifnot(exprs = {
    is.numeric(c(fmmeas, fmblank, massmeas, massblank))
    #fmmeas >= 0
    #fmblank >= 0
    #massblank >=0
    #massmeas > massblank
  })

    fmmeas + (fmmeas - fmblank) * massblank / (massmeas - massblank)
}

#' Propagate mass balance blank correction error
#'
#' Note, use total mass - blank mass for mass of the sample
#' if appropriate
#'
#' @param fmmeas Normalized Fm of the sample(s)
#' @param fmblank Normalized Fm of the blank
#' @param massmeas Mass of the sample(s)
#' @param massblank Mass of the blank
#' @param fmmeaserr Measurement error of the sample(s)
#' @param fmblankerr Measurement error of the blank
#' @param massmeaserr Mass error of the sample(s)
#' @param massblankerr Mass error of the blank
#'
#' @return A vector of errors for blank corrected samples
#' @export
#'
doMBCerr <- function(fmmeas, fmblank, massmeas, massblank,
                  fmmeaserr, fmblankerr, massmeaserr, massblankerr) {
    sqrt(fmmeaserr ^ 2 * (1 + massblank / (massmeas - massblank)) ^ 2 +
         (massmeaserr ^ 2 + massblankerr ^ 2) * ((fmmeas - fmblank) * massblank / (massmeas - massblank) ^ 2) ^ 2 +
         fmblankerr ^ 2 * (massblank / (massmeas - massblank)) ^ 2 +
         massblankerr ^ 2 * ((fmmeas - fmblank) / (massmeas - massblank)) ^ 2)
}
