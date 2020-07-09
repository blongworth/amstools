# General data reduction functions
# TODO: efficiency functions
# TODO: Calculate confidence interval
# TODO: better function for statistical normalization/standard (0 mean 1 var)

#' Relative SD
#'
#' Provides the standard deviation of a vector normalized to the mean of the vector.
#'
#' @param x A numeric vector.
#'
#' @return The relative SD of x
#' @export
#'
rsd <- function(x) {
  sd(x)/mean(x)
}


#' Standard Error
#'
#' @param x A numeric vector.
#'
#' @return The SE of x
#' @export
#'
se <- function(x) {
  sqrt(var(x)/length(x))
}


#' Sigma
#'
#' Also known as a z-score. Provides an estimate of the quality of
#' a measurement relative to it's expected value.
#'
#' @param fmm Measured value.
#' @param fmc Expected value.
#' @param em Measurement error.
#' @param ec Error of expected value.
#'
#' @return A vector of sigma values
#' @export
#'
sigma <- function(fmm, fmc, em, ec = 0) {
  (fmm - fmc) / sqrt(em^2 + ec^2)
}

#' Normalized Value
#'
#' Normalize a value using the expected value or population mean.
#'
#' @param fmm Measured value.
#' @param fmc Expected value.
#'
#' @return A vector of normalized fm
#' @export
#'
normFm <- function (fmm, fmc) {
  #Calculate normalized fm
  (fmm - fmc) / fmc
}


#' Intrinsic Error
#'
#' Calculate the "extra" (intrinsic, residual) error.
#'
#' @param x A vector of measured values.
#' @param err A vector of measurement errors.
#' @param ... Additional parameters passed to sd.
#'
#' @return A vector of residual errors
#' @export
#'
intrErr <- function(x, err, ...) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(err))
  xsd <- sd(x, ...)
  ifelse((xsd >= err),
         suppressWarnings(sqrt(xsd^2 - err^2)),
         NA)
}


#' Total error.
#'
#' Calculate the predicted variability in a population given a measurement
#' error and an intrisic error.
#'
#' @param rep_err Measurement error.
#' @param res_err Intrinsic error.
#' @param fm Fraction modern (optional).
#'
#' @return A vector of total errors.
#' @export
#'
totErr <- function(rep_err, res_err, fm) {
  stopifnot(is.numeric(rep_err))
  stopifnot(is.numeric(res_err))
  if (missing(fm)) {
    fm <- 1
  }
  stopifnot(is.numeric(fm))

  ifelse(is.na(res_err),
         rep_err,
         sqrt(rep_err^2 + (res_err * fm)^2)
  )
}

#' Calculate D14C.
#'
#' Calculate D14C given fraction modern and year of collection
#'
#'
#' @param fm Fraction modern.
#' @param yc Year of collection.
#'
#' @return A vector of D14C values
#' @export
#'
d14c <- function(fm, yc) {
  stopifnot(is.numeric(fm))
  stopifnot(is.numeric(yc))

  l <- 0.00012097 # 1/radiocarbon half life

  (fm * exp(l * (1950 - yc)) - 1) * 1000

}

#' Calculate radiocarbon years
#'
#' Calculate radiocarbon age from Fm
#'
#'
#' @param fm Fraction modern.
#'
#' @return radiocarbon age in years.
#' @export
#'
rcage <- function(fm) {
  stopifnot(is.numeric(fm))

  -8033 * log(fm)

}

#' Calculate Fm
#'
#' Calculate Fm from D14C
#'
#'
#' @param dc D14C of sample.
#' @param yc Year of collection.
#'
#' @return Fraction modern of sample.
#' @export
#'
dctofm <- function(dc, yc) {
  stopifnot(is.numeric(dc))
  stopifnot(is.numeric(yc))


  l <- 0.00012097 # 1/radiocarbon half life
  ((dc / 1000) + 1) / exp(l * (1950 - yc))

}

#' Remove outliers by interquartile range
#'
#' Outliers are defined as points more than 1.5 * IQR above the
#' third quartile or 1.5 * IQR below the first quartile.
#'
#' @param x A vector of numeric values
#'
#' @return The vector with outliers replaced by `NA`
#' @export
#'
removeOutliers = function(x) {
  # Get Q1 and Q3
  qnt <-  quantile(x, probs=c(.25, .75))

  iqt <- 1.5 * IQR(x)

  # Apply on a copy of the original data
  y = x
  y[x < (qnt[1] - iqt)] = NA
  y[x > (qnt[2] + iqt)] = NA
  y
  # Remove incomplete cases and return the resulted variable
  #y[complete.cases(y)]
}
