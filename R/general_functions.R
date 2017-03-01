# General data reduction functions

#' Relative SD
#'
#' @param x A numeric vector.
#'
#' @return
#' @export
#'
#' @examples
rsd <- function(x) {
  sd(x)/mean(x)
}


#' Standard Error
#'
#' @param x A numeric vector.
#'
#' @return
#' @export
#'
#' @examples
se <- function(x) {
  sqrt(var(x)/length(x))
}


#' Sigma
#'
#' Also known as a z-score.
#'
#' @param fmm Measured value.
#' @param fmc Expected value.
#' @param em Measurement error.
#' @param ec Error of expected value.
#'
#' @return
#' @export
#'
#' @examples
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
#' @return
#' @export
#'
#' @examples
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
#'
#' @return
#' @export
#'
#' @examples
intrErr <- function(x, err, ...) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(err))
  xsd <- sd(x)
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
#' @return
#' @export
#'
#' @examples
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
#' @return
#' @export
#'
#' @examples
d14c <- function(fm, yc) {
  stopifnot(is.numeric(fm))
  stopifnot(is.numeric(yc))

  l <- 0.00012097 # 1/radiocarbon half life

  (fm * exp(l * (1950 - yc)) - 1) * 1000

}


# TODO: Calculate confidence interval


# TODO: better function for statistical normalization/standard (0 mean 1 var)

