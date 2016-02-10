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
#' @param popErr Standard deviation of the population.
#' @param targErr Measurement error of a measurement.
#'
#' @return
#' @export
#'
#' @examples
intrErr <- function(popErr, targErr) {
  #calculate intrinsic error
  if (targErr > totErr) {
     sqrt(popErr^2 - targErr^2)
  } else {
    NA
  }
}


#' Total error.
#'
#' Calculate the predicted variability in a population given a measurement
#' error and an intrisic error.
#'
#' @param targErr Measurement error.
#' @param intrErr Intrinsic error.
#'
#' @return
#' @export
#'
#' @examples
totErr <- function(targErr, intrErr) {
#Calculate total error
  sqrt(targErr^2 + intrErr^2)
}

# Calculate confidence interval


#better function for statistical normalization/standard (0 mean 1 var)

