# General data reduction functions

#relative sd
rsd <- function(x) {
  sd(x)/mean(x)
}

se <- function(x) {
  #Calculate standard error
  sqrt(var(x)/length(x))
}

sigma <- function(fmm, fmc, em, ec = 0) {
  #Calculate Sigma
  #also known as a z-score
  (fmm - fmc) / sqrt(em^2 + ec^2)
}

normFm <- function (fmm, fmc) {
  #Calculate normalized fm
  (fmm - fmc) / fmc
}

intrErr <- function(popErr, targErr) {
  #calculate intrinsic error
  if (targErr > totErr) {
     sqrt(popErr^2 - targErr^2)
  } else {
    NA
  }
}

totErr <- function(targErr, intrErr) {
#Calculate total error
  sqrt(targErr^2 + intrErr^2)
}

# Calculate confidence interval


#better function for statistical normalization/standard (0 mean 1 var)

