# Functions for generating data reports
# Rmd templates are in inst/rmd


#' Rec_num report
#'
#' Produces a report of all runs of a rec_num.
#'
#' @param rec Receipt number to query.
#'
#' @return A html report in the current directory.
#' @export
#'
recReport <- function(rec) {
  rmarkdown::render(system.file("rmd/recReport.Rmd", package="amstools"),
                    output_file = paste0(rec, ".html"),
                    output_dir = getwd(),
                    params = list(
                      rec = rec
                    )
  )
}

#' Wheel standards summary
#'
#' Produces a report of all standards on a wheel
#'
#' @param wheel Wheel to summarize.
#'
#' @return A dataframe with summary information
#' @export
#'
sumWheelStdsSR <- function(wheel) {

  # get data
  data <- getWheelStdsSR(wheel)

  # group by wheel and generate summary
  data %>%
    filter(fm_corr > 0.1) %>%
    mutate(normfm = amsdata::normFm(fm_corr, fm_cons),
           sigma = amsdata::sigma(fm_corr, fm_cons, sig_fm_corr)) %>%
    group_by(wheel) %>%
    dplyr::summarize(sigma.m = mean(sigma, na.rm = TRUE),
              sigma.sd = sd(sigma, na.rm = TRUE),
              normFm.m = mean(normfm, na.rm = TRUE))
}
