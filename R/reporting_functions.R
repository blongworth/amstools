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
