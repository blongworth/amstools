# Write data to flat file formats

#' Write out tsv file in SNICS results format
#'
#' @param snics_df A dataframe in SNICS_results format
#' @param filename Filename to write to.
#'
#' @return
#' @export
#'
write_snics_results <- function(snics_df, filename) {
  #
  # there's an extra space before some fields in a snics results file. Not putting those here.
  # Handle NA's?
  writeLines(snics_header, filename)
  #readr::write_tsv(snics_df, filename, append = TRUE)
  write.table(snics_df, filename,
              append = TRUE, quote = FALSE,
              sep = "\t", na = "42",
              col.names = FALSE, row.names = FALSE)
}
