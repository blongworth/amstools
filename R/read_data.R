# read_data.r
#
# Functions to load raw wheel data from file or database


#' Read raw results data from file
#'
#' @param file A file path to a SNICS format results file.
#'
#' @return A data frame of the wheel results.
#' @export
readResfile  <- function (file) {
  read.delim(file, skip = 4, comment.char = "=")
}


#' Process wheel results data frame.
#'
#' @param data A data table as returned by readResfile.
#' @return A results data table with fixed and calculated fields.
#' @export
#' @importFrom dplyr mutate %>%
mungeResfile  <- function (data) {
  mutate(data,
         ts = as.POSIXct(strptime(
           Run.Completion.Time, format = "%a %b %d %H:%M:%S %Y")),
         Pos = as.factor(Pos),
         ce = 1/sqrt(CntTotGT), #Add counting error
         cor1412he = X14.12he/X13.12he^2 * 1E9, #Add corrected 14/12
         X14.12he = X14.12he * 1E12, #Convert ratio to 1E12
         he12C = he12C * 1E6, #Convert current to uA
         le12C = le12C * 1E6)
}


#' Read a SNICS wheelfile
#'
#' Won't work for CFAMS wheels
#'
#' @param wheel character. A USAMS format wheelfile/runlist.
#'
#' @return A data frame containing the wheelfile
readWheelfile <- function(wheel) {
  # won't work for CFAMS wheels
  read.delim(file)

}


#' Read Fudger or SNICSer Output
#'
#' Reads normalized and blank corrected data in Fudger format
#' from Fudger or SNICSer blank correction
#' "print" output. Files should be in tsv format with standard SNICS
#' headers.
#'
#' @param resfile character. A SNICSer format file with path.
#'
#' @return A list of data tables for each chunk in file.
#' @export
#'
read_fudger <- function(resfile) {

  # read the file by lines
  con <- file(resfile, open = "r")
  lines <- readLines(con)

  # get list of result blocks
  stds <- match("Standards", lines)
  rest <- lines[stds:length(lines)]
  out <- split(rest[rest != ""], cumsum(rest == "")[rest != ""])

  # Extract named tibble from block
  sn_table <- function(x) {
    out <- readr::read_tsv(I(x[2:length(x)]), show_col_types = FALSE)
    out <- list(out)
    names(out) <- x[1]
    out
  }

  # Extract all result blocks as list
  purrr::map(out, sn_table) |>
    purrr::flatten()
}

#' Read BATS format MICADAS output from Excel files
#'
#' BATS writes files in Excel 2003 xml format
#' File must be converted to `xlsx` format by opening and
#' saving in Excel
#'
#' @param file Path to BATS file
#'
#' @return A parsed dataframe of MICADAS results
#' @export
#'
read_bats <- function(file) {
  col_names <- c('sample_nr',	'sample_label', 'sample_label_nr',
  'measurement_comment', 'target_comment', 'counts',	'c12', 'c1412',
  'int_err_c1412', 'c1312', 'sigma', 'f14c', 'int_err_f14c', 'ext_err_f14c',
  'age', 'sig_age', 'd13c', 'mass', 'c13h', 'user', 'position', 'timestamp')
  col_types <- c(rep('text', 5), rep('numeric', 14), 'text', 'numeric', 'text')
  readxl::read_excel(file, skip = 5,na = "null",
                    col_names = col_names,
                    col_types = col_types) |>
    dplyr::filter(!is.na(counts)) |>
    dplyr::mutate(timestamp = as.POSIXct(timestamp, format='%d.%m.%Y %H:%M:%S'))
}

# pull data on a target by tp_num

# pull data on an osg
