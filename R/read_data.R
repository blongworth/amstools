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
#' @export
readWheelfile <- function(wheel) {
  # won't work for CFAMS wheels
  read.delim(file)

}

# read raw results from database

# TODO: this needs work!

# #' Read SNICSer Output
# #'
# #' Reads normalized and blank corrected data from SNICSer blank correction
# #' "print" output. Files should be in tsv format with standard SNICS
# #' headers.
# #'
# #' @param file character. A SNICSer format file with path.
# #'
# #' @return A list of data tables for each chunk in file.
# #' @export
# #'
# readSnicsfile <- function(wheel) {
#
#   # read the file by lines
#   con <- file(wheel, open = "r")
#   lines <- readLines(con)
#   for (line in 15:length(lines)) {
#     if (!('\t' %in% lines[line])) {
#       name <- lines[line]
#       namei <- 1
#       line <- line + 1
#       while (!(lines[line] == "")) {
#         unlist(strsplit(lines[line], split = "\t"))
#       }
#     }
#   }
# }

# read analysed wheel from database

# read analyses for a recnum from db

# pull data on a target by tp_num

# pull data on an osg
