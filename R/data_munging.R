# Functions for cleaning up and calculations on data pulled from the database

joinStandards <- function(data, std) {

  #join standard table
   dplyr::inner_join(data, std, by = "rec_num")
}


#' Modify and calculate fields for raw standards data.
#'
#' @param data A data frame as returned by getStandards
#' @param std The standards table as returned by getStdTable
#'
#' @return A data frame of standards with calculated fields.
#' @export
#' @importFrom dplyr select mutate group_by %>%
mungeStandards <- function(data, std) {

  data <- joinStandards(data, std)

  data <- data %>%
    mutate(
      tp_date_pressed = as.Date(tp_date_pressed),
      gf_date = as.Date(gf_date),
      rep_err = pmax(f_int_error, f_ext_error),
      normFm = normFm(f_modern, fm_consensus),
      sigma = sigma(f_modern, fm_consensus, rep_err),
      frep_err = rep_err / f_modern,
      system = ifelse(grepl("CFAMS", wheel), "CFAMS", "USAMS"),
      #is ox-i primary?
      primary = (((sample_type == "S") |
                    (sample_type_1 == "S")) &
                  ((grepl("OX-I", name)) |
                    (grepl("OX-1", name))))
    ) %>%
    select(-f_int_error,-f_ext_error,-sample_type,-sample_type_1) %>%
    #number of splits?
    group_by(osg_num) %>% #For each osg_num
    mutate(splits = dplyr::n()) #Count occurrences to get number of splits

  if (exists('le12c', where = data)) {
    data <- mutate(data, le12c = ifelse(system == "USAMS", le12c * -1, le12c))
  }

  return(data)
}

#' Modify and calculate fields for raw qc data.
#'
#' @param data A data frame as returned by getQCTable
#'
#' @return A data frame with calculated fields.
#' @export
#' @importFrom dplyr select mutate group_by %>%
mungeQCTable <- function(data) {
  data %>%
    mutate(
      tp_date_pressed = as.Date(tp_date_pressed),
      rep_err = pmax(f_int_error, f_ext_error),
      normFm = normFm(f_modern, fm_consensus),
      sigma = sigma(f_modern, fm_consensus, rep_err),
      frep_err = rep_err / f_modern,
      name = descr,
      system = substring(wheel, 1, 5)
    ) %>% #system
    select(-f_int_error,-f_ext_error)

}


#' Get and format standards data
#'
#' @param from Character vector of date in form 'YYYY-MM-DD'.
#' @param to Character vector of date in form 'YYYY-MM-DD'.
#' @param sys Character vector of system name: 'cfams', 'usams', or 'both'.
#' @param useQC Use data from QC table if true.
#' @param intcal Use standards data from intcal table if true.
#' @param getcurrents get current and count data if true.
#' @param ... Additional options passed to getStandards
#'
#' @return A data frame of munged standards data
#' @export
#'
getQCData <- function(from, to = "present", sys = "both",
                      useQC = FALSE, intcal = FALSE, getcurrents = FALSE, ...) {
  # Function to get standards from database and return munged table

  if (missing(from)) {
    stop('argument "from" is missing, with no default')
  }

  if (useQC) {
    data <- getQCTable(from, to, sys)
    out <- mungeQCTable(data)
  } else {
    if (intcal) {
      std <- getIntcalTable()
    } else {
      std <- getStdTable()
    }
    data <- getStandards(from, to, sys, getcurrents, ...)
    out <- mungeStandards(data, std)
  }

  return(out)

}

