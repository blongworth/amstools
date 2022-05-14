# Functions for interacting with MICADAS DB

#' Get a list of MICADAS magazines
#'
#' @return A character vector of magazine names
#' @export
list_magazines <- function() {
  db <- conMICADAS()
  mags <- DBI::dbGetQuery(db, "SELECT DISTINCT magazine FROM target_v")
  DBI::dbDisconnect(db)
  mags[[1]]
}

#' Get run data for a list of magazine names.
#'
#' @param magazine A vector of magazine names in character format
#' @return A data frame of raw data
#' @export
get_magazine <- function(magazine) {
  db <- conMICADAS()
  sql <- glue::glue_sql("SELECT *
                  FROM workproto_v_nt
                  WHERE MAGAZINE IN ({magazines*})",
                          magazines = magazine,
                          .con = db)
  query <- DBI::dbSendQuery(db, sql)
  data <- DBI::dbFetch(query)
  DBI::dbClearResult(query)
  checkDB(data)
  DBI::dbDisconnect(db)
  data
}

#' Convert MICADAS results to SNICS results format
#'
#' @param micadas_df MICADAS data in `get_magazine()` format.
#'
#' @seealso \code{\link{get_magazine}} to store user credentials
#' in the system key store, and \code{\link{write_snics_results}} for
#' writing data to a SNICS results file.
#'
#' @return A dataframe in SNICS Results format
#' @export
#'
convert_micadas_snics <- function(micadas_df) {
  # Convert to snics results format
  micadas_df %>%
    group_by(position) %>%
    mutate(Meas = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    mutate(timestamp = t <- strptime(TIMEDAT, "%F %T"),
           RunCompletionTime = format(timestamp, "%a %b %d %H:%M:%S %Y"),
           Num = dplyr::case_when(type == "bl" ~ "B",
                           type == "oxa1" ~ "S",
                           TRUE ~ "U"),
           Cycles = RUNTIME * 10,
           le12C = ANA * 1E-6,
           le13C = NA,
           he12C = A * 1E-6,
           he13C = B * 1E-6,
           he1312 = BA,
           he1412 = RA
    ) %>%
    select(`Run Completion Time` = RunCompletionTime,
           Pos = position,
           Meas,
           `Sample Name` = user_label,
           Num,
           Cycles,
           le12C,
           le13C,
           he12C,
           he13C,
           CntTotH = R,
           CntTotS = R,
           CntTotGT = R,
           `13/12he` = he1312,
           `14/12he` = he1412
    )
}

#' Get MICADAS results and write SNICS resultsfile
#'
#' @param magazine Name of MICADAS magazine to convert.
#' @param filename Path and name of SNICS-style results file to write.
#'
#' @return Silently returns a dataframe of MICADAS data
#' @export
#'
make_micadas_resultsfile <- function(magazine, filename) {
  df <- get_magazine(magazine) %>%
    convert_micadas_snics()

  write_snics_results(df, filename)
  invisible(df)
}
