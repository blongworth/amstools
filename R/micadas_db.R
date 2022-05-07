# Functions for interacting with MICADAS DB

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
#' @examples
convert_micadas_snics <- function(micadas_df) {
  # Convert to snics results format
  micadas_df %>%
    group_by(position) %>%
    mutate(Meas = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    mutate(timestamp = t <- strptime(TIMEDAT, "%F %T"),
           RunCompletionTime = format(timestamp, "%a %b %d %H:%M:%S %Y"),
           SampleName = "Unk",
           Num = "U",
           Cycles = RUNTIME * 10,
           le12C = ANA * 1E-6,
           le13C = NA,
           he12C = A * 1E-6,
           he13C = B * 1E-6,
           CntTotH = R,
           CntTotS = R,
           CntTotGT = R,
           he1312 = BA,
           he1412 = RA
    ) %>%
    select(`Run Completion Time` = RunCompletionTime,
           Pos = position,
           Meas,
           `Sample Name` = SampleName,
           Num,
           Cycles,
           le12C,
           le13C,
           he12C,
           he13C,
           CntTotH,
           CntTotS,
           CntTotGT,
           `13/12he` = he1312,
           `14/12he` = he1412
    )
}

