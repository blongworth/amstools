# Database functions

# TODO:
# get results for any tp_num
# get results for any osg_num

#' Check odbc call returns for errors
#'
#' @param data The return from a call to sqlQuery
#'
#' @export
#'
checkDB  <- function(data) {
  if (is.character(data)) {
    stop(paste(data, collapse = "\n"))
  }
}


#' Open NOSAMS DB connection
#'
#' Takes a connection string from the CONSTRING environment variable.
#'
#' @return An odbc db connection object
#' @export
#'
conNOSAMS  <- function() {
  odbc::dbConnect(odbc::odbc(), .connection_string = Sys.getenv("CONSTRING"))
}


#' Get secondary data from qc table
#'
#' @param from Character vector of date in form 'YYYY-MM-DD'.
#' @param to Character vector of date in form 'YYYY-MM-DD'.
#' @param sys Character vector of system name: 'cfams', 'usams', or 'both'.
#'
#' @return A data frame of qc table data
#' @export
#'
getQCTable <- function(from, to, sys = "both") {

  if (missing(from)) {
    stop('argument "from" is missing, with no default')
  }

  # If no to, get to today.
  if (missing(to)) {
    to <- Sys.Date()
  }

  #What system do we want data for?
  if (sys == "cfams") {
    whid <- "AND wheel LIKE 'C%'"
  } else if (sys =="usams") {
    whid <- "AND wheel LIKE 'U%'"
  } else if (sys =="both") {
    whid <- ""
  } else {
    whid <- "AND wheel NOT LIKE 'C%'"
  }

  dquery <- paste(
    "SELECT
   	 target.tp_num,
         target.tp_date_pressed,
         qc.target_time,
         qc.rec_num,
         qc.descr,
         qc.process,
         qc.num,
         qc.wheel,
         qc.lab,
         qc.fm_consensus,
         qc.f_modern,
         qc.f_int_error,
         qc.f_ext_error,
         qc.co2_yield,
         qc.perc_yield,
         qc.gf_co2_qty,
         qc.dc13_sample,
         qc.dc13_measured,
         qc.q_flag,
         qc.dc13_con,
         qc.ss
    FROM qc
      INNER JOIN target ON qc.tp_num = target.tp_num
    WHERE target.tp_date_pressed > ?
    AND target.tp_date_pressed < ?",
    whid)

  #Do the queries
  db <- conNOSAMS()
  query <- odbc::dbSendQuery(db, dquery)
  odbc::dbBind(query, list(from, to))
  data <- odbc::dbFetch(query)
  odbc::dbClearResult(query)
  checkDB(data)
  return(data)
}


#' Get Standards
#'
#' Get standards from database using standards table.
#'
#' @param from Character vector of date in form 'YYYY-MM-DD'.
#' @param to Character vector of date in form 'YYYY-MM-DD'.
#' @param sys Character vector of system name: 'cfams', 'usams', or 'both'.
#' @param getcurrents logical. Get current and count data?
#' @param rec Numeric vector of NOSAMS reciept numbers. If supplied, get
#'            all records matching these receipt numbers.
#' @param osg Numeric vector of NOSAMS osg numbers. Supersedes 'rec' parameter.
#'
#' @return A data frame of standards data
#' @export
#'
getStandards <- function (from,
                          to,
                          sys = "both",
                          getcurrents = TRUE,
                          rec = NULL,
                          osg = NULL) {

  # TODO: input validation
  if (missing(from)) {
    stop('argument "from" is missing, with no default')
  }

  # If no to, get to today.
  if (missing(to)) {
    to <- Sys.Date()
  }

    # convert dates
  from <- as.Date(from)
  to <- as.Date(to)

  #get any rec_num if requested
  if (is.null(rec)) {
    samples  <- ""
  } else {
    samples  <- paste0("AND target.rec_num IN (", paste(rec, collapse = ","),")")
  }

  # or get a list of OSG nums
  if (!is.null(osg)) {
    samples  <- paste0("AND target.osg_num IN (", paste(osg, collapse = ","),")")
  }

  #What system do we want data for?
  if (sys == "cfams" | sys == "CFAMS") {
    whid <- "AND wheel_id LIKE 'CF%'"
  } else if (sys == "usams" | sys == "USAMS") {
    whid <- "AND wheel_id NOT LIKE 'CF%'"
  } else if (sys == "both") {
    whid <- ""
  } else {
    whid <- "AND wheel_id NOT LIKE 'C%'"
  }

  # include form to get old data (don't use snics tables)
  # need to include target_time, d13 irms, co2_yield, process
  dquery <- paste(
    "SELECT
      target.tp_num,
      gf_date,
      target.tp_date_pressed,
      snics_results.runtime,
      target.rec_num,
      target.osg_num,
      wheel_pos.wheel_id AS wheel,
      graphite_lab.lab_name AS lab,
      no_os.f_modern,
      no_os.f_int_error,
      no_os.f_ext_error,
      --snics_results.int_err,
      --snics_results.ext_err,
      graphite.gf_co2_qty,
      gf_devel, gf_test,
      no_os.dc13,
      no_os.q_flag,
      snics_results.sample_type,
      snics_results.sample_type_1
    FROM no_os
    INNER JOIN target
      ON no_os.tp_num = target.tp_num
    INNER JOIN wheel_pos
      ON no_os.tp_num = wheel_pos.tp_num
    LEFT JOIN snics_results
      ON no_os.tp_num = snics_results.tp_num
    INNER JOIN graphite
      ON target.osg_num = graphite.osg_num
    INNER JOIN graphite_lab
      ON target.graphite_lab = graphite_lab.lab_id
    JOIN (SELECT
            rec_num, Fm_cons,
            d13_cons, sample_id
          FROM standards
          WHERE Fm_cons IS NOT NULL)
            AS standards
      ON target.rec_num = standards.rec_num
    WHERE target.tp_date_pressed > ?
    AND target.tp_date_pressed < ?
    AND f_modern > -1
    ", samples, whid
  )

  cquery <- paste(
    "SELECT
       snics_raw.tp_num,
       AVG(le12c) AS le12c,
       SUM(cnt_14c) AS counts
     FROM snics_raw
       INNER JOIN snics_results
         ON snics_results.tp_num = snics_raw.tp_num
       INNER JOIN target
         ON snics_raw.tp_num = target.tp_num
       INNER JOIN wheel_pos
         ON snics_results.tp_num = wheel_pos.tp_num
     WHERE target.tp_date_pressed > ?
       AND target.tp_date_pressed < ?
       AND ok_calc = 1
       ", samples, whid,
       "GROUP BY snics_raw.tp_num"
    )

  #Do the queries

  db <- conNOSAMS()
  query <- odbc::dbSendQuery(db, dquery)
  odbc::dbBind(query, list(from, to))
  data <- odbc::dbFetch(query)
  odbc::dbClearResult(query)
  checkDB(data)

  if (getcurrents) {
    db <- conNOSAMS()
    query <- odbc::dbSendQuery(db, cquery)
    odbc::dbBind(query, list(from, to))
    cur <- odbc::dbFetch(query)
    odbc::dbClearResult(query)
    checkDB(cur)

    data  <- dplyr::left_join(data, cur, by = "tp_num")
  }

  return(data)
}


#' Get info for a wheel
#'
#' @param wheel Character vector of wheel name in form '[CF|US]AMSMMDDYY'.
#' @return A data frame of wheel information
#' @export
#'
getWheelInfo <- function(wheel) {

  # TODO: validate wheel
  con <- conNOSAMS()
  sql <- "SELECT wheel_position,
            cl_id, target.tp_num,
            osg_num, target.rec_num
          FROM wheel_pos
          LEFT JOIN target
          ON wheel_pos.tp_num = target.tp_num
          LEFT JOIN logged_sample
          ON target.rec_num = logged_sample.rec_num
          WHERE wheel_id = ?"
  query <- odbc::dbSendQuery(con, sql)
  odbc::dbBind(query, list(wheel))
  data <- odbc::dbFetch(query)
  odbc::dbClearResult(query)
  checkDB(data)
  data
}


#' Get Intcal table
#'
#' @return A data frame of the Intcal table
#' @export
#'
getIntcalTable <- function() {

  #get intcal table
  db <- conNOSAMS()
  intcal <- odbc::dbGetQuery(db, "SELECT * FROM intercal_samples")

  #create factor of tiri_id, order by Fm
  intcal <- within(intcal, name <- factor(tiri_id, levels = unique(
                   tiri_id[order(fm_consensus, tiri_id)]),ordered = TRUE))

  #Replace C-6 with new consensus from Xiaomei 2010
  intcal$fm_consensus[intcal$rec_num == 1086] <- 1.5016

  # trim table
  dplyr::select(intcal, rec_num, name, fm_consensus)
}


#' Get Standards Table
#'
#' @return A data frame of secondary standards data
#' @export
#'
getStdTable <- function() {

  #Open DB connection
  db <- conNOSAMS()

  # get standards. Should we get NOSAMS_cons?
  standards <- odbc::dbGetQuery(db, "SELECT * FROM standards WHERE Fm_cons IS NOT NULL")

  standards <- dplyr::mutate(standards, fm_consensus = Fm_cons)
  standards <- dplyr::select(standards, rec_num, sample_id, fm_consensus)

  # TODO: make names the same
  #create factor of tiri_id, order by Fm
  within(standards,
         name <- factor(sample_id,
                        levels = unique(sample_id[order(fm_consensus, sample_id)]),
                        ordered = TRUE))
}

#' Count wheels and runs in a time period.
#'
#' Uses target runtimes from snics_results for data source.
#'
#' @param from A date object or date in character form (m-d-Y)
#' @param to A date object or date in character. Defaults to present.
#' @param sys System- "USAMS", "CFAMS", defaults to both.
#' @return A list: number of runs, number of wheels
#' @export
#'
numRun <- function(from, to, sys = "both") {

  # If no to, get to today.
  if (missing(to)) {
    to <- Sys.Date()
  }

  con <- conNOSAMS()
  sql <- "SELECT wheel
            FROM snics_results
            WHERE runtime > ?
            AND runtime < ?"
  query <- odbc::dbSendQuery(con, sql)
  odbc::dbBind(query, list(from, to))
  data <- odbc::dbFetch(query)
  odbc::dbClearResult(query)
  checkDB(data)

  if (sys != "both") {
    data <- dplyr::filter(data, substr(wheel, 1, 5) == sys)
  }

  targets <- length(data$wheel)
  wheels <- length(unique(data$wheel))
  c(targets,wheels)
}


#' Get results for a list of wheel names.
#'
#' @param wheel A vector of wheel names in character format
#' @param test Get data from snics_results_test if true
#' @return A data frame of analysed data
#' @export
getWheel <- function(wheel, test = FALSE) {
  db <- conNOSAMS()
  table <- ifelse(test == TRUE, "snics_results_test", "snics_results")
  sql <- glue::glue_sql("SELECT *
                  FROM {`table`}
                  WHERE wheel IN ({wheels*})",
                          table = table,
                          wheels = wheel,
                          .con = db)
  query <- DBI::dbSendQuery(db, sql)
  data <- DBI::dbFetch(query)
  checkDB(data)
  data
}


#' Get raw data for a list of wheel names.
#'
#' @param wheel A vector of wheel names in character format
#' @return A data frame of raw data
#' @export
getRawWheel <- function(wheel) {
  db <- conNOSAMS()
  sql <- glue::glue_sql("SELECT *
                  FROM snics_raw
                  WHERE wheel IN ({wheels*})",
                          wheels = wheel,
                          .con = db)
  query <- DBI::dbSendQuery(db, sql)
  data <- DBI::dbFetch(query)
  checkDB(data)
  data
}

#' Get raw ams data from database
#'
#' @param from A date in character form (m-d-Y)
#' @param to A date in character. Defaults to present.
#' @return A data frame of raw data
#' @export
getRawData <- function(from, to) {

  # If no to, get to today.
  if (missing(to)) {
    to <- Sys.Date()
  }

  con <- conNOSAMS()
  sql <- "SELECT *
             FROM snics_raw
             WHERE runtime > ?
             AND runtime < ?"
  query <- odbc::dbSendQuery(con, sql)
  odbc::dbBind(query, list(from, to))
  data <- odbc::dbFetch(query)
  odbc::dbClearResult(query)
  checkDB(data)
  data
}


#' Get standards from a wheel
#'
#' @param wheel A wheelname as a character vector
#'
#' @return A dataframe of summary statistics
#' @export
#'
getWheelStds <- function(wheel) {
 # mean sigma
  # sd of sigma
  # mean NormFm
  db <- conNOSAMS()
  query <- "SELECT wheel_id,
                      sample_id, target.rec_num, target.osg_num,
                      f_modern, f_ext_error, dc13,
                      s.fm_cons, s.d13_cons
                    FROM no_os
                    INNER JOIN wheel_pos
                      ON no_os.tp_num = wheel_pos.tp_num
                    JOIN target
                    ON no_os.tp_num = target.tp_num
                    JOIN (SELECT rec_num, Fm_cons, d13_cons, sample_id
                          FROM standards WHERE Fm_cons IS NOT NULL)
                        AS s
                    ON target.rec_num = s.rec_num
                    WHERE wheel_id = ?"

  wheels <- odbc::dbSendQuery(db, query)
  dbBind(wheels, list(wheel))
  data <- dbFetch(wheels)
  dbClearResult(wheels)
  data
}

#' Get standards from a wheel from snics_results
#'
#' @param wheel A wheelname as a character vector
#'
#' @return A dataframe of summary statistics
#' @export
#'
getWheelStdsSR <- function(wheel) {
 # mean sigma
  # sd of sigma
  # mean NormFm
  db <- conNOSAMS()
  query <- "SELECT wheel, wheel_pos, sample_name, target.rec_num, target.osg_num,
                      fm_corr, sig_fm_corr, del_13c,
                      s.fm_cons, s.d13_cons
                    FROM snics_results
                    JOIN target
                      ON snics_results.tp_num = target.tp_num
                    JOIN (SELECT rec_num, Fm_cons, d13_cons
                          FROM standards WHERE Fm_cons IS NOT NULL)
                        AS s
                      ON s.rec_num = target.rec_num
                    WHERE wheel = ?"

  wheels <- odbc::dbSendQuery(db, query)
  dbBind(wheels, list(wheel))
  data <- dbFetch(wheels)
  dbClearResult(wheels)
  data
}


#' Get results by recnum from snics_results
#'
#' @param recnum A vector of recnums
#'
#' @return A dataframe of summary statistics
#' @export
#'
getRecSR <- function(recnum) {
  db <- conNOSAMS()
  query <- glue::glue_sql("SELECT wheel, wheel_pos, sample_name,
                             tp_date_pressed, target.tp_num, target.rec_num,
                             target.osg_num, gf_devel, gf_test, ws_r_d,
                             ws_method_num, ws_line_num, fm_corr, sig_fm_corr, dc13
                          FROM snics_results
                          JOIN target ON snics_results.tp_num = target.tp_num
                          JOIN graphite ON target.osg_num = graphite.osg_num
                          JOIN dc13 ON snics_results.tp_num = dc13.tp_num
                          LEFT JOIN water_strip ON graphite.ws_num = water_strip.ws_num
                          WHERE target.rec_num IN ({recnums*})",
                          recnums = recnum,
                          .con = db
  )

  recs <- odbc::dbSendQuery(db, query)
  data <- dbFetch(recs)
  dbClearResult(recs)
  data
}

#' Get the process id for a target
#'
#' @param tp_num The numeric target identifier
#' @param .con A odbc database connection The numeric target identifier
#'
#' @return A short character code for the process
#' @export
#'
getProcess <- function(tp_num, .con = con) {
  #Check connection
  if (missing(.con) || class(.con) != "Microsoft SQL Server") {
    .con <- conNOSAMS()
  }
  # Get process id
  procid <- odbc::dbGetQuery(.con,
                glue::glue_sql("SELECT
                               [amsprod].[dbo].[fn_get_process_code] ({tp_num}); ",
                               tp_num = tp_num))

  # Get process name
  sql <- "SELECT key_short_desc
          FROM dbo.alxrefnd
          WHERE key_name = 'PROCESS_TYPE'
          AND key_cd = ?"
  query <- odbc::dbSendQuery(.con, sql)
  odbc::dbBind(query, list(procid[1,]))
  data <- odbc::dbFetch(query)
  odbc::dbClearResult(query)
  data[1,1]
}
