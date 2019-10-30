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
getQCTable <- function(from, to = "present", sys = "both") {

  if (missing(from)) {
    stop('argument "from" is missing, with no default')
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

  #Data to present or provided end date
  if (to != "present") {
    ts <- paste("AND target.tp_date_pressed < '", to,"' ")
  } else {
    ts <- ""
  }

  dquery <- paste("
   SELECT
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
   WHERE
      target.tp_date_pressed > '",from,"'
      ", ts, "
      ", whid, "
      --AND f_modern > -1
 ")

  #Do the queries
  db <- conNOSAMS()
  data <-odbc::dbGetQuery(db, dquery)
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
                          to = "present",
                          sys = "both",
                          getcurrents = TRUE,
                          rec = NULL,
                          osg = NULL) {

  # TODO: input validation
  if (missing(from)) {
    stop('argument "from" is missing, with no default')
  }

  #get any rec_num if requested
  if (is.null(rec)) {
    samples  <- "JOIN (SELECT rec_num, Fm_cons, d13_cons, sample_id
                          FROM standards WHERE Fm_cons IS NOT NULL)
                        AS standards
                    ON target.rec_num = standards.rec_num
                 WHERE "
  } else {
    samples  <- paste0("JOIN (SELECT rec_num, Fm_cons, d13_cons, sample_id
                          FROM standards WHERE Fm_cons IS NOT NULL)
                        AS standards
                    ON target.rec_num = standards.rec_num
                       WHERE
                         target.rec_num IN (", paste(rec, collapse = ","),")
                       AND")
  }

  # or get a list of OSG nums
  if (!is.null(osg)) {
    samples  <- paste0("JOIN (SELECT rec_num, Fm_cons, d13_cons, sample_id
                          FROM standards WHERE Fm_cons IS NOT NULL)
                        AS standards
                    ON target.rec_num = standards.rec_num
                       WHERE
                         target.osg_num IN (", paste(osg, collapse = ","),")
                       AND")
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

  #Data to present or provided end date
  if (to != "present") {
    ts <- paste0("AND target.tp_date_pressed < '", to,"' ")
  } else {
    ts <- ""
  }
  # include form to get old data (don't use snics tables)
  # need to include target_time, d13 irms, co2_yield, process
  # process id comes from fn_get_process_code(tp_num) and
  # process name comes from "SELECT key_short_desc FROM dbo.alxrefnd WHERE (key_name = 'PROCESS_TYPE') AND (key_cd = " & TargetProcNums(iTarg).ToString & ");
  dquery <- paste0(
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
    ", samples," target.tp_date_pressed > '",from,"'
    ", ts, "
    ", whid, "
    AND f_modern > -1
    "
  )

  cquery <- paste0("SELECT
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
                ", samples," ok_calc = 1
                ",whid, "
                AND target.tp_date_pressed > '",from,"'
                ", ts, "
              GROUP BY snics_raw.tp_num
              ")

  #Do the queries

  db <- conNOSAMS()
  data <- odbc::dbGetQuery(db, dquery)
  checkDB(data)

  if (getcurrents) {
    db <- conNOSAMS()
    cur <- odbc::dbGetQuery(db, cquery)
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
  query <- paste0("SELECT wheel_position,
                     cl_id, target.tp_num,
                     osg_num, target.rec_num
                   FROM wheel_pos
                   JOIN target
                   ON wheel_pos.tp_num = target.tp_num
                   JOIN logged_sample
                   ON target.rec_num = logged_sample.rec_num
                   WHERE wheel_id = '", wheel, "'")

  db <- conNOSAMS()
  d <- odbc::dbGetQuery(db, query)
  d
}


#' Get Intcal table
#'
#' @return A data frame of the Intcal table
#' @export
#'
getIntcalTable <- function() {

  #get intcal table
  db <- conNOSAMS()
  intcal <- odbc::dbGetQuery(db, paste("select * from ", "intercal_samples"))

  #create factor of tiri_id, order by Fm
  intcal <- within(intcal, name <- factor(tiri_id, levels = unique(
                   tiri_id[order(fm_consensus, tiri_id)]),ordered = TRUE))

  #Replace C-6 with new consensus from Xiaomei 2010
  intcal$fm_consensus[intcal$rec_num == 1086] <- 1.5016

  #add process type TODO: get this from db
  intps <- dplyr::select(intps, rec_num, process)
  intcal <- dplyr::inner_join(intcal, intps)

  # trim table
  intcal <- dplyr::select(intcal, rec_num, name, process, fm_consensus)
  return(intcal)
}

#' Get Standards Table
#'
#' @return A data frame of secondary standards data
#' @export
#'
getStdTable <- function() {

  #Open DB connection
  db <- conNOSAMS()
  standards <- odbc::dbGetQuery(db, paste("select * from ", "standards"))

  #add process type
  standards <- dplyr::left_join(standards, stdps, by = "rec_num")

  standards <- dplyr::mutate(standards, fm_consensus = ifelse(!is.na(Fm_cons), Fm_cons, Fm_NOSAM_avg))
  standards <- dplyr::select(standards, rec_num, sample_id, process, fm_consensus)

  # TODO: make names the same
  #create factor of tiri_id, order by Fm
  standards <- within(standards, name <- factor(sample_id, levels = unique(
                   sample_id[order(fm_consensus, sample_id)]),ordered = TRUE))

  return(standards)
}

#' Count runs
#'
#' @param from A date object or date in character form (m-d-Y)
#' @param to A date object or date in character. Defaults to present.
#' @param sys System- "USAMS", "CFAMS", defaults to both.
#' @return A list: number of runs, number of wheels
#' @export
#'
numRun <- function(from, to = "present", sys = "both") {

  if (class(to) == "Date") {
    as.character.Date(to, "%m-%d-%Y")
  }

  if (class(from) == "Date") {
    as.character.Date(from, "%m-%d-%Y")
  }

  if (as.character(to) != "present") {
    todate <- paste0("AND tp_date_pressed <= '", to, "' ")
  } else {
    todate <- ""
  }

  query <- paste0("select target.tp_num, wheel_id
          from target
          join wheel_pos on target.tp_num = wheel_pos.tp_num
          where tp_date_pressed > '", from, "' ", todate)

  db <- conNOSAMS()
  data <- odbc::dbGetQuery(db, query)
  checkDB(data)

  if (sys == "USAMS") {
    data <- dplyr::filter(data, grepl("USAMS", wheel_id))
  } else if (sys == "CFAMS") {
    data <- dplyr::filter(data, grepl("CFAMS", wheel_id))
  } else if (sys == "both") {
  } else {
    stop("Invalid sys. Use 'USAMS', 'CFAMS', or 'both'")
  }

  targets <- length(data$tp_num)
  wheels <- length(unique(data$wheel_id))
  c(targets,wheels)
}


#' Get Wheel
#'
#' @param wheel A wheel name in character format
#' @return A data frame of analysed data
#' @export
getWheel <- function(wheel) {
  query <- paste0("SELECT *
                  FROM snics_results
                  WHERE wheel = '", wheel, "'")

  db <- conNOSAMS()
  data <- odbc::dbGetQuery(db, query)
  checkDB(data)
  data
}


#' Get Raw Wheel
#'
#' @param wheel A wheel name in character format
#' @return A data frame of raw data
#' @export
getRawWheel <- function(wheel) {
  query <- paste0("SELECT *
                  FROM snics_raw
                  WHERE wheel = '", wheel, "'")

  db <- conNOSAMS()
  data <- odbc::dbGetQuery(db, query)
  checkDB(data)
  data
}

#' Get standards from a wheel
#'
#' @param wheel
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
#' @param wheel
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
#' @param recnum
#'
#' @return A dataframe of summary statistics
#' @export
#'
getRecSR <- function(recnum) {
  db <- conNOSAMS()
  query <- "SELECT wheel, wheel_pos, sample_name, tp_date_pressed, target.tp_num, target.rec_num,
                      target.osg_num, gf_devel, gf_test, ws_r_d, fm_corr, sig_fm_corr, dc13
                    FROM snics_results
                    JOIN target ON snics_results.tp_num = target.tp_num
                    JOIN graphite ON target.osg_num = graphite.osg_num
                    JOIN dc13 ON snics_results.tp_num = dc13.tp_num
                    LEFT JOIN water_strip ON graphite.ws_num = water_strip.ws_num
                    WHERE target.rec_num = ?"

  recs <- odbc::dbSendQuery(db, query)
  dbBind(recs, list(recnum))
  data <- dbFetch(recs)
  dbClearResult(recs)
  data
}
