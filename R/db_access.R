# Database functions


#Open DB connection
#' Open NOSAMS DB connection
#' 
#' Takes a connection string from the CONSTRING environment variable.
#'
#' @return A RODBC db connection object
#' @export
#'
#' @examples
conNOSAMS  <- function() {
  #RODBC::odbcConnect(database, uid = uid, pwd = pwd)
  RODBC::odbcDriverConnect(Sys.getenv("CONSTRING"))
}

# get results for a tp_num

# get results for an osg_num

# get results using intcal table

# get results for a wheel


#' Get secondary data from qc table
#'
#' @param from Character vector of date in form 'YYYY-MM-DD'.
#' @param to Character vector of date in form 'YYYY-MM-DD'.
#' @param sys Character vector of system name: 'cfams', 'usams', or 'both'.
#'
#' @return
#' @export
#'
#' @examples
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
         qc.tp_num,
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

  data <- RODBC::sqlQuery(db, dquery)
  if (is.character(data)) {
    RODBC::odbcClose(db)
    stop(paste(data, collapse = "\n"))
  }

  RODBC::odbcClose(db)
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
#' @param rec numeric. If supplied, get all records matching this receipt number.
#'
#' @return
#' @export
#'
#' @examples
getStandards <- function (from, to = "present", sys = "both", getcurrents = TRUE, rec = NULL) {
  #Get data for all secondaries from database
  #Return query result as data table

  if (missing(from)) {
    stop('argument "from" is missing, with no default')
  }

  #get any rec_num if requested
  if (is.null(rec)) {
    samples  <- paste("INNER JOIN standards
                         ON target.rec_num = standards.rec_num
                       WHERE
                       ")
  } else {
    samples  <- paste("LEFT JOIN standards
                         ON target.rec_num = standards.rec_num
                       WHERE
                         target.rec_num =", rec, "
                       AND")
  }
  #What system do we want data for?
  if (sys == "cfams") {
    whid <- "AND wheel_id LIKE 'C%'"
  } else if (sys =="usams") {
    whid <- "AND wheel_id LIKE 'U%'"
  } else if (sys =="both") {
    whid <- ""
  } else {
    whid <- "AND wheel_id NOT LIKE 'C%'"
  }

  #Data to present or provided end date
  if (to != "present") {
    ts <- paste("AND target.tp_date_pressed < '", to,"' ")
  } else {
    ts <- ""
  }

  dquery <- paste("SELECT
                    target.tp_num,
                    target.tp_date_pressed, 
                    target.rec_num,
                    target.target_name,
                    target.osg_num,
                    wheel_pos.wheel_id AS wheel,
		    graphite_lab.lab_name AS lab,
                    no_os.f_modern, 
		    no_os.f_int_error, 
		    no_os.f_ext_error,
                    snics_results.int_err,
		    snics_results.ext_err,
                    no_os.dc13,
		    graphite.gf_co2_qty,
		    no_os.q_flag,
                    snics_results.sample_type,
		    snics_results.sample_type_1
                  FROM target
                    INNER JOIN no_os
                      ON target.tp_num = no_os.tp_num
                    INNER JOIN wheel_pos
                      ON target.tp_num = wheel_pos.tp_num
                    INNER JOIN snics_results
                      ON target.tp_num = snics_results.tp_num
                    INNER JOIN graphite
                      ON target.osg_num = graphite.osg_num
                    INNER JOIN graphite_lab
                      ON target.graphite_lab = graphite_lab.lab_id
                    ", samples," target.tp_date_pressed > '",from,"'
                    ", ts, "
                    ", whid, "
                    AND f_modern > -1
                  ")

  cquery <- paste("SELECT
                snics_raw.tp_num,
                AVG(le12c) AS le12c,
                SUM(cnt_14c) AS counts
              FROM snics_raw
                INNER JOIN target
                  ON snics_raw.tp_num = target.tp_num
                ", samples," ok_calc = 1
                ",whid, "
                AND target.tp_date_pressed > '",from,"'
                ", ts, "
              GROUP BY snics_raw.tp_num
              ")

  #Do the queries

  db <- conNOSAMS()

  data <- RODBC::sqlQuery(db, dquery)
  if (is.character(data)) {
    RODBC::odbcClose(db)
    stop(paste(data, collapse = "\n"))
  }

  if (!getcurrents) {
    RODBC::odbcClose(db)
    return(data)
  }

  cur <- RODBC::sqlQuery(db, cquery)
  if (is.character(cur)) {
    RODBC::odbcClose(db)
    stop(paste(cur, collapse = "\n"))
  }

  data  <- dplyr::left_join(data, cur, by = "tp_num")

  RODBC::odbcClose(db)
  return(data)

}


#' Get Intcal table
#'
#' @return
#' @export
#'
#' @examples
getIntcalTable <- function() {

  #Open DB connection
  db <- conNOSAMS()

  #get intcal table
  intcal <- RODBC::sqlQuery(db, paste("select * from ", "intercal_samples"))

  RODBC::odbcClose(db)

  #create factor of tiri_id, order by Fm
  intcal <- within(intcal, name <- factor(tiri_id, levels = unique(
                   tiri_id[order(fm_consensus, tiri_id)]),ordered = TRUE))

  #Replace C-6 with new consensus from Xiaomei 2010
  intcal$fm_consensus[intcal$rec_num == 1086] <- 1.5016

  #add in OX-I, OX-II
  intcal <- rbind(intcal, intox)

  #add process type
  intps <- dplyr::select(intps, rec_num, process)
  intcal <- dplyr::inner_join(intcal, intps)
  return(intcal)
}

#' Get Standards Table
#'
#' @return
#' @export
#'
#' @examples
getStdTable <- function() {

  #Open DB connection
  db <- conNOSAMS()
  standards <- RODBC::sqlQuery(db, paste("select * from ", "standards"))
  RODBC::odbcClose(db)

  #add process type
  standards <- dplyr::inner_join(standards, stdps, by = "rec_num")

  standards <- dplyr::mutate(standards, fm_exp = ifelse(!is.na(Fm_cons), Fm_cons, Fm_NOSAM_avg))
  standards <- dplyr::select(standards, rec_num, sample_id, process, fm_exp)

  #create factor of tiri_id, order by Fm
  standards <- within(standards, name <- factor(sample_id, levels = unique(
                   sample_id[order(fm_exp, sample_id)]),ordered = TRUE))

  return(standards)
}
