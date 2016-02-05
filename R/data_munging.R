# Functions for cleaning up and calculations on data pulled from the database

mungeStandards <- function(data, std) {

  #join standard table
  data <- inner_join(data, std, by = "rec_num")

  out <- data %>%
    mutate(rep_err = pmax(f_int_error, f_ext_error),
           normFm = normFm(f_modern, fm_exp),
           sigma = sigma(f_modern, fm_exp, rep_err),
           frep_err = rep_err/f_modern,
           #system
           system = substring(wheel, 1, 5),
           #fix CFAMS 12C
           le12c = ifelse(system == "USAMS", le12c * -1, le12c),
           #is ox-i primary?
           primary = (((sample_type == "S") | (sample_type_1 == "S")) & (grepl("OX-I", name)))) %>%
    select(-target_name, -f_int_error, -f_ext_error, -sample_type,
           -sample_type_1) %>%
    #number of splits?
    group_by(osg_num) %>% #For each osg_num
    mutate(splits = n()) #Count occurrences to get number of splits

  return(out)
}


calcQC <- function(data) {
  # add calculated fields to qc table data and rename to match other tables
#Filter data
  out <- data %>%
    mutate(rep_err = pmax(f_int_error, f_ext_error),
           normFm = normFm(f_modern, fm_exp),
           sigma = sigma(f_modern, fm_exp, rep_err),
           frep_err = rep_err/f_modern,
           #system
           system = substring(wheel, 1, 5),
           #fix CFAMS 12C
           le12c = ifelse(system == "USAMS", le12c * -1, le12c),
           #is ox-i primary?
           primary = (((sample_type == "S") | (sample_type_1 == "S")) & (grepl("OX-I", name)))) %>%
    select(-target_name, -f_int_error, -f_ext_error, -sample_type,
           -sample_type_1) %>%
    #number of splits?
    group_by(osg_num) %>% #For each osg_num
    mutate(splits = n()) #Count occurrences to get number of splits

  return(out)
sec <- qc %>%
  mutate(
    system = substring(wheel, 1, 5),
    rep_err = pmax(f_int_error, f_ext_error),
    sigma = sigma(f_modern, fm_consensus, rep_err),
    normFm = normFm(f_modern, fm_consensus)) %>%

  return(sec)
}


getQCData <- function(from, to = "present", sys = "both") {
  # Function to get standards from database and return munged table

  if (missing(from)) {
    stop('argument "from" is missing, with no default')
  }

  std <- getStdTable()
  data <- getStandards(from, to, sys)
  out <- mungeStandards(data, std)
  return(out)

}

