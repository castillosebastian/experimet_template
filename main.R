# principal
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, dtplyr, log4r,
               lubridate, Rcpp, yaml, rlist, ggplot2, 
               ROCR, methods, Matrix, caret, rsample)
options(scipen = 999)

# logs
my_logfile = "logfile.txt"
my_console_appender = console_appender(layout = default_log_layout())
my_file_appender = file_appender(my_logfile, append = TRUE, 
                                 layout = default_log_layout())

my_logger <- log4r::logger(threshold = "INFO", appenders= list(my_console_appender,my_file_appender))
log4r_info <- function(mess) {log4r::info(my_logger, paste0("Info_message:", mess)) }
log4r_error <- function(mess) {  log4r::error(my_logger, paste0("Error_message:", mess)) }


HOME_DIR = here::here()
RAW_DATA_DIR = paste0(HOME_DIR,  "/data/raw")
PROCESSED_DATA_DIR = paste0(HOME_DIR,  "/data/processed")
LOGS_DIR = paste0(HOME_DIR,  "/logs")
TRACKING_FILE = paste0(HOME_DIR,  "/tracking")
EXP_DIR = paste0(HOME_DIR,  "/exp")


# queda el ambiente sucio
exp_start  <- function( exp_nombre = NA) {

  #leo los parametros con los que se llamo al script  R
  if( is.na(exp_nombre ) ) {
    args  <- commandArgs( trailingOnly= TRUE )
  }  else {
    args  <- c( exp_nombre )
  }

  #el primer parametro tiene el nombre del  yml
  EXP  <- yaml::read_yaml( paste0(EXP_DIR,"/", args[1], ".yml") )
  EXP$experiment$name  <- args[1]
  PARAM  <- EXP$param
  
  source(EXP$experiment$script, local = TRUE)
  
}


# 
# my_logfile <- as_tibble(readLines(my_logfile))
# fwrite(my_logfile, paste0(LOGS_DIR, "/logs.txt"), append = T, sep= "\t")

#exp_start("001FEa")
