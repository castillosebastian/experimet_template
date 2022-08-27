pacman::p_load("tidyverse", "data.table", "dtplyr", "log4r")

# logs
my_logfile = "logfile.txt"
my_console_appender = console_appender(layout = default_log_layout())
my_file_appender = file_appender(my_logfile, append = TRUE, 
                                 layout = default_log_layout())

my_logger <- log4r::logger(threshold = "INFO", appenders= list(my_console_appender,my_file_appender))

log4r_info <- function(mess) {
  log4r::info(my_logger, paste0("Info_message:", mess))
}

log4r_error <- function(mess) {
  log4r::error(my_logger, paste0("Error_message:", mess))
}

#log4r_info("algo") 
#log4r_error("se rompio todo") 
#readLines(my_logfile)

HOME_DIR = here::here()
RAW_DATA_DIR = paste0(HOME_DIR,  "/data/raw")
PROCESSED_DATA_DIR = paste0(HOME_DIR,  "/data/processed")
LOGS_DIR = paste0(HOME_DIR,  "/logs")
TRACKING_FILE = paste0(HOME_DIR,  "/tracking")

