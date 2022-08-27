## get raw data, changes the data types, save to data process files.
## final output is written to the specified directory

source(paste0(here::here(), "/main.R"))

# start activity---
log4r_info("reading dataset")

data = readRDS(paste0(RAW_DATA_DIR, "/covid_utn2022.rds"))

# basic reading formatin
log4r_info("basic reading formating")

data = data %>% ungroup()

# save log----
my_logfile <- as_tibble(readLines(my_logfile))
fwrite(my_logfile, paste0(LOGS_DIR, "/logs.txt"), append = T, sep= "\t")
