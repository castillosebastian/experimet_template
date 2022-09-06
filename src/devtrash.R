dataset = fread(paste0(PROCESSED_DATA_DIR, "/dataset_growth4.csv.gz"))
source(paste0(here::here(), "/main.R"))

exp_nombre = "001TSa"
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
  

set.seed(314)
split <-  rsample::initial_split(data = data, strata = resultado,    prop = 0.8)
train <- training(split)
test  <- testing(split)


