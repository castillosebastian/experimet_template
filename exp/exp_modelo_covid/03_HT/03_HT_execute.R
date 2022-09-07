
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


library(yaml)
setwd("/Volumes/GoogleDrive/Mi unidad/99yo/teach/DMCT-UTN/DMCT-UTN2022/datos/covid/exp")
PARAMS <- yaml.load_file("./03_HT/03_HT_covid_01a.yml")
source("./03_HT/03_HT_BO_covid.R")
