#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

library(yaml)
setwd("/Volumes/GoogleDrive/Mi unidad/99yo/teach/DMCT-UTN/DMCT-UTN2022/datos/covid")
setwd("./exp/02_TS")
PARAMS <- read_yaml("02_TS_covid_01a.yml")
setwd("/Volumes/GoogleDrive/Mi unidad/99yo/teach/DMCT-UTN/DMCT-UTN2022/datos/covid")
source("./exp/02_TS/z921_TS_generico_sin_yml.r")
