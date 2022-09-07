library(data.table)

setwd("/Volumes/GoogleDrive/Mi unidad/99yo/teach/DMCT-UTN/DMCT-UTN2022/datos/covid")
setwd("./exp/03_HT/covid_01a")
log <- fread("covid_01a.txt")

log[ganancia==max(ganancia),.(seed,max_bin,learning_rate,num_iterations,num_leaves,min_data_in_leaf,feature_fraction,iteracion,ganancia)]
