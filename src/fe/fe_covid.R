## feature engineering
# El feature engineering es propio de cada dataset
# realiza Feature Engineering sobre el dataset original
# Este script con seguridad va a ser modificado de proyecto en proyecto

# notes
# 
# 1. Dataset COVID
# Ambas variables son fundamentales para ver quién se muere: 
#  ARM = asistencia respiratoria mecánica
#  Inotrópicos = estimular el corazon
# 
# Confirguraciones
# -100+ iteraciones
# -Ver si hay character convertir en ordinal
# -Implementar canaritos
# -"binary"
# -Funcion objetivo optimizar: "AUC"

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

source(paste0(here::here(), "/main.R"))
source(paste0(HOME_DIR,"/src/scripts/data_processing/process_raw_data.R"))
#dataset = readRDS(paste0(PROCESSED_DATA_DIR, "/dataset.rds"))

# tools
ReportarCampos  <- function( dataset )
{
  cat( "La cantidad de campos es ", ncol(dataset) , "\n" )
}

# start activity---
log4r_info(paste0("start fe, dim: ",dim(data), rstudioapi::getActiveDocumentContext()$path))

dataset = data %>% 
  mutate(
    # format
    date = ymd(finternacion),
    mes = month(date),
    fdiag_covid = as.numeric(ymd(fdiag_covid)),
    fini_sintomas = as.numeric(ymd(fini_sintomas)),
    finternacion = as.numeric(ymd(finternacion)),
    resultado = ifelse(resultado == "muerte", 1,0)
  ) %>% 
  mutate(
    # new feature
    durac_fdiag_finis = fini_sintomas - fdiag_covid,
    durac_finis_finter = finternacion - fini_sintomas,
    durac_fdiag_finter = finternacion - fdiag_covid,
    foto_mes = as.integer(str_remove_all(str_sub(date, 1,7), "-")),
    paciente_id = row_number(),
  ) %>% 
  select(!c("date", "codigo_paciente", "no_neumonia_cual", "metodo_otro"))



# funciones genéricas----
datasetdt = dataset %>% as.data.table()

cols_lagueables = setdiff( colnames(datasetdt), c("paciente_id", "resultado") )
cols_lagueables  <- intersect( colnames(datasetdt), cols_lagueables )

# rankeador----

Rankeador  <- function( cols )
{
  gc()
  sufijo  <- "_rank" 
  
  for( vcol in cols )
  {
    datasetdt[ , paste0( vcol, sufijo) := frank( get(vcol), ties.method= "random")/ .N, 
             by= foto_mes ]
  }
  
  ReportarCampos( datasetdt )
}

Rankeador( cols_lagueables )


# Radom Forest------

datasetdt_rf  <- datasetdt

azar  <- runif( nrow(datasetdt_rf) )

datasetdt_rf$entrenamiento = as.integer(datasetdt$resultado | azar < 0.10 )

#imputo los nulos, ya que ranger no acepta nulos
#datasetdt_rf = datasetdt_rf %>% select_if(is.numeric) %>% as.data.table()

datasetdt_rf  <- randomForest::na.roughfix( datasetdt_rf )


modelo  <- ranger::ranger( formula= "resultado ~ .",
                   data=  datasetdt_rf ,
                   classification= TRUE,
                   probability=   FALSE,
                   num.trees=     500
                   # max.depth=     max.depth,
                   # min.node.size= min.node.size,
                   # mtry=          mtry)
                   )

rfhojas  <- predict( object= modelo, 
                     data= datasetdt_rf,
                     predict.all= TRUE,    #entrega la prediccion de cada arbol
                     type= "terminalNodes" #entrega el numero de NODO el arbol
)

num.trees = 10

for( arbol in 1:num.trees )
{
  hojas_arbol  <- unique(  rfhojas$predictions[  , arbol  ] )
  
  for( pos in 1:length(hojas_arbol) )
  {
    nodo_id  <- hojas_arbol[ pos ]  #el numero de nodo de la hoja, estan salteados
    datasetdt[  ,  paste0( "rf_", sprintf( "%03d", arbol ), "_", sprintf( "%03d", nodo_id ) ) := 0L ]
    
    datasetdt[ which( rfhojas$predictions[ , arbol] == nodo_id ,  ), 
             paste0( "rf_", sprintf( "%03d", arbol ), "_", sprintf( "%03d", nodo_id ) ) := 1L ]
  }
}

rm( datasetdt_rf )
saveRDS(datasetdt, file = "dataset_big.rds", compress = T)



log4r_info(paste0("end fe, dim: ",dim(data), rstudioapi::getActiveDocumentContext()$path))


# save log----
my_logfile <- as_tibble(readLines(my_logfile))
fwrite(my_logfile, paste0(LOGS_DIR, "/logs.txt"), append = T, sep= "\t")
