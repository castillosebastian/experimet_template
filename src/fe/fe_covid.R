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
  ) %>% 
  mutate(
    # new feature
    durac_fdiag_finis = fini_sintomas - fdiag_covid,
    durac_finis_finter = finternacion - fini_sintomas,
    durac_fdiag_finter = finternacion - fdiag_covid,
  )

dataset_num = dataset %>% select_if(is.numeric)
dataset_notnum = dataset %>% select_if(~!is.numeric(.))

clase =  "resultado"
campos_rsort = c("date", "codigo_paciente" )   #por donde hago el sort inicial
campos_fijos = c("codigo_paciente", "date", "mes", "resultado")

# funciones genéricas----
dataset = as.data.table(dataset)

dataset[ , resultado:= ifelse( resultado=="noMuerte", 0, 1 ) ]

campos_buenos  <- setdiff( colnames(dataset), c("resultado" ) )

dataset_rf  <- copy( dataset[ , campos_buenos, with=FALSE] )
azar  <- runif( nrow(dataset_rf) )
dataset_rf[ , entrenamiento := as.integer( dataset$resultado | azar < 0.10 ) ]

#imputo los nulos, ya que ranger no acepta nulos
#Leo Breiman, ¿por que le temias a los nulos?
dataset_rf = dataset_rf %>% select_if(is.numeric) %>% as.data.table()

dataset_rf  <- randomForest::na.roughfix( dataset_rf )

campos_buenos  <- setdiff( colnames(dataset_rf), c("resultado","entrenamiento" ) )

modelo  <- ranger::ranger( formula= "resultado ~ .",
                   data=  dataset_rf[ entrenamiento==1L, campos_buenos, with=FALSE  ] ,
                   classification= TRUE,
                   probability=   FALSE,
                   num.trees=     num.trees,
                   max.depth=     max.depth,
                   min.node.size= min.node.size,
                   mtry=          mtry)

rfhojas  <- predict( object= modelo, 
                     data= dataset_rf[ , campos_buenos, with=FALSE ],
                     predict.all= TRUE,    #entrega la prediccion de cada arbol
                     type= "terminalNodes" #entrega el numero de NODO el arbol
)

for( arbol in 1:num.trees )
{
  hojas_arbol  <- unique(  rfhojas$predictions[  , arbol  ] )
  
  for( pos in 1:length(hojas_arbol) )
  {
    nodo_id  <- hojas_arbol[ pos ]  #el numero de nodo de la hoja, estan salteados
    dataset[  ,  paste0( "rf_", sprintf( "%03d", arbol ), "_", sprintf( "%03d", nodo_id ) ) := 0L ]
    
    dataset[ which( rfhojas$predictions[ , arbol] == nodo_id ,  ), 
             paste0( "rf_", sprintf( "%03d", arbol ), "_", sprintf( "%03d", nodo_id ) ) := 1L ]
  }
}

rm( dataset_rf )
dataset[ , clase01 := NULL ]



log4r_info(paste0("end fe, dim: ",dim(data), rstudioapi::getActiveDocumentContext()$path))


# save log----
my_logfile <- as_tibble(readLines(my_logfile))
fwrite(my_logfile, paste0(LOGS_DIR, "/logs.txt"), append = T, sep= "\t")
