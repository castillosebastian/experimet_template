#limpio la memoria
# rm( list=ls() )  #remove all objects
# gc()             #garbage collection
source(paste0(here::here(), "/main.R"))

require("data.table")
require("rlist")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

#################### Parametros para cambiar ######################

# path a la carpeta base
p_carpeta_base <- PROCESSED_DATA_DIR
# nombre del archivo a importar como dataset
p_archivo_dataset <- "dataset_growth4.csv.gz"
# nombre de la columna que usare como clase
# columna_clase <- "resultado"
columna_clase <- "resultado"
# prefijo para nombrar los archivos de salida (el .RDATA y el .txt)
p_etiqueta_archivos_salida <- "COVID05"
# parametro a minimizar (si desea maximizar debe cambiar a FALSE la linea del obj.fun)
p_objective <- "binary" # esto si es un problema con clase numérica. Si la clase es binaria, cambiar por "binary"

p_parametro_optimizar <- "average_precision" # cambiar por lo que estemos estimando. Si es un problema de clasificacion, cambiar por "auc"
# Consultar lista en https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
# IMPORTANTE: no usar los alias de esa lista, sino la denominación principal

p_minimize <- FALSE # Si se quiere maximizar un parámetro cambiar por FALSE, por ejemplo en un problema de clasificacion


###################################################################

kBO_iter  <- 100   #cantidad de iteraciones de la Optimizacion Bayesiana

#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
  makeNumericParam("learning_rate",    lower=  0.01 , upper=    0.3),
  makeNumericParam("feature_fraction", lower=  0.2  , upper=    0.5),
  makeIntegerParam("min_data_in_leaf", lower=  0    , upper= 2000),
  makeIntegerParam("num_leaves",       lower= 10L   , upper= 1000L)
)

ksemilla_azar  <- 102191  #Aqui poner la propia semilla

#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./exp/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )
  
  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )
    
    cat( linea, file=archivo )
  }
  
  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )
  
  cat( linea, file=archivo, append=TRUE )  #grabo al archivo
  
  if( verbose )  cat( linea )   #imprimo por pantalla
}

#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarParametro_lightgbm  <- function( x )
{
  gc()  #libero memoria
  
  #llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1
  
  kfolds  <- 3   # cantidad de folds para cross validation
  
  param_basicos  <- list( objective= p_objective,
                          metric= p_parametro_optimizar, # rmse o el que prefiera, definido al inicio
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          verbosity= -100,
                          seed= ksemilla_azar,
                          max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                          lambda_l1= 0.0,         #por ahora, lo dejo fijo
                          lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          max_bin= 31,            #por ahora, lo dejo fijo
                          num_iterations= 9999,    #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE    #para que los alumnos no se atemoricen con tantos warning
                          # ,min_data_in_leaf = 2
  )
  
  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(30 + 5/x$learning_rate) )
  
  param_completo  <- c( param_basicos, param_variable, x )
  
  set.seed( ksemilla_azar )
  modelocv  <- lgb.cv( data= dtrain,
                       stratified= TRUE, #sobre el cross validation
                       nfold= kfolds,    #folds del cross validation
                       param= param_completo,
                       verbose= -100
  )
  #obtengo el rmse
  parametro  <- unlist(modelocv$record_evals$valid[[p_parametro_optimizar]]$eval)[ modelocv$best_iter ]
  
  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  #param_completo["early_stopping_rounds"]  <- NULL     #elimino de la lista el componente  "early_stopping_rounds"
  
  #logueo 
  xx  <- param_completo
  xx$parametro  <- parametro   #le agrego el parametro
  xx$iteracion <- GLOBAL_iteracion
  loguear( xx, arch= klog )
  return( parametro )
}



#------------------------------------------------------------------------------
setwd(p_carpeta_base)
# Aqui el metodo corresponde a un archivo rds. Cambiar por el metodo adecuado para su tipo de archivo
dsLearn <- fread(p_archivo_dataset)
dsLearn <- as.data.table(dsLearn)

#paso la clase a binaria que tome valores {0,1}  enteros
dsLearn[ , clase01 := resultado ]
dsLearn$resultado = NULL
#dsLearn = na.omit(dsLearn[,-c('embarazo')])

#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
setwd(PROCESSED_DATA_DIR)
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/lightGBM2/", showWarnings = FALSE )
setwd(paste0(PROCESSED_DATA_DIR, "/exp/lightGBM2/"))   #Establezco el Working Directory DEL EXPERIMENTO

#en estos archivos quedan los resultados
kbayesiana  <- paste0(p_etiqueta_archivos_salida,".RDATA")
klog        <- paste0(p_etiqueta_archivos_salida,".txt")

GLOBAL_iteracion  <- 0   #inicializo la variable global

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog )
  GLOBAL_iteracion  <- nrow( tabla_log )
}


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dsLearn), c(columna_clase,"clase01") )

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dsLearn[ , campos_buenos, with=FALSE]),
                        label= dsLearn[["clase01"]] ) #,..


#Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar  <- EstimarParametro_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
  fn=       funcion_optimizar, #la funcion que voy a maximizar
  minimize= p_minimize,   #estoy Minimizando el parametro (RMSE)
  noisy=    TRUE,
  par.set=  hs,     #definido al comienzo del programa
  has.simple.signature = FALSE   #paso los parametros en una lista
)

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )


#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( kbayesiana ) ) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  print("continuo")
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}

quit( save="no" )
