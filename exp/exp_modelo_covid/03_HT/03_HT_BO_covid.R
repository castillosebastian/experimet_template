require("data.table")
require("rlist")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")
#################### Parametros para cambiar ######################

# path a la carpeta base
p_carpeta_base <- PARAMS$environment$base_dir
# nombre del archivo a importar como dataset
p_archivo_dataset <- PARAMS$param$files$input$dentrada
# nombre de la columna que usare como clase
columna_clase <- PARAMS$param$const$campo_clase
# prefijo para nombrar los archivos de salida (el .RDATA y el .txt)
p_etiqueta_archivos_salida <- PARAMS$experiment$name
# parametro a minimizar (si desea maximizar debe cambiar a FALSE la linea del obj.fun)
p_objective <- "binary" # esto si es un problema con clase numérica. Si la clase es binaria, cambiar por "binary"
p_parametro_optimizar <- "custom" # cambiar por lo que estemos estimando. Si es un problema de clasificacion, cambiar por "auc"
# Consultar lista en https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
# IMPORTANTE: no usar los alias de esa lista, sino la denominación principal

p_minimize <- FALSE # Si se quiere maximizar un parámetro cambiar por FALSE, por ejemplo en un problema de clasificacion


###################################################################

kBO_iter  <- PARAMS$param$BO$iterations   #cantidad de iteraciones de la Optimizacion Bayesiana

kexperimento   <- PARAMS$experiment$name
# kbayesiana     <- "BO.RDATA"
klog           <- p_etiqueta_archivos_salida

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

parametrizar  <- function( lparam )
{
  param_fijos  <- copy( lparam )
  hs  <- list()
  
  for( param  in  names( lparam ) )
  {
    
    if( length( lparam[[ param ]] ) > 1 )
    {
      desde  <- as.numeric( lparam[[ param ]][[1]]  )
      hasta  <- as.numeric( lparam[[ param ]][[2]]  )
      
      if( length( lparam[[ param ]] ) == 2 )
      {
        hs  <- append( hs,  
                       list( makeNumericParam( param, lower= desde, upper= hasta)  ) )
      } else {
        hs  <- append( hs, 
                       list( makeIntegerParam( param, lower= desde, upper= hasta) ) )
      }
      
      param_fijos[[ param ]] <- NULL  #lo quito 
    }
  }
  
  return( list( "param_fijos" =  param_fijos,
                "paramSet"    =  hs ) )
}
#------------------------------------------------------------------------------
#esta funcion calcula internamente la ganancia de la prediccion probs
fganancia_logistic_lightgbm   <- function( probs, datos) 
{
  vclase01 <- get_field(datos,"label")
  data <-as.data.frame( cbind(clase=vclase01,probs=probs))
  data <- data[order(data$probs,decreasing = T), ] 
  
  gan <- sum(head(data$clase,n=1000)) * 0.1

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
  gc()  #libero memoria

  #llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  PROB_CORTE <<- x$prob_corte   #asigno la variable global

  kfolds  <- 5   # cantidad de folds para cross validation

  param_basicos  <- list( objective= "binary",
                          metric= "custom",
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          verbosity= -100,
                          seed= PARAMS$param$semilla,
                          max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                          lambda_l1= 0.0,         #por ahora, lo dejo fijo
                          lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          max_bin= 31,            #por ahora, lo dejo fijo
                          num_iterations= 9999,    #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE    #para que los alumnos no se atemoricen con tantos warning
                        )

  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )

  param_completo  <- c( param_basicos, param_variable, x )

  set.seed( PARAMS$param$semilla )
  modelocv  <- lgb.cv( data= dtrain,
                       eval= fganancia_logistic_lightgbm,
                       stratified= TRUE, #sobre el cross validation
                       nfold= kfolds,    #folds del cross validation
                       param= param_completo,
                       verbose= -100
                      )

  #obtengo la ganancia
  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]
  #normailizo la ganancia
  ganancia_normalizada  <-  ganancia* kfolds 

  #el lenguaje R permite asignarle ATRIBUTOS a cualquier variable
  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra

  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL     #elimino de la lista el componente  "early_stopping_rounds"

  #si es una ganancia superadora, genero e imprimo ESA importancia de variables
  if( ganancia_normalizada > GLOBAL_ganancia )
  {
    GLOBAL_ganancia  <<- ganancia_normalizada
    modelo  <-  lgb.train( data= dtrain,
                           eval= fganancia_logistic_lightgbm,
                           param= param_completo,
                           verbose= -100
                         )

    tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
    archivo_importancia  <- paste0( "impo_",  sprintf( "%03d", GLOBAL_iteracion ), ".txt" )

    fwrite( tb_importancia, 
            file= archivo_importancia, 
           sep= "\t" )

  }

  #logueo 
  xx  <- param_completo
  xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia

  xx$experimento  <- kexperimento
  xx$cols         <- ncol( dtrain )
  xx$rows         <- nrow( dtrain )

  xx$iteracion <- GLOBAL_iteracion
  
  loguear( xx, arch= klog )

  return( ganancia )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
setwd(p_carpeta_base)
setwd(PARAMS$experiment$requires)
dataset <- fread(p_archivo_dataset)

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( resultado=="muerte", 1L, 0L) ]

#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
setwd(p_carpeta_base)
dir.create( PARAMS$environment$exp_dir,  showWarnings = FALSE ) 
setwd(PARAMS$environment$exp_dir)   #Establezco el Working Directory DEL EXPERIMENTO

#Prepara todo la la Bayesian Optimization -------------------------------------
hiperparametros <- PARAMS$param[[ PARAMS$param$algoritmo ]]
apertura  <- parametrizar( hiperparametros )
param_fijos  <-  apertura$param_fijos

#en estos archivos quedan los resultados
kbayesiana  <- paste0(p_etiqueta_archivos_salida,".RDATA")
klog        <- paste0(p_etiqueta_archivos_salida,".txt")


GLOBAL_iteracion  <- 0   #inicializo la variable global
GLOBAL_ganancia   <- 0

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) ){
  tabla_log  <- fread( klog )
  GLOBAL_iteracion  <- nrow( tabla_log )
  GLOBAL_ganancia   <- tabla_log[ , max( ganancia ) ]
}


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c(columna_clase,"clase01","part_train") )


#--------------------------------------

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset[ , clase01]  )



#Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar, #la funcion que voy a maximizar
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  makeParamSet( params= apertura$paramSet ),     #definido al comienzo del programa
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
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}



