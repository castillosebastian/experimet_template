# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# el resultado queda en  el bucket en  ./exp/KA7410/ 
# son varios archivos, subirlos inteligentemente a Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")
library(caret)


#################### Parametros para cambiar ######################
setwd("/Volumes/GoogleDrive/Mi unidad/99yo/teach/DMCT-UTN/DMCT-UTN2022/datos/covid")
# path a la carpeta base
p_carpeta_base <- "./datasets"
# nombre del archivo a importar como dataset
p_archivo_dataset <- "covid_utn2022.rds"
# nombre de la columna que usare como clase
# columna_clase <- "resultado"
columna_clase <- "resultado"

p_objective <- "binary" # esto si es un problema con clase numérica. Si la clase es binaria, cambiar por "binary"
p_parametro_optimizar <- "auc" # cambiar por lo que estemos estimando. Si es un problema de clasificacion, cambiar por "auc"
# Consultar lista en https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
# IMPORTANTE: no usar los alias de esa lista, sino la denominación principal
ksemilla_azar  <- 102191  #Aqui poner la propia semilla

###################################################################


kprefijo       <- "KA741"
ksemilla_azar  <- 102191  #Aqui poner la propia semilla
# path a la carpeta base
p_carpeta_base <- "./datasets"
# nombre del archivo a importar como dataset
p_archivo_dataset <- "covid_utn2022.rds"


#donde entrenar
# kfinal_mes_desde    <- 201912        #mes desde donde entreno
# kfinal_mes_hasta    <- 202011        #mes hasta donde entreno, inclusive
# kfinal_meses_malos  <- c( 202006 )   #meses a excluir del entrenamiento

#hiperparametros de LightGBM
#aqui copiar a mano lo menor de la Bayesian Optimization
# si es de IT y le gusta automatizar todo, no proteste, ya llegara con MLOps
kmax_bin           <-    31
klearning_rate     <-     0.0236613856409593
knum_iterations    <-   549
knum_leaves        <-  10
kmin_data_in_leaf  <- 193
kfeature_fraction  <-     0.293977122248678



kexperimento   <- paste0( kprefijo, "1a" )



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
#cargo el dataset donde voy a entrenar
setwd(p_carpeta_base)
dsLearn <- readRDS(p_archivo_dataset)
dsLearn <- as.data.table(dsLearn)
#paso la clase a binaria que tome valores {0,1}  enteros
dsLearn[ , clase01 := ifelse( resultado=="muerte", 1L, 0L) ]

set.seed(ksemilla_azar)
trainIndex <- createDataPartition(dsLearn$clase01, p = 0.8,
                                  list = F,times = 1)

dtrain <- dsLearn[trainIndex,]
dtest <- dsLearn[-trainIndex,]



setwd("/Volumes/GoogleDrive/Mi unidad/99yo/teach/DMCT-UTN/DMCT-UTN2022/datos/covid")  #Establezco el Working Directory
setwd( "./exp/lightGBM" )

columna_clase <- "clase01"


#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
# dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
# campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#--------------------------------------


#establezco donde entreno
# dataset[ , train  := 0L ]
# 
# dataset[ foto_mes >= kfinal_mes_desde &
#          foto_mes <= kfinal_mes_hasta &
#          !( foto_mes %in% kfinal_meses_malos), 
#          train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
# dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./", kexperimento, "/" ), showWarnings = FALSE )
setwd( paste0("./", kexperimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO



#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(dsLearn[,!c(columna_clase,"resultado"),with=FALSE]),
                        label= dsLearn[[columna_clase]] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   metric= "auc",
                                   max_bin=            kmax_bin,
                                   learning_rate=      klearning_rate,
                                   num_iterations=     knum_iterations,
                                   num_leaves=         knum_leaves,
                                   min_data_in_leaf=   kmin_data_in_leaf,
                                   feature_fraction=   kfeature_fraction,
                                   seed=               ksemilla_azar
                                  )
                    )

#--------------------------------------
#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "impo_explog.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#--------------------------------------




#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dtest[,!c(columna_clase,"resultado"),with=FALSE])
                        ,type="prob")

#genero la tabla de entrega
tb_entrega  <-  dtest[ , list( codigo_paciente,clase01 ) ]
tb_entrega[  , pred := prediccion ]

#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion_explog.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, pred )

  
