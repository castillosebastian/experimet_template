#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")
library(magrittr)


#Aqui se debe poner la carpeta de la computadora local
setwd("/Volumes/GoogleDrive/Mi unidad/99yo/teach/DMCT-UTN/DMCT-UTN2022/datos/covid")


kprefijo       <- "KA741"
ksemilla_azar  <- 999983  #Aqui poner la propia semilla

#hiperparametros de LightGBM
#aqui copiar a mano lo menor de la Bayesian Optimization
# si es de IT y le gusta automatizar todo, no proteste, ya llegara con MLOps
kmax_bin           <-    31
klearning_rate     <-     0.02001339
knum_iterations    <-   1105
knum_leaves        <-  1993
kmin_data_in_leaf  <- 552
kfeature_fraction  <-     0.102304

kexperimento   <- paste0( kprefijo, "-covid_01a" )

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
#cargo el dataset donde voy a entrenar
setwd("/Volumes/GoogleDrive/Mi unidad/99yo/teach/DMCT-UTN/DMCT-UTN2022/datos/covid")
setwd("./exp/02_TS/covid_01a")
dsTrain <- fread("TS_train_strategy.csv.gz")

# Aca traer Julio para ver resultado
setwd("/Volumes/GoogleDrive/Mi unidad/99yo/teach/DMCT-UTN/DMCT-UTN2022/datos/covid")
setwd("./exp/02_TS/covid_01a")
futuro <- fread("TS_future_data.csv.gz")


setwd("/Volumes/GoogleDrive/Mi unidad/99yo/teach/DMCT-UTN/DMCT-UTN2022/datos/covid")
dir.create("./exp/lightGBM",showWarnings = F)
setwd( "./exp/lightGBM" )

columna_clase <- "resultado"
#paso la clase a binaria que tome valores {0,1}  enteros
dsTrain[ , clase01 := ifelse( resultado=="muerte", 1L, 0L) ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
# dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./", kexperimento, "/" ), showWarnings = FALSE )
setwd( paste0("./", kexperimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO


columnas_particion <- c("part_train")

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(dsTrain[,!c(columna_clase,columnas_particion,"clase01"),with=FALSE]),
                        label= dsTrain[["clase01"]] )

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
archivo_importancia  <- "impo_val.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#--------------------------------------




#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( futuro[,!c(columna_clase),with=FALSE]))


#genero la tabla de entrega
tb_entrega  <-  futuro[ , list( codigo_paciente ) ]
tb_entrega[  , pred := prediccion ]
setorder(tb_entrega,-pred)
tb_entrega[,npaciente:=1:nrow(tb_entrega)]
tb_entrega[,destino_recurso:=ifelse(npaciente<=1000,"SI","NO")]

fwrite( tb_entrega,
        file= paste0( kexperimento, ".txt" ),
        sep= "\t" )

