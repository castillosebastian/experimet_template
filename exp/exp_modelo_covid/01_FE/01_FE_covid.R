# El feature engineering es propio de cada dataset
# realiza Feature Engineering sobre el dataset original
# Este script con seguridad va a ser modificado por los alumnos
# para agregar su propio FE, al menos en la funcion AgregarVariables

require("data.table")
require("Rcpp")
require("rlist")
require("yaml")

require("lightgbm")
require("ranger")
require("randomForest")  #solo se usa para imputar nulos

#------------------------------------------------------------------------------

ReportarCampos  <- function( dataset )
{
  cat( "La cantidad de campos es ", ncol(dataset) , "\n" )
}

#------------------------------------------------------------------------------
#Elimina las variables que uno supone hace Data Drifting

DriftEliminar  <- function( dataset, variables )
{
  gc()
  dataset[  , c(variables) := NULL ]
  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Autor:  Santiago Dellachiesa, UAustral 2021
#A las variables que tienen nulos, les agrega una nueva variable el dummy de is es nulo o no {0, 1}

DummiesNA  <- function( dataset )
{
  gc()
  nulos  <- colSums( is.na(dataset[ foto_mes %in% PARAMS$param$const$futuro ]) )  #cuento la cantidad de nulos por columna
  colsconNA  <- names( which(  nulos > 0 ) )

  dataset[ , paste0( colsconNA, "_isNA") :=  lapply( .SD,  is.na ),
             .SDcols= colsconNA]

  ReportarCampos( dataset )
}

#------------------------------------------------------------------------------
#Corrige poniendo a NA las variables que en ese paciente estan dañadas

CorregirNA  <- function( dataset )
{
  gc()
  #acomodo los errores del dataset. Rellenar el codigo  de paciente para el cual se quiera corregir. copiar esta sentencia por cada variable y paciente que se quiera  corregir.

  dataset[ codigo_paciente=="",  altura   := NA ]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Esta es la parte que los alumnos deben desplegar todo su ingenio

AgregarVariables  <- function( dataset ){
  gc()
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas

  #creo una variable que tenga un valor mas alto cuanto mayor sea la LDH y menor sea la saturacion
  dataset[!is.na(ldh)  , ldh_sat := ldh/saturacion ]
  
  #Aqui debe usted agregar sus propias nuevas variables

  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0
  }

  ReportarCampos( dataset )
}

#------------------------------------------------------------------------------
#Autor: Antonio Velazquez Bustamente,  UBA 2021

Tony  <- function( cols ){

  sufijo  <- paste0( "_tony")

  dataset[ , paste0( cols, sufijo) := lapply( .SD,  function(x){ x/mean(x, na.rm=TRUE)} ), 
             by= foto_mes, 
             .SDcols= cols]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#agrega al dataset nuevas variables {0,1} que provienen de las hojas de un Random Forest

AgregaVarRandomForest  <- function( num.trees, max.depth, min.node.size, mtry)
{
  gc()
  ReportarCampos( dataset )
  dataset[ , clase01:= ifelse( resultado=="noMuerte", 0, 1 ) ]

  campos_buenos  <- setdiff( colnames(dataset), c("resultado","codigo_paciente","fdiag_covid","metodo_otro","fini_sintomas","finternacion","no_neumonia_cual" ) )

  dataset_rf  <- copy( dataset[ , campos_buenos, with=FALSE] )
  azar  <- runif( nrow(dataset_rf) )
  dataset_rf[ , entrenamiento :=  (clase01==1 | azar < 0.10)  ]
  dataset_rf[, entrenamiento:=as.integer(entrenamiento)]

  #imputo los nulos, ya que ranger no acepta nulos
  #Leo Breiman, ¿por que le temias a los nulos?
  dataset_rf  <- na.roughfix( dataset_rf )

  campos_buenos  <- setdiff( colnames(dataset_rf), c("resultado","entrenamiento" ) )
  modelo  <- ranger( formula= "clase01 ~ .",
                     data=  dataset_rf[ entrenamiento==1L, campos_buenos, with=FALSE  ] ,
                     classification= TRUE,
                     probability=   FALSE,
                     num.trees=     num.trees,
                     max.depth=     max.depth,
                     min.node.size= min.node.size,
                     mtry=          mtry
                   )

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

  gc()
  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
#cargo el dataset
setwd(PARAMS$environment$base_dir)
setwd(PARAMS$environment$repo_dir)
nom_arch  <- PARAMS$param$files$input$dentrada
dataset   <- readRDS( nom_arch )
sinclase  <- PARAMS$param$files$input$sinclase
sinclase   <- readRDS( sinclase )
dataset <- dplyr::bind_rows(dataset,sinclase)
dataset <- as.data.table(dataset)

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( resultado=="muerte", 1L, 0L) ]

#ordeno el dataset
setorderv( dataset, PARAMS$param$const$campos_sort )


if( length( PARAMS$param$variablesdrift) > 0 )    DriftEliminar( dataset, PARAMS$param$variablesdrift )

if( PARAMS$param$dummiesNA )  DummiesNA( dataset )  #esta linea debe ir ANTES de Corregir  !!

if( PARAMS$param$corregir == "ClaudioCastillo" )  CorregirClaudioCastillo( dataset )  #esta linea debe ir DESPUES de  DummiesNA
if( PARAMS$param$corregir == "AsignarNA" )       CorregirNA( dataset )  #esta linea debe ir DESPUES de  DummiesNA

if( PARAMS$param$variablesmanuales )  AgregarVariables( dataset )


#--------------------------------------
#Esta primera parte es muuuy  artesanal  y discutible  ya que hay multiples formas de hacerlo

if( PARAMS$param$randomforest$correr )   AgregaVarRandomForest( PARAMS$param$randomforest$num.trees,
                                                         PARAMS$param$randomforest$max.depth,
                                                         PARAMS$param$randomforest$min.node.size,
                                                         PARAMS$param$randomforest$mtry
                                                        )

#dejo la clase como ultimo campo
nuevo_orden  <- c( setdiff( colnames( dataset ) , PARAMS$param$const$clase ) , PARAMS$param$const$clase )
setcolorder( dataset, nuevo_orden )

setwd(PARAMS$environment$base_dir)
setwd(PARAMS$environment$exp_dir)
dir.create(PARAMS$experiment$name,showWarnings = F)
setwd(PARAMS$experiment$name)
#Grabo el dataset    https://www.youtube.com/watch?v=66CP-pq7Cx0
fwrite( dataset,
        paste0( PARAMS$param$files$output ),
        logical01= TRUE,
        sep= "," )



