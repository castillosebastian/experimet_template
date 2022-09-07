#Arma la Training Strategy
#parte del dataset con feature engineering
#  1. dataset de future, donde se va a aplicar el modelo, son los datos que NO tiene clase
#  2. dataset de la train_strategy, donde estan marcados los registros de training, validation y testing
#        puede no considerar algunos meses, hacer undersampling de las clases, etc
#        hace falta mucha creatividad para esta estapa, decidir donde entrar es MUY ESTRATEGICO y esta sometido a las limitaciones de procesamiento
#  3. dataset de  train_final  donde se va a entrenar el modelo final una vez que se tengan los mejores hiperparametros de la Bayesian Optimization


#Necesita para correr en Google Cloud
# 64 GB de memoria RAM
#300 GB de espacio en el disco local
#  8 vCPU


#limpio la memoria
# rm( list=ls() )  #remove all objects
# gc()             #garbage collection

require("data.table")

# source( "~/labo/src/lib/exp_lib.r" )

#------------------------------------------------------------------------------
#particiona en el dataset una seccion  del yaml

aplicar_particion  <- function( seccion )
{
  columna_nueva  <- paste0( "part_", seccion)
  dataset[  , (columna_nueva) := 0L ]

  if( length( PARAMS$param[[seccion]]$periodos ) > 0 )
  {
    dataset[ get( PARAMS$param$const$periodo ) %in%  PARAMS$param[[seccion]]$periodos ,
             (columna_nueva)  := 1L ]
  } else {

     dataset[ get( PARAMS$param$const$periodo ) >= PARAMS$param[[seccion]]$rango$desde  &  get(PARAMS$param$const$periodo)  <= PARAMS$param[[seccion]]$rango$hasta,
              (columna_nueva)  := 1L ]

  }

  if( length( PARAMS$param[[seccion]]$excluir ) > 0 )
  {
    dataset[ get( PARAMS$param$const$periodo ) %in%  PARAMS$param[[seccion]]$excluir , 
             (columna_nueva) := 0L ]
  }


  if( "undersampling" %in% names( PARAMS$param[[seccion]] ) )
  {
    for( clase_valor  in  PARAMS$param[[seccion]]$undersampling )
    {

       dataset[ get(columna_nueva)==1L & get(PARAMS$param$const$clase) == clase_valor$clase  & part_azar > clase_valor$prob,
                (columna_nueva) := 0L  ]
                  
    }
  }

}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
#el input es SIEMPRE un dataset con Feature Engineering

setwd(PARAMS$environment$base_dir)

#cargo el dataset
nom_arch  <- paste0( PARAMS$experiment$requires,PARAMS$param$files$input$dentrada )
dataset   <- fread( nom_arch )
dataset[resultado=="",resultado:=NA]

#ordeno el dataset por <foto_mes, numero_de_cliente> 
setorderv( dataset, PARAMS$param$const$campos_sort )


#hago las particiones de cada seccion
dataset[,part_train:=ifelse(!is.na(resultado),1,0)]
dataset[,part_future:=ifelse(is.na(resultado),1,0)]

psecciones  <- paste0( "part_", PARAMS$param$const$secciones )

#genero el archivo de control, que DEBE ser revisado
tb_control  <- dataset[ , .N, 
                        psecciones]

dir.create( PARAMS$environment$exp_dir,  showWarnings = FALSE )

fwrite( tb_control,
        file= paste0(PARAMS$environment$exp_dir,PARAMS$param$files$output$control),
        sep= "\t" )



#Grabo Future
if( 0 < dataset[ part_future>0, .N ] ){
  #Grabo future
  fwrite( dataset[ part_future>0,
                   setdiff( colnames( dataset ) , 
                            c( psecciones, PARAMS$param$const$clase ) ),
                   with= FALSE ] ,
        file= paste0(PARAMS$environment$exp_dir,PARAMS$param$files$output$future_data),
        logical01 = TRUE,
        sep= "," )
}


#Grabo train_strategy
if( 0 < dataset[ part_train>0 , .N ] ){
  fwrite( dataset[ part_train>0 ,
                   setdiff( colnames( dataset ) , c("part_future") ),
                   with= FALSE ] ,
          file= paste0(PARAMS$environment$exp_dir,PARAMS$param$files$output$train_strategy),
          logical01 = TRUE,
          sep= "," )
}

