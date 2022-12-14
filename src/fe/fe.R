# feauture engenieering

# rm( list=ls() )  #remove all objects
# gc()             #garbage collection

require("data.table")
require("Rcpp")
require("rlist")
require("yaml")

require("lightgbm")
require("ranger")
require("randomForest")

# tools----
ReportarCampos  <- function( dataset )
{
  cat( "La cantidad de campos es ", ncol(dataset) , "\n" )
}


# features functions----
AgregarMes  <- function( dataset ) {
  gc()
  dataset[  , mes := foto_mes %% 100 ]
  ReportarCampos( dataset )
}

#Elimina las variables que uno supone hace Data Drifting

DriftEliminar  <- function( dataset, variables ) {
  
  gc()
  dataset[  , c(variables) := NULL ]
  ReportarCampos( dataset )
  
}

#Autor:  Santiago Dellachiesa, UAustral 2021
#A las variables que tienen nulos, les agrega una nueva variable el dummy de si es nulo o no {0, 1}

DummiesNA  <- function( dataset ) {
  
  gc()
  nulos  <- colSums( is.na(dataset[ foto_mes %in% PARAM$const$futuro ]) )  #cuento la cantidad de nulos por columna
  colsconNA  <- names( which(  nulos > 0 ) )
  
  dataset[ , paste0( colsconNA, "_isNA") :=  lapply( .SD,  is.na ),
           .SDcols= colsconNA]
  
  ReportarCampos( dataset )
}

# Corregir meses con error

CorregirCampoMes  <- function( pcampo, pmeses ) {
  
  tbl <- dataset[  ,  list( "v1" = shift( get(pcampo), 1, type="lag" ),
                            "v2" = shift( get(pcampo), 1, type="lead" )
  ), 
  by=paciente_id ]
  
  tbl[ , paciente_id := NULL ]
  tbl[ , promedio := rowMeans( tbl,  na.rm=TRUE ) ]
  
  dataset[ ,  
           paste0(pcampo) := ifelse( !(foto_mes %in% pmeses),
                                     get( pcampo),
                                     tbl$promedio ) ]
  
}

# reemplaza cada variable ROTA  (variable, foto_mes)  con el promedio entre  ( mes_anterior, mes_posterior )

CorregirClaudioCastillo  <- function( dataset ) {
  
  # CorregirCampoMes( "internet",     c(201801,202006, 202010, 202011, 202012, 202101) )
  #...
}

#Corrige poniendo a NA las variables que en ese mes estan dañadas

CorregirNA  <- function( dataset )
{
  gc()
  #acomodo los errores del dataset
  
  # dataset[ foto_mes==201801,  internet   := NA ]
  
  ReportarCampos( dataset )
  
}

# Esta es la parte crear nuevas variables

AgregarVariables  <- function( dataset ) {
  
  gc()
  # #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  
  dataset[  , arm_x_inotropicos  := as.numeric(arm) * as.numeric(inotropicos) ]
  dataset[  , arm_sobre_inotropicos  := as.numeric(arm) / as.numeric(inotropicos) ]
   
  # #creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
  # dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
  # dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
  # dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
  # dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]
  # 
  # #variable extraida de una tesis de maestria de Irlanda
  # dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]
  # 
  # #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  # #varias formas de combinar Visa_status y Master_status
  # dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  # dataset[ , mv_status02       := Master_status +  Visa_status ]
  # dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  # dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  # dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]
  # 
  # dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
  #                                         ifelse( is.na(Master_status), 10, Master_status), 
  #                                         Visa_status)  ]
  # 
  # dataset[ , mv_status07       := ifelse( is.na(Master_status), 
  #                                         ifelse( is.na(Visa_status), 10, Visa_status), 
  #                                         Master_status)  ]
  # 
  # 
  # #combino MasterCard y Visa
  # dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
  # 
  # dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  # dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  # dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  # dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  # dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  # dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  # dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  # dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  # dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  # dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  # dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  # dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  # dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  # dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  # dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  # dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  # dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  # dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  # dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
  # 
  # #a partir de aqui juego con la suma de Mastercard y Visa
  # dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
  # dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
  # dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
  # dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
  # dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
  # dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
  # dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
  # dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
  # dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
  # dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
  # dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
  # dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
  # dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
  # dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
  # dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
  # dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]
  # 
  # #Aqui debe usted agregar sus propias nuevas variables
  # 
  # # Suma total de comisiones bancarias
  # dataset[ , total_comisiones := sum(mcomisiones, na.rm = T), by = paciente_id][]
  # dataset[ , ctotal_comisiones := rowSums( cbind( ccomisiones_mantenimiento, ccomisiones_otras), na.rm=TRUE ) ]
  # dataset[ , ratio_comisiones :=  mcomisiones  / ctotal_comisiones  ]
  # dataset[ , ratio_totalcomisiones_antiguedad  := total_comisiones / cliente_antiguedad ] # top 13
  # dataset[ , ratio_totalcomisiones_ctrx  := total_comisiones / ctrx_quarter_normalizado ]
  # dataset[ , exp_totalcomisiones := total_comisiones ^ 2 ] # top 19
  # 
  # # Suma deuda x prestamos
  # # mprestamos_personales + mprestamos_prendarios + mprestamos_hipotecarios  = monto total de 
  # # deuda restante de todos los prestamos personales del cliente
  # dataset[ , total_deuda_acumulada := rowSums( cbind( mprestamos_personales, mprestamos_prendarios, mprestamos_hipotecarios), na.rm=TRUE ) ] #top 11
  # dataset[ , ratio_deuda_acumulada_antiguedad  := total_deuda_acumulada / cliente_antiguedad ]
  # dataset[ , ratio_deuda_acumulada_cproductos_sobre_ctrx_q  := total_deuda_acumulada / ctrx_quarter_normalizado ]
  # 
  # # ahorro
  # dataset[ , total_mcaja_ahorro := sum(mcaja_ahorro, na.rm = T), by = paciente_id][]
  # dataset[ , ratio_total_mcaja_ahorro_antiguedad  := total_mcaja_ahorro / cliente_antiguedad ]
  # dataset[ , ratio_total_mcaja_ahorro_ctrx_q := total_mcaja_ahorro / ctrx_quarter_normalizado ]
  # 
  # # Ver valor monetario normalizado 
  # # Aplanar variables ej. movimientos-saldos => vairable sobre cuánto entró, cuando salio, 
  # 
  # # Cantidad de productos / edad
  # dataset[ , cproductos_sobre_edad  := cproductos / cliente_edad ]
  # dataset[ , cproductos_sobre_antiguedad  := cproductos / cliente_antiguedad ]
  # dataset[ , cproductos_sobre_ctrx_q  := cproductos / ctrx_quarter_normalizado ]
  # 
  # # Variable de Retabilidad SOBRE edad, antiguedad y ctrx
  # dataset[ , mrentabilidad_sobre_edad  := mrentabilidad / cliente_edad ]
  # dataset[ , mrentabilidad_annual_sobre_edad  :=  mrentabilidad_annual/ cliente_edad ]
  # dataset[ , mcomisiones_sobre_edad  := mcomisiones / cliente_edad ]
  # dataset[ , mactivos_margen_sobre_edad  :=  mactivos_margen/ cliente_edad ]
  # dataset[ , mpasivos_margen_sobre_edad  := mpasivos_margen / cliente_edad ]
  # 
  # dataset[ , mrentabilidad_sobre_antiguedad  := mrentabilidad / cliente_antiguedad ]
  # dataset[ , mrentabilidad_annual_sobre_antiguedad  :=  mrentabilidad_annual/ cliente_antiguedad ]
  # dataset[ , mcomisiones_sobre_antiguedad  := mcomisiones / cliente_antiguedad ]
  # dataset[ , mactivos_margen_sobre_antiguedad  :=  mactivos_margen/ cliente_antiguedad ]
  # dataset[ , mpasivos_margen_sobre_antiguedad  := mpasivos_margen / cliente_antiguedad ]
  # 
  # dataset[ , mrentabilidad_sobre_ctrx_q  := mrentabilidad / ctrx_quarter_normalizado ]
  # dataset[ , mrentabilidad_annual_sobre_ctrx_q  :=  mrentabilidad_annual/ ctrx_quarter_normalizado ]
  # dataset[ , mcomisiones_sobre_ctrx_q  := mcomisiones / ctrx_quarter_normalizado ]
  # dataset[ , mactivos_margen_sobre_ctrx_q  :=  mactivos_margen/ ctrx_quarter_normalizado ]
  # dataset[ , mpasivos_margen_sobre_ctrx_q  := mpasivos_margen / ctrx_quarter_normalizado ]
  # 
  # # Variable de Retabilidad POR edad
  # dataset[ , mrentabilidad_por_edad  := mrentabilidad * cliente_edad ]
  # dataset[ , mrentabilidad_annual_por_edad  :=  mrentabilidad_annual* cliente_edad ]
  # dataset[ , mcomisiones_por_edad  := mcomisiones * cliente_edad ]
  # dataset[ , mactivos_margen_por_edad  :=  mactivos_margen* cliente_edad ]
  # dataset[ , mpasivos_margen_por_edad  := mpasivos_margen * cliente_edad ]
  # 
  # dataset[ , mrentabilidad_por_antiguedad  := mrentabilidad * cliente_antiguedad ]
  # dataset[ , mrentabilidad_annual_por_antiguedad  :=  mrentabilidad_annual* cliente_antiguedad ]
  # dataset[ , mcomisiones_por_antiguedad  := mcomisiones * cliente_antiguedad ]
  # dataset[ , mactivos_margen_por_antiguedad  :=  mactivos_margen* cliente_antiguedad ]
  # dataset[ , mpasivos_margen_por_antiguedad  := mpasivos_margen * cliente_antiguedad ]
  # 
  # dataset[ , mrentabilidad_por_ctrx_q  := mrentabilidad * ctrx_quarter_normalizado ]
  # dataset[ , mrentabilidad_annual_por_ctrx_q  :=  mrentabilidad_annual* ctrx_quarter_normalizado ]
  # dataset[ , mcomisiones_por_ctrx_q  := mcomisiones * ctrx_quarter_normalizado ]
  # dataset[ , mactivos_margen_por_ctrx_q  :=  mactivos_margen* ctrx_quarter_normalizado ]
  # dataset[ , mpasivos_margen_por_ctrx_q  := mpasivos_margen * ctrx_quarter_normalizado ] # top 2
  # 
  # # Más sobre rentabilidad
  # dataset[ , mrentabilidad_exp  := mrentabilidad ^ 2]
  # dataset[ , mrentabilidad_annual_exp  :=  mrentabilidad_annual ^ 2 ]
  # 
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

# temp ratios----

#esta funcion supone que dataset esta ordenado por   <paciente_id, foto_mes>
#calcula el lag y el delta lag

Lags  <- function( cols, nlag, deltas ) {
  
  gc()
  sufijo  <- paste0( "_lag", nlag )
  
  dataset[ , paste0( cols, sufijo) := shift(.SD, nlag, NA, "lag"), 
           by= paciente_id, 
           .SDcols= cols]
  
  #agrego los deltas de los lags, con un "for" nada elegante
  if( deltas )
  {
    sufijodelta  <- paste0( "_delta", nlag )
    
    for( vcol in cols )
    {
      dataset[,  paste0(vcol, sufijodelta) := get( vcol)  - get(paste0( vcol, sufijo))]
    }
  }
  
  ReportarCampos( dataset )
}

#se calculan para los 6 meses previos el minimo, maximo y tendencia calculada con cuadrados minimos
#la formula de calculo de la tendencia puede verse en https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
#para la maxíma velocidad esta funcion esta escrita en lenguaje C, Autor: Gustavo Denicolay

cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
{
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( 5*n );

  for(int i = 0; i < n; i++)
  {
    //lag
    if( pdesde[i]-1 < i )  out[ i + 4*n ]  =  pcolumna[i-1] ;
    else                   out[ i + 4*n ]  =  NA_REAL ;


    int  libre    = 0 ;
    int  xvalor   = 1 ;

    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;

       if( !R_IsNA( a ) ) 
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }

       xvalor++ ;
    }

    /* Si hay al menos dos valores */
    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++)
      { 
        xsum  += x[h] ;
        ysum  += y[h] ; 
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;

        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ;
    }
    else
    {
      out[ i       ]  =  NA_REAL ; 
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ;
    }
  }

  return  out;
}')

# calcula la tendencia de las variables cols de los ultimos 6 meses
# la tendencia es la pendiente de la recta que ajusta por cuadrados minimos
# La funcionalidad de ratioavg es autoria de  Daiana Sparta,  UAustral  2021

TendenciaYmuchomas  <- function( dataset, cols, ventana=6, tendencia=TRUE, minimo=TRUE, 
                                 maximo=TRUE, promedio=TRUE, ratioavg=FALSE, ratiomax=FALSE) {
  gc()
  #Esta es la cantidad de meses que utilizo para la historia
  ventana_regresion  <- ventana
  
  last  <- nrow( dataset )
  
  #creo el vector_desde que indica cada ventana
  #de esta forma se acelera el procesamiento ya que lo hago una sola vez
  vector_ids   <- dataset$paciente_id
  
  vector_desde  <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
  vector_desde[ 1:ventana_regresion ]  <-  1
  
  for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
  for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }
  
  for(  campo  in   cols )
  {
    nueva_col     <- fhistC( dataset[ , get(campo) ], vector_desde ) 
    
    if(tendencia)  dataset[ , paste0( campo, "_tend", ventana) := nueva_col[ (0*last +1):(1*last) ]  ]
    if(minimo)     dataset[ , paste0( campo, "_min", ventana)  := nueva_col[ (1*last +1):(2*last) ]  ]
    if(maximo)     dataset[ , paste0( campo, "_max", ventana)  := nueva_col[ (2*last +1):(3*last) ]  ]
    if(promedio)   dataset[ , paste0( campo, "_avg", ventana)  := nueva_col[ (3*last +1):(4*last) ]  ]
    if(ratioavg)   dataset[ , paste0( campo, "_ratioavg", ventana)  := get(campo) /nueva_col[ (3*last +1):(4*last) ]  ]
    if(ratiomax)   dataset[ , paste0( campo, "_ratiomax", ventana)  := get(campo) /nueva_col[ (2*last +1):(3*last) ]  ]
  }
  
}

#Autor: Antonio Velazquez Bustamente,  UBA 2021

Tony  <- function( cols ) {
  
  sufijo  <- paste0( "_tony")
  
  dataset[ , paste0( cols, sufijo) := lapply( .SD,  function(x){ x/mean(x, na.rm=TRUE)} ), 
           by= foto_mes, 
           .SDcols= cols]
  
  ReportarCampos( dataset )
}

# domain functions----

VPOS_CORTE  <- c()

fganancia_lgbm_meseta  <- function(probs, datos) {
  
  vlabels  <- get_field(datos, "label")
  vpesos   <- get_field(datos, "weight")
  
  tbl  <- as.data.table( list( "prob"=probs, "gan"= ifelse( vlabels==1 & vpesos > 1, 60000, -1000 ) ) )
  
  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]
  setorder( tbl, -gan_acum )   #voy por la meseta
  
  gan  <- mean( tbl[ 1:500,  gan_acum] )  #meseta de tamaño 500
  
  pos_meseta  <- tbl[ 1:500,  median(posicion)]
  VPOS_CORTE  <<- c( VPOS_CORTE, pos_meseta )
  
  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}

GVEZ <- 1 

CanaritosImportancia  <- function( canaritos_ratio = 0.2) {
  
  # Canaritos es una funciòn muy importante porque limpia el dataset de variables sin valor. Para ello, 
  # entrena un modelo GBDT, evalùa sus resultados y eliminar todas aquellas columnas con menor importancia que 
  # la "capa de canaritos", equivalente a una serie de columnas con valores puramente random.
  # canaritos ratio = porcentaje de canaritos (columnas con valores aleatorias o ruido) respecto del total de columnas del dataset
  # año_mes_excluidos = el modelo excluye dichos meses del entrenamiento
  
  gc()
  
  ReportarCampos( dataset )
  
  dataset[ , clase01 :=  get(PARAM$const$clase)]
  
  for( i  in 1:(ncol(dataset)*canaritos_ratio))  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset))] # la magia de variables random
  
  campos_buenos  <- setdiff( colnames(dataset), c(PARAM$const$clase,"clase01" ) )
  
  azar  <- runif( nrow(dataset) )
  
  dataset[ , entrenamiento := !(foto_mes %in% PARAM$canaritos_params$meses_excluidos) & ( clase01==1 | azar < 0.10 ) ]
  
  dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ entrenamiento==TRUE, campos_buenos, with=FALSE]),
                          label=   dataset[ entrenamiento==TRUE, clase01],
                          weight=  dataset[ entrenamiento==TRUE, ifelse(resultado == 1, 1.0000001, 1.0)],
                          free_raw_data= FALSE  )
  
  dvalid  <- lgb.Dataset( data=    data.matrix(  dataset[ foto_mes==PARAM$canaritos_params$meses_validacion, campos_buenos, with=FALSE]), # ojo muy manual
                          label=   dataset[ foto_mes==PARAM$canaritos_params$meses_validacion, clase01],
                          weight=  dataset[ foto_mes==PARAM$canaritos_params$meses_validacion, ifelse(resultado == 1, 1.0000001, 1.0)],
                          free_raw_data= FALSE  )
  
  
  param <- list( objective= "binary",
                 metric= "custom",
                 first_metric_only= TRUE,
                 boost_from_average= TRUE,
                 feature_pre_filter= FALSE,
                 verbosity= -0,
                 seed= 999983,
                 max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                 min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                 lambda_l1= 0.0,         #por ahora, lo dejo fijo
                 lambda_l2= 0.0,         #por ahora, lo dejo fijo
                 max_bin= 31,            #por ahora, lo dejo fijo
                 num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                 force_row_wise= TRUE,    #para que los alumnos no se atemoricen con tantos warning
                 learning_rate= 0.065, 
                 feature_fraction= 1.0,   #lo seteo en 1 para que las primeras variables del dataset no se vean opacadas
                 min_data_in_leaf= 260,
                 num_leaves= 60,
                 # num_threads= 8,
                 early_stopping_rounds= 200 )
  
  modelo  <- lgb.train( data= dtrain,
                        valids= list( valid= dvalid ),
                        eval= fganancia_lgbm_meseta,
                        param= param,
                        verbose= -0 )
  
  tb_importancia  <- lgb.importance( model= modelo )
  tb_importancia[  , pos := .I ]
  
  fwrite( tb_importancia, 
          file= paste0(EXP_DIR, "/postcanaritos", GVEZ,"_",EXP$experiment$name, "_featuregain.txt"),
          sep= "\t" )
  
  GVEZ  <<- GVEZ + 1
  
  umbral  <- tb_importancia[ Feature %like% "canarito", median(pos) + 2 * sd(pos) ]  # Atencion corto en la mediana mas DOS desvios!!
  
  col_utiles  <- tb_importancia[ pos < umbral & !( Feature %like% "canarito"),  Feature ]
  col_utiles  <-  unique( c( col_utiles,  PARAM$const$campos_fijos ) )
  col_inutiles  <- setdiff( colnames(dataset), col_utiles )
  
  dataset[  ,  (col_inutiles) := NULL ]
  
  ReportarCampos( dataset )
}


#agrega para caca columna de cols una nueva variable _rank  que es un numero entre 0 y 1  del ranking de esa variable ese mes

Rankeador  <- function( cols )
{
  gc()
  sufijo  <- "_rank" 
  
  for( vcol in cols )
  {
    dataset[ , paste0( vcol, sufijo) := frank( get(vcol), ties.method= "random")/ .N, 
             by= foto_mes ]
  }
  
  ReportarCampos( dataset )
}


#agrega al dataset nuevas variables {0,1} que provienen de las hojas de un Random Forest

AgregaVarRandomForest  <- function( num.trees, max.depth, min.node.size, mtry) {
  
  gc()
  
  ReportarCampos( dataset )
  
  dataset[ , clase01 :=  get(PARAM$const$clase)]
  
  campos_buenos  <- setdiff( colnames(dataset), PARAM$const$clase)
  
  dataset_rf  <- copy( dataset[ , campos_buenos, with=FALSE] )
  azar  <- runif( nrow(dataset_rf) )
  dataset_rf[ , entrenamiento := !(foto_mes %in% PARAM$canaritos_params$meses_excluidos) & ( clase01==1 | azar < 0.10 ) ]
  
  #imputo los nulos, ya que ranger no acepta nulos   #Leo Breiman, ¿por que le temias a los nulos?
  dataset_rf[, names(dataset_rf) := lapply(.SD, as.numeric)]
  dataset_rf  <- na.roughfix( dataset_rf )
  
  campos_buenos  <- setdiff( colnames(dataset_rf), c(PARAM$const$clase,"entrenamiento" ) )
  
  modelo  <- ranger( formula= "clase01 ~ .",
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
  
  gc()
  
  ReportarCampos( dataset )
  
}

# RUN fe-----
# Aqui empieza el programa

## cargo el dataset----
# cambiar si entrada es csv, rds, agregar si otros

nom_arch  <-  paste0("fe-", Sys.Date())

if(stringr::str_sub(PARAM$files$input$dentrada, -3) == "csv") {
  
  dataset   <- fread( paste0(RAW_DATA_DIR, "/", PARAM$files$input$dentrada))
  
} else if(stringr::str_sub(PARAM$files$input$dentrada, -3) == "rds") {
  
  dataset = readRDS(paste0(RAW_DATA_DIR, "/", PARAM$files$input$dentrada))
  
} else {
  
  log4r_error(paste0("no se reconoce extensión del archivo. Definir funcion de lectura en ", 
                     rstudioapi::getActiveDocumentContext()$path))
}

log4r_info(paste0(EXP$experiment$name, ": start experiment. Dataset de entrada con filas = ", dim(dataset)[1], ", columnas = " , dim(dataset)[2]))

## opero sobre timestamp----
AgregarMes( dataset )  

## correcciones 1:nulos,NA,drift,agregomes,variablesmanuales----  
#ordeno el dataset por <paciente_id, foto_mes> para poder hacer lags

setorderv( dataset, PARAM$const$campos_sort )

if( length( PARAM$variablesdrift) > 0 )    DriftEliminar( dataset, PARAM$variablesdrift )

if( PARAM$dummiesNA )  DummiesNA( dataset )  #esta linea debe ir ANTES de Corregir  !!

if( PARAM$corregir == "ClaudioCastillo" )  CorregirClaudioCastillo( dataset )  #esta linea debe ir DESPUES de  DummiesNA

if( PARAM$corregir == "AsignarNA" )  CorregirNA( dataset )  #esta linea debe ir DESPUES de  DummiesNA

if( PARAM$variablesmanuales )  AgregarVariables( dataset )

## correcciones 2:lag,randomforest,otros----  

cols_lagueables  <- copy( setdiff( colnames(dataset), PARAM$const$campos_fijos ) )

for( i in 1:length( PARAM$tendenciaYmuchomas$correr ) ) {
  
  if( PARAM$tendenciaYmuchomas$correr[i] )   {
    
    #veo si tengo que ir agregando variables
    
    if( PARAM$acumulavars )  cols_lagueables  <- setdiff( colnames(dataset), PARAM$const$campos_fijos )
    
    cols_lagueables  <- intersect( colnames(dataset), cols_lagueables )
    
    TendenciaYmuchomas( dataset, 
                        cols= cols_lagueables,
                        ventana=   PARAM$tendenciaYmuchomas$ventana[i],
                        tendencia= PARAM$tendenciaYmuchomas$tendencia[i],
                        minimo=    PARAM$tendenciaYmuchomas$minimo[i],
                        maximo=    PARAM$tendenciaYmuchomas$maximo[i],
                        promedio=  PARAM$tendenciaYmuchomas$promedio[i],
                        ratioavg=  PARAM$tendenciaYmuchomas$ratioavg[i],
                        ratiomax=  PARAM$tendenciaYmuchomas$ratiomax[i]     )
    
    #elimino las variables poco importantes, para hacer lugar a las importantes
    if( PARAM$tendenciaYmuchomas$canaritos[ i ] > 0 )  CanaritosImportancia( canaritos_ratio = unlist(PARAM$tendenciaYmuchomas$canaritos[i]))
  }
}

for( i in 1:length( PARAM$lags$correr ) ) {
  
  if( PARAM$lags$correr[i] )
  {
    #veo si tengo que ir agregando variables
    if( PARAM$acumulavars )  cols_lagueables  <- setdiff( colnames(dataset), PARAM$const$campos_fijos )
    
    cols_lagueables  <- intersect( colnames(dataset), cols_lagueables )
    Lags( cols_lagueables, 
          PARAM$lags$lag[i], 
          PARAM$lags$delta[ i ] )   #calculo los lags de orden  i
    
    #elimino las variables poco importantes, para hacer lugar a las importantes
    if( PARAM$lags$canaritos[ i ] > 0 )  CanaritosImportancia( canaritos_ratio= unlist(PARAM$lags$canaritos[ i ] ))
  }
}

if( PARAM$acumulavars )  cols_lagueables  <- setdiff( colnames(dataset), PARAM$const$campos_fijos )

if( PARAM$rankeador ) {
  if( PARAM$acumulavars )  cols_lagueables  <- setdiff( colnames(dataset), PARAM$const$campos_fijos )
  
  cols_lagueables  <- intersect( colnames(dataset), cols_lagueables )
  setorderv( dataset, PARAM$const$campos_rsort )
  Rankeador( cols_lagueables )
  setorderv( dataset, PARAM$const$campos_sort )
}

if( PARAM$randomforest$correr )   AgregaVarRandomForest( PARAM$randomforest$num.trees,
                                                         PARAM$randomforest$max.depth,
                                                         PARAM$randomforest$min.node.size,
                                                         PARAM$randomforest$mtry )

if( PARAM$canaritos_final > 0  )   CanaritosImportancia( canaritos_ratio= PARAM$canaritos_final)

#dejo la clase como ultimo campo
nuevo_orden  <- c( setdiff( colnames( dataset ) , PARAM$const$clase ) , PARAM$const$clase )
setcolorder( dataset, nuevo_orden )

# Grabo el dataset  ----
fwrite( dataset,
        paste0(PROCESSED_DATA_DIR, "/", PARAM$files$output ),
        logical01= TRUE,
        sep= "," )

log4r_info(paste0(EXP$experiment$name, ": end experiment. Dataset de salida con filas = ", dim(dataset)[1], 
                  ", columnas = " , dim(dataset)[2], ". Rscript=", EXP$experiment$script ))

