pacman::p_load("tidyverse", "data.table", "dtplyr", "log4r",
               "lubridate", "Rcpp", "yaml", "rlist")

# logs
my_logfile = "logfile.txt"
my_console_appender = console_appender(layout = default_log_layout())
my_file_appender = file_appender(my_logfile, append = TRUE, 
                                 layout = default_log_layout())

my_logger <- log4r::logger(threshold = "INFO", appenders= list(my_console_appender,my_file_appender))

log4r_info <- function(mess) {
  log4r::info(my_logger, paste0("Info_message:", mess))
}

log4r_error <- function(mess) {
  log4r::error(my_logger, paste0("Error_message:", mess))
}

#log4r_info("algo") 
#log4r_error("se rompio todo") 
#readLines(my_logfile)

HOME_DIR = here::here()
RAW_DATA_DIR = paste0(HOME_DIR,  "/data/raw")
PROCESSED_DATA_DIR = paste0(HOME_DIR,  "/data/processed")
LOGS_DIR = paste0(HOME_DIR,  "/logs")
TRACKING_FILE = paste0(HOME_DIR,  "/tracking")
EXP_DIR = paste0(HOME_DIR,  "/exp")

# control experimentos----
# source( "~/labo/src/lib/exp_lib.r" )
# exp_start( "001FEa" )
# exp_start( "8101TSa" )
# exp_start( "8101HTa" )
# exp_restart( "8101HTa" )
# exp_start( "8101FMa" )
# exp_start( "8101SCa" )
# exp_start( "8101KAa" )
# exp_start( "8101ZZa", deletevm= FALSE )

exp_start  <- function( exp_name= NA, deletevm= TRUE, repo_dir= HOME_DIR, exp_dir= EXP_DIR ) {
  
  exp_verificar( exp_name, repo_dir, exp_dir )
  
  #si ya existe la carpeta del experimento,  aborto
  exp_exp_dir  <- paste0( exp_dir, exp_name, "/" )
  if( dir.exists( exp_exp_dir ) )   raise_error( paste0( "debe llamar a  exp_restart() , ya existe la carpeta: " , exp_exp_dir ) )
  
  #creo la carpeta del experimento generalmente en  ~/buckets/b1/exp
  res  <- dir.create( exp_exp_dir,  showWarnings= FALSE )
  if( res == FALSE )  raise_error( paste0( "No se pudo crear la carpeta: ", exp_exp_dir )) 
  
  #creo la carpeta compartida
  
  user_dir  <- paste0( "/media/expshared/" , Sys.info()["user"] )
  dir.create( user_dir,  showWarnings= FALSE )
  
  userexp_dir  <- paste0( "/media/expshared/" , Sys.info()["user"], "/exp/" )
  dir.create( userexp_dir,  showWarnings= FALSE )
  
  shared_dir  <- paste0( "/media/expshared/" , Sys.info()["user"], "/exp/", exp_name , "/" )
  dir.create( shared_dir,  showWarnings= FALSE )
  
  #copio el archivo del experimento
  repo_exp_dir  <- paste0( repo_dir, "exp/", exp_name, "/" )
  archivo_experimento  <- paste0( exp_name, ".yml" )
  if( !file.exists( paste0( repo_exp_dir, archivo_experimento ) ) )
  {
    archivos  <- list.files( path= repo_exp_dir, pattern= "*.yml" )
    if( length( archivos ) == 0 )  raise_error( paste0( "No existe ningun archivo .yml en la carpeta : " , repo_exp_dir ) )
    if( length( archivos )  > 1 )  raise_error( paste0( "Existen muchos archivos .yml en la carpeta : " , repo_exp_dir ) )
    
    if( length( archivos ) == 1 )  archivo_experimento  <- paste0( archivos[1] )
  }
  
  archivo_original  <- paste0( repo_exp_dir, archivo_experimento )
  #copio el .yml del experimento generalmente a  ~/buckets/b1/exp/<experimento>
  res  <- file.copy( archivo_original, exp_exp_dir )
  archivo_destino  <- paste0( exp_exp_dir, archivo_experimento )
  if( res==FALSE )  raise_error( paste0( "No se pudo crear el archivo: ", archivo_destino )) 
  #dejo readonly el archivo
  Sys.chmod( archivo_destino, mode = "444", use_umask = TRUE)
  
  #copio el .yml del experimento generalmente a  ~/media/expshared/<usuario>/exp/<experimento>
  res  <- file.copy( archivo_original, shared_dir, overwrite= TRUE )
  archivo_destino  <- paste0( shared_dir, archivo_experimento )
  
  
  #me paro en la carpeta del experimento
  setwd( exp_exp_dir )
  
  #cargo el experimento
  experimento  <- read_yaml( archivo_experimento )
  script  <- paste0( experimento[["environment"]][["repo_dir"]], experimento[["experiment"]][["script"]] )
  
  #creo el shell script que voy a correr
  
  linea1  <- "tabulador=\"\t\"\n"
  linea2  <- paste0( "exp_name=", exp_name, "\n" )
  linea3  <- "echo \"experiment\ttimestamp\tevent\"  >  log.txt \n"
  linea4  <- "fecha0=$(date +\"%Y%m%d %H%M%S\") \n"
  linea5  <- "echo \"$exp_name\"\"$tabulador\"\"$fecha0\"\"$tabulador\"\"SH_START\" >> log.txt \n"
  
  linea6  <- paste0( "Rscript --vanilla ",
                     experimento[["environment"]][["repo_dir"]], 
                     "src/lib/exp_run_init.r",
                     "  ",
                     exp_name,
                     "\n" )
  
  #esta es la llamada al script de R que hace el trabajo
  linea7  <- paste0( "Rscript --vanilla ",
                     script,
                     "  " ,
                     exp_name,
                     "  2>&1 | tee outfile \n" )
  
  linea8  <- "cp  ~/log/*.txt  ./  \n"
  
  linea9  <- paste0( "Rscript --vanilla ", 
                     experimento[["environment"]][["repo_dir"]], 
                     "src/lib/exp_run_end.r",
                     "\n"  )
  
  linea10  <- "fecha1=$(date +\"%Y%m%d %H%M%S\") \n"
  linea11  <- "echo \"$exp_name\"\"$tabulador\"\"$fecha1\"\"$tabulador\"\"SH_END\" >> log.txt \n"
  
  #esta linea debe cambiarse por un rsync
  linea12  <- paste0( "find ./ ! -name \"*.gz\" ! -name . -exec cp -prt ",  shared_dir, "  {} +  \n")
  
  linea13  <- "\n#suicidio\n" 
  linea14  <- "export NAME=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/name -H 'Metadata-Flavor: Google') \n"
  linea15  <- "export ZONE=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/zone -H 'Metadata-Flavor: Google') \n"
  linea16  <- "gcloud --quiet compute instances delete $NAME --zone=$ZONE \n"
  linea_suicidio  <- paste0( linea13, linea14, linea15, linea16)
  if( deletevm==FALSE ) linea_suicidio <- "\n"
  
  comando  <- paste0( linea1, linea2, linea3, linea4, linea5,
                      linea6,
                      linea7,
                      linea8,
                      linea9, 
                      linea10, linea11, linea12, 
                      linea_suicidio )
  
  
  shell_script  <- paste0( exp_name, ".sh" )
  cat( comando, 
       file= shell_script )
  
  #doy permisos de ejecucion al shell script
  Sys.chmod( shell_script, mode = "544", use_umask = TRUE)
  
  #ejecuto en linux el script recien creado
  system( paste0( "./", shell_script ) )
}

# otras funciones: analizar para implementar----

exp_iniciar  <- function( exp_nombre = NA){

  pacman::p_load("lightgbm")

  #leo los parametros con los que se llamo al script  R
  if( is.na(exp_nombre ) ) {
    args  <- commandArgs( trailingOnly= TRUE )
  }  else {
    args  <- c( exp_nombre )
  }

  #el primer parametro tiene el nombre del  yml
  EXP  <<- yaml::read_yaml( paste0(EXP_DIR,"/", args[1], ".yml") )
  EXP$experiment$name  <<- args[1]
  PARAM  <<- EXP$param
  
  #escribo al log que comenzo el SCRIPT
  linea  <- paste0( EXP$experiment$name,
                    "\t",
                    format(Sys.time(), "%Y%m%d %H%M%S"),
                    "\t",
                    "SCRIPT_START\n" )

  log4r_info(paste0("start experiment, dim: ",linea, rstudioapi::getActiveDocumentContext()$path))

}
