require("data.table")
require("lightgbm")

# start activity---

# http://devseed.com/sat-ml-training/LightGBM_cropmapping

data = fread(paste0(RAW_DATA_DIR, "/data_train.csv"))

# Parametros
columna_clase <- "cultivo"

p_objective <- "multiclass" 

#p_parametro_optimizar <- "auc"
#p_parametro_optimizar <- "multi_logloss" 
p_parametro_optimizar <- "multi_error"

ksemilla_azar = 7778
# training----

dsLearn <- data

dsLearn <- as.data.table(dsLearn)

#paso la clase a binaria que tome valores {0,1}  enteros
dsLearn[ , clase01 := as.numeric(as.factor(data$cultivo))-1 ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dsLearn), c(columna_clase,"clase01") )

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dsLearn[ , campos_buenos, with=FALSE]),
                        label= dsLearn[["clase01"]] )

param_basicos  <- list( objective= p_objective,
                        metric= p_parametro_optimizar, # rmse o el que prefiera, definido al inicio
                        first_metric_only= TRUE,
                        boost_from_average= TRUE,
                        feature_pre_filter= FALSE,
                        verbosity= -100,
                        num_class= 7,
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

param_variable <- list(
  learning_rate=0.1
  ,feature_fraction=0.5
  ,min_data_in_leaf=500
  ,num_leaves=100
)

#el parametro discolo, que depende de otro
esr  <- list(  early_stopping_rounds= as.integer(50 + 5/param_variable$learning_rate) )

param_completo  <- c( param_basicos, param_variable, esr )

set.seed( ksemilla_azar )

kfolds  <- 5   # cantidad de folds para cross validation

modelocv  <- lgb.cv( data= dtrain,
                     stratified= TRUE, #sobre el cross validation
                     nfold= kfolds,    #folds del cross validation
                     param= param_completo,
                     verbose= -100
)

modelocv$record_evals$valid[[p_parametro_optimizar]]$eval
unlist(modelocv$record_evals$valid[[p_parametro_optimizar]]$eval)[ modelocv$best_iter ]



# # prediction
# pred = predict(modelocv, data_predict, reshape=T)
# pred_y = max.col(pred)-1
# # accuracy check
# confusionMatrix(as.factor(test_y), as.factor(pred_y))
# # feature importance
# tree_imp = lgb.importance(model, percentage = T)
# lgb.plot.importance(tree_imp, measure = "Gain")
