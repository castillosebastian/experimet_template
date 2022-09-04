library(caret)
library(lightgbm)


# prepare data 
data = fread(paste0(RAW_DATA_DIR, "/data_train.csv"))
data = data %>% 
  mutate(cultivo = as.factor(cultivo)) %>% 
  as_tibble()

indexes = createDataPartition(data$cultivo, p = .85, list = F)

# replace label data into numeric 
data$cultivo<-as.numeric(data$cultivo)-1

train = as.matrix(data[-indexes, ])
test = as.matrix(data[indexes, ])

train_x = train[, 2]
train_y = train[, 2]

test_x = test[, -2]
test_y = test[, 2]

dtrain = lgb.Dataset(train_x, label = train_y)
dtest = lgb.Dataset.create.valid(dtrain, data=test_x, label = test_y)

# set validataion data 
valids = list(test = dtest)

# define parameters
params = list(
  objective= 'multiclass',
  metric = "multi_error",
  num_class= 3
) 

# train model 
model = lgb.train(params,
                  dtrain,
                  nrounds = 100,
                  valids,
                  min_data=1,
                  learning_rate = 1,
                  early_stopping_rounds = 10)

print(model$best_score)

# prediction and accuracy check 
pred = predict(model, test_x, reshape=T)
pred_y = max.col(pred)-1

confusionMatrix(as.factor(test_y), as.factor(pred_y))

