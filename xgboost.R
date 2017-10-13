  #Discover your data
  require(xgboost)
  require(Matrix)
  require(data.table)
  if (!require('vcd')) install.packages('vcd')
  
  data(Arthritis)
  df <- data.table(Arthritis, keep.rownames = F)
  
  head(df)
  str(df)
  
  #Grouping per 10 years
  head(df[,AgeDiscret := as.factor(round(Age/10,0))])
  
  #Random split into two groups
  head(df[,AgeCat := as.factor(ifelse(Age>30,"Old","Young"))])
  
  #Cleaning data
  df[,ID:=NULL]
  levels(df[,Treatment])
  
  #One-hot encoding
  #transform the categorical data to dummy variables
  sparse_matrix <- sparse.model.matrix(Improved~.-1, data = df)
  head(sparse_matrix)
  output_vector = df[,Improved] == "Marked"
  
  #Build the model
  bst <- xgboost(data = sparse_matrix, label = output_vector, max_depth = 4,
                 eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")
  
  #Feature importance
  #Measure feature importance
  #Build the feature importance data.table
  importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst)
  head(importance)
  
  #The column Gain provide the information we are looking for
  #As you can see, features are classified by Gain.
  
  #Improvement in the interpretability of feature importance data.table
  importanceRaw <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst, data = sparse_matrix, label = output_vector)
  
  # Cleaning for better display
  importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]
  
  head(importanceClean)
  
  #Plotting the feature importance
  xgb.plot.importance(importance_matrix = importance)
  
  #Do these results make sense?
  c2 <- chisq.test(df$Age, output_vector)
  print(c2)  #Pearson correlation between Age and illness disapearing is 35.48.
  
  c2 <- chisq.test(df$AgeDiscret, output_vector)
  print(c2)
  
  c2 <- chisq.test(df$AgeCat, output_vector)
  print(c2)
  
  #For instance, to compute a model with 1000 trees, with a 0.5 factor on sampling rows and columns:
  data(agaricus.train, package='xgboost')
  data(agaricus.test, package='xgboost')
  train <- agaricus.train
  test <- agaricus.test
  
  #Random Forest??? - 1000 trees
  bst <- xgboost(data = train$data, label = train$label, max_depth = 4, 
                 num_parallel_tree = 1000, subsample = 0.5, colsample_bytree =0.5, nrounds = 1, objective = "binary:logistic")
  
  #Boosting - 3 rounds
  bst <- xgboost(data = train$data, label = train$label, max_depth = 4, nrounds = 3, objective = "binary:logistic")
  
  #Cross Validation
  data(agaricus.train, package='xgboost')
  dtrain <- xgb.DMatrix(agaricus.train$data, label = agaricus.train$label)
  cv <- xgb.cv(data = dtrain, nrounds = 3, nthread = 2, nfold = 5, metrics = list("rmse","auc"),
               max_depth = 3, eta = 1, objective = "binary:logistic")
  print(cv)
  print(cv, verbose=TRUE)
  
  #---进行特征得分是较为常见的---#
  library(xgboost)
  library(readr)
  library(stringr)
  library(caret)
  library(car)
  
  #第二步:加载数据集
  set.seed(100)
  setwd("C:\\Users\\ts93856\\Desktop\\datasource")
  # 加载数据
  df_train = read_csv("train_users_2.csv")
  df_test = read_csv("test_users.csv")
  
  #加载数据标签
  labels = df_train['labels']
  df_train = df_train[-grep('labels', colnames(df_train))]
  
  # combine train and test data
  df_all = rbind(df_train,df_test)
  
  #---第三步:数据清洗和特征工程---#
  
  # 清洗变量 : 这里我筛选出年龄不到14岁或超过100的人
  df_all[df_all$age < 14 | df_all$age > 100,'age'] <- -1
  df_all$age[df_all$age < 0] <- mean(df_all$age[df_all$age > 0])
  
  #算法只接收数值类型，所以需要进行
  
  # 独热编码分类特征
  ohe_feats = c('gender', 'education', 'employer')
  dummies <- dummyVars(~ gender +  education + employer, data = df_all)
  df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
  df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],
                           df_all_ohe)df_all_combined$agena <- as.factor(ifelse(df_all_combined$age < 0,1,0))
  
  
  df_all_combined <- df_all_combined[,c('id',features_selected)] 
  # split train and test
  X = df_all_combined[df_all_combined$id %in% df_train$id,]
  y <- recode(labels$labels,'True'=1, 'False'=0)
  X_test = df_all_combined[df_all_combined$id %in% df_test$id,]
  
  #---第四步:调整和运行模式---#
  xgb <- xgboost(data = data.matrix(X[,-1]), 
                 label = y, 
                 eta = 0.1,
                 max_depth = 15, 
                 nround=25, 
                 subsample = 0.5,
                 colsample_bytree = 0.5,
                 seed = 1,
                 eval_metric = "merror",
                 objective = "multi:softprob",
                 num_class = 12,
                 nthread = 3
  )
  
  #---第五步:测试分数---#
  y_pred <- predict(xgb, data.matrix(X_test[, -1]))
  
  # 计算特征重要性矩阵
  importance_matrix <- xgb.importance(names, model = xgb)
  # 制图
  xgb.plot.importance(importance_matrix[1:10,])
  
  
  
  
  
  
  
  
  
  #------------------------第二个列子-------------------------#
  #dataset loading

  data(agaricus.train, package='xgboost')
  data(agaricus.test, package='xgboost')
  train <- agaricus.train
  test <- agaricus.test
  
  str(train)
  
  #---这种用列表装数据，有感觉---#
  dim(train$data)
  
  class(train$data)[1]
  class(train$label)  
  
  #-Basic Training using XGBoost-#
  #这个就是一个树的算法
  bstSparse <- xgboost(data = train$data, 
                       label = train$label, 
                       max_depth = 10000, 
                       eta = 1, 
                       nthread = 2, 
                       nrounds = 2, 
                       objective = "binary:logistic")
  
  #--xgh.DMatrix---#
  #一个用来装数据额方法
  dtrain <- xgb.DMatrix(data = train$data, label = train$label)
  bstDMatrix <- xgboost(data = dtrain, 
                        max_depth = 2, 
                        eta = 1, 
                        nthread = 2, 
                        nrounds = 2, objective = "binary:logistic")
  
  # verbose = 0, no message
  bst <- xgboost(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 0)
  
  # verbose = 1, print evaluation metric
  bst <- xgboost(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 1)
  
  # verbose = 2, also print information about tree
  bst <- xgboost(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 2)
  
  
  #---Basic predict using xgboost---#
  pred <- predict(bst, test$data)
  
  #size of the prediction vector
  print(length(pred))
  
  print(head(pred))
  #Transform the regression in a binary classification
  
  prediction <- as.numeric(pred > 0.5)
  print(head(prediction))
  
  #-Measuring model performance
  err <- mean(as.numeric(pred > 0.5) != test$label)
  print(paste("test-error=", err))
  
  dtrain <- xgb.DMatrix(data = train$data, label=train$label)
  dtest <- xgb.DMatrix(data = test$data, label=test$label)
  watchlist <- list(train=dtrain, test=dtest)
  
  bst <- xgb.train(data=dtrain, 
                   max_depth=2, 
                   eta=1, 
                   nthread = 2, 
                   nrounds=2, 
                   watchlist=watchlist, 
                   objective = "binary:logistic")
  
  bst <- xgb.train(data=dtrain, max_depth=2, 
                   eta=1, 
                   nthread = 2, 
                   nrounds=2, 
                   watchlist=watchlist, 
                   eval_metric = "error", 
                   eval_metric = "logloss", 
                   objective = "binary:logistic")
  
  
  
  #---------Linear boosting------------------#
  bst <- xgb.train(data=dtrain, 
                   booster = "gblinear", 
                   max_depth=2, 
                   nthread = 2, 
                   nrounds=2, 
                   watchlist=watchlist, 
                   eval_metric = "error", 
                   eval_metric = "logloss", 
                   objective = "binary:logistic")
  
  #--Manipulating xgb.DMatrix---#
  xgb.DMatrix.save(dtrain, "dtrain.buffer")
  
  bst <- xgb.train(data=dtrain2, 
                   max_depth=2, 
                   eta=1, 
                   nthread = 2, 
                   nrounds=2, 
                   watchlist=watchlist, 
                   objective = "binary:logistic")
  
  #Information extraction
  label = getinfo(dtest, "label")
  pred <- predict(bst, dtest)
  err <- as.numeric(sum(as.integer(pred > 0.5) != label))/length(label)
  print(paste("test-error=", err))
  
  #View feature importance/influence from the learnt model
  
  importance_matrix <- xgb.importance(model = bst)
  print(importance_matrix)
  xgb.plot.importance(importance_matrix = importance_matrix)
  
  xgb.dump(bst, with_stats = T)
  
  xgb.plot.tree(model = bst)
  
  #-Save and load models-#
  # save model to binary local file
  xgb.save(bst, "xgboost.model")
  # load binary model to R
  bst2 <- xgb.load("xgboost.model")
  pred2 <- predict(bst2, test$data)
  
  # And now the test
  print(paste("sum(abs(pred2-pred))=", sum(abs(pred2-pred))))
  # save model to R's raw vector
  rawVec <- xgb.save.raw(bst)
  
  # print class
  print(class(rawVec))
  
  
  # load binary model to R
  bst3 <- xgb.load(rawVec)
  pred3 <- predict(bst3, test$data)
  
  # pred2 should be identical to pred
  print(paste("sum(abs(pred3-pred))=", sum(abs(pred2-pred))))
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  