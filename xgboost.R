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
  
  
  
  