  #Starting H2O For example
  library(h2o)
  if(Sys.info()['sysname'] == "Darwin" && Sys.info()['release'] == '13.4.0'){
    quit(save="no")
  }else{
    h2o.init(nthreads = 2)
  }
  
  #example1
  h2o.init()
  irisPath <- system.file("extdata", "iris.csv", package="h2o")
  iris.hex <- h2o.importFile(path = irisPath, destination_frame = "iris.hex")
  summary(apply(iris.hex, 2, sum))
  
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  as.data.frame(prostate.hex)
  
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  prostate.hex[,2] <- as.factor(prostate.hex[,2])
  summary(prostate.hex)
  
  h2o.init()
  hi <- as.h2o(iris)
  he <- as.h2o(euro)
  hl <- as.h2o(letters)
  hm <- as.h2o(state.x77)
  hh <- as.h2o(hi)
  stopifnot(is.h2o(hi), dim(hi)==dim(iris),
            is.h2o(he), dim(he)==c(length(euro),1L),
            is.h2o(hl), dim(hl)==c(length(letters),1L),
            is.h2o(hm), dim(hm)==dim(state.x77),
            is.h2o(hh), dim(hh)==dim(hi))
  if (requireNamespace("Matrix", quietly=TRUE)) {
    data <- rep(0, 100)
    data[(1:10)^2] <- 1:10 * pi
    m <- matrix(data, ncol = 20, byrow = TRUE)
    m <- Matrix::Matrix(m, sparse = TRUE)
    hs <- as.h2o(m)
    stopifnot(is.h2o(hs), dim(hs)==dim(m))
  }
  
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  prostate.hex[,2] <- as.factor (prostate.hex[,2])
  prostate.hex[,2] <- as.numeric(prostate.hex[,2])
  
  colnames(x, do.NULL = TRUE, prefix = "col")
  
  h2o.init()
  iris.hex <- as.h2o(iris)
  dim(iris.hex)
  
  h2o.abs #Compute the absolute value of x
  h2o.abs(x);h2o.acos(x)
  h2o.aic(object, train = FALSE, valid = FALSE, xval = FALSE)
  h2o.all(x)
  h2o.anomaly(object, data, per_feature = FALSE)
  
  library(h2o)
  h2o.init()
  prosPath = system.file("extdata", "prostate.csv", package = "h2o")
  prostate.hex = h2o.importFile(path = prosPath)
  prostate.dl = h2o.deeplearning(x = 3:9, training_frame = prostate.hex, autoencoder = TRUE,
                                 hidden = c(10, 10), epochs = 5)
  prostate.anon = h2o.anomaly(prostate.dl, prostate.hex)
  head(prostate.anon)
  prostate.anon.per.feature = h2o.anomaly(prostate.dl, prostate.hex, per_feature=TRUE)
  head(prostate.anon.per.feature)
  
  library(h2o)
  h2o.init()
  irisPath <- system.file("extdata", "iris_wheader.csv", package="h2o")
  iris.hex <- h2o.importFile(path = irisPath)
  h2o.anyFactor(iris.hex)
  
  h2o.arrange(x, ...)
  h2o.ascharacter(x)
  h2o.asfactor(x)
  h2o.asnumeric(x)
  h2o.assign(data, key)
  h2o.as_date(x, format, ...)
  
  h2o.auc(object, train = FALSE, valid = FALSE, xval = FALSE)
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  hex <- h2o.uploadFile(prosPath)
  hex[,2] <- as.factor(hex[,2])
  model <- h2o.gbm(x = 3:9, y = 2, training_frame = hex, distribution = "bernoulli")
  perf <- h2o.performance(model, hex)
  h2o.auc(perf)
  
  h2o.betweenss(object, train = FALSE, valid = FALSE, xval = FALSE)
  h2o.biases(object, vector_id = 1)
  h2o.bottomN(x, column, nPercent)
  h2o.cbind(...)
  
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  prostate.cbind <- h2o.cbind(prostate.hex, prostate.hex)
  head(prostate.cbind)
  
  h2o.ceiling(x)
  h2o.centers(object)
  h2o.centersSTD(object)
  h2o.centroid_stats(object, train = FALSE, valid = FALSE, xval = FALSE)
  h2o.clearLog()  #清除记录
  
  library(h2o)
  h2o.init()
  h2o.startLogging()
  ausPath = system.file("extdata", "australia.csv", package="h2o")
  australia.hex = h2o.importFile(path = ausPath)
  h2o.stopLogging()
  h2o.clearLog()
  
  h2o.clusterInfo()
  h2o.clusterIsUp(conn = h2o.getConnection())
  h2o.clusterStatus()  #集群的状态
  
  h2o.init()
  h2o.clusterStatus()
  h2o.cluster_sizes(object, train = FALSE, valid = FALSE, xval = FALSE)
  h2o.coef(object)
  h2o.coef_norm(object)
  h2o.colnames(x)
  h2o.columns_by_type(object, coltype = "numeric", ...)
  
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  h2o.columns_by_type(prostate.hex,coltype="numeric")
  
  h2o.computeGram(X, weights = "", use_all_factor_levels = FALSE,
                  standardize = TRUE, skip_missing = FALSE)
  h2o.confusionMatrix(object, ...)
  ## S4 method for signature 'H2OModel'
  h2o.confusionMatrix(object, newdata, valid = FALSE, ...)
  ## S4 method for signature 'H2OModelMetrics'
  h2o.confusionMatrix(object, thresholds = NULL,
                      metrics = NULL)
  
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  hex <- h2o.uploadFile(prosPath)
  hex[,2] <- as.factor(hex[,2])
  model <- h2o.gbm(x = 3:9, y = 2, training_frame = hex, distribution = "bernoulli")
  h2o.confusionMatrix(model, hex)
  # Generating a ModelMetrics object
  perf <- h2o.performance(model, hex)
  h2o.confusionMatrix(perf)
  
  h2o.connect #Connect to a running H2O instance.
  
  h2o.connect(ip = "localhost", port = 54321, strict_version_check = TRUE,
              proxy = NA_character_, https = FALSE, insecure = FALSE,
              username = NA_character_, password = NA_character_,
              cookies = NA_character_, context_path = NA_character_, config = NULL)
  
  ## Not run:
  library(h2o)
  # Try to connect to a H2O instance running at http://localhost:54321/cluster_X
  # If not found, start a local H2O instance from R with the default settings.
  #h2o.connect(ip = "localhost", port = 54321, context_path = "cluster_X")
  # Or
  #config = list(ip = "localhost", port = 54321, context_path = "cluster_X")
  #h2o.connect(config = config)
  # Skip strict version check during connecting to the instance
  #h2o.connect(config = c(strict_version_check = FALSE, config))
  ## End(Not run)
  
  h2o.cor(x, y = NULL, na.rm = FALSE, use)
  
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  cor(prostate.hex$AGE)
  h2o.cos(x)
  h2o.cosh(x)
  h2o.createFrame(rows = 10000, cols = 10, randomize = TRUE, value = 0,
                  real_range = 100, categorical_fraction = 0.2, factors = 100,
                  integer_fraction = 0.2, integer_range = 100, binary_fraction = 0.1,
                  binary_ones_fraction = 0.02, time_fraction = 0, string_fraction = 0,
                  missing_fraction = 0.01, response_factors = 2, has_response = FALSE,
                  seed, seed_for_column_types)
  
  library(h2o)
  h2o.init()
  hex <- h2o.createFrame(rows = 1000, cols = 100, categorical_fraction = 0.1,
                         factors = 5, integer_fraction = 0.5, integer_range = 1,
                         has_response = TRUE)
  head(hex)
  summary(hex)
  hex2 <- h2o.createFrame(rows = 100, cols = 10, randomize = FALSE, value = 5,
                          categorical_fraction = 0, integer_fraction = 0)
  summary(hex2)
  
  h2o.cross_validation_holdout_predictions(object)
  h2o.cross_validation_models(object)
  h2o.cross_validation_predictions(object)
  h2o.cummax(x, axis = 0)
  h2o.cummin(x, axis = 0)
  h2o.cumprod(x, axis = 0)
  h2o.cumsum(x, axis = 0)
  h2o.cut(x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE,
          dig.lab = 3, ...)
  
  library(h2o)
  h2o.init()
  irisPath <- system.file("extdata", "iris_wheader.csv", package="h2o")
  iris.hex <- h2o.uploadFile(path = irisPath, destination_frame = "iris.hex")
  summary(iris.hex)
  # Cut sepal length column into intervals determined by min/max/quantiles
  sepal_len.cut <- cut(iris.hex$sepal_len, c(4.2, 4.8, 5.8, 6, 8))
  head(sepal_len.cut)
  summary(sepal_len.cut)
  
  h2o.day(x)
  h2o.dayOfWeek(x)
  h2o.dct(data, destination_frame, dimensions, inverse = FALSE)
  
  library(h2o)
  h2o.init()
  df <- h2o.createFrame(rows = 1000, cols = 8*16*24,
                        categorical_fraction = 0, integer_fraction = 0, missing_fraction = 0)
  df1 <- h2o.dct(data=df, dimensions=c(8*16*24,1,1))
  df2 <- h2o.dct(data=df1,dimensions=c(8*16*24,1,1),inverse=TRUE)
  max(abs(df1-df2))
  df1 <- h2o.dct(data=df, dimensions=c(8*16,24,1))
  df2 <- h2o.dct(data=df1,dimensions=c(8*16,24,1),inverse=TRUE)
  max(abs(df1-df2))
  df1 <- h2o.dct(data=df, dimensions=c(8,16,24))
  df2 <- h2o.dct(data=df1,dimensions=c(8,16,24),inverse=TRUE)
  max(abs(df1-df2))
  
  h2o.ddply(X, .variables, FUN, ..., .progress = "none")
  
  library(h2o)
  h2o.init()
  # Import iris dataset to H2O
  irisPath <- system.file("extdata", "iris_wheader.csv", package = "h2o")
  iris.hex <- h2o.uploadFile(path = irisPath, destination_frame = "iris.hex")
  # Add function taking mean of sepal_len column
  fun <- function(df) { sum(df[,1], na.rm = TRUE)/nrow(df) }
  # Apply function to groups by class of flower
  # uses h2o's ddply, since iris.hex is an H2OFrame object
  res <- h2o.ddply(iris.hex, "class", fun)
  head(res)
  
  h2o.deepfeatures(object, data, layer) #Feature Generation via H2O Deep Learning or DeepWater Model
  link{h2o.deeplearning} #for making H2O Deep Learning models
  
  library(h2o)
  h2o.init()
  prosPath = system.file("extdata", "prostate.csv", package = "h2o")
  prostate.hex = h2o.importFile(path = prosPath)
  prostate.dl = h2o.deeplearning(x = 3:9, y = 2, training_frame = prostate.hex,
                                 hidden = c(100, 200), epochs = 5)
  prostate.deepfeatures_layer1 = h2o.deepfeatures(prostate.dl, prostate.hex, layer = 1)
  prostate.deepfeatures_layer2 = h2o.deepfeatures(prostate.dl, prostate.hex, layer = 2)
  head(prostate.deepfeatures_layer1)
  head(prostate.deepfeatures_layer2)
  #if (h2o.deepwater.available()) {
  # prostate.dl = h2o.deepwater(x = 3:9, y = 2, backend="mxnet", training_frame = prostate.hex,
  # hidden = c(100, 200), epochs = 5)
  # prostate.deepfeatures_layer1 =
  # h2o.deepfeatures(prostate.dl, prostate.hex, layer = "fc1_w")
  # prostate.deepfeatures_layer2 =
  # h2o.deepfeatures(prostate.dl, prostate.hex, layer = "fc2_w")
  # head(prostate.deepfeatures_layer1)
  # head(prostate.deepfeatures_layer2)
  #}
           
  h2o.deeplearning(x, y, training_frame, model_id = NULL,
                   validation_frame = NULL, nfolds = 0,
                   keep_cross_validation_predictions = FALSE,
                   keep_cross_validation_fold_assignment = FALSE, fold_assignment = c("AUTO",
                                                                                      "Random", "Modulo", "Stratified"), fold_column = NULL,
                   ignore_const_cols = TRUE, score_each_iteration = FALSE,
                   weights_column = NULL, offset_column = NULL, balance_classes = FALSE,
                   class_sampling_factors = NULL, max_after_balance_size = 5,
                   max_hit_ratio_k = 0, checkpoint = NULL, pretrained_autoencoder = NULL,
                   overwrite_with_best_model = TRUE, use_all_factor_levels = TRUE,
                   standardize = TRUE, activation = c("Tanh", "TanhWithDropout", "Rectifier",
                                                      "RectifierWithDropout", "Maxout", "MaxoutWithDropout"), hidden = c(200,200), epochs = 10, train_samples_per_iteration = -2,
                   target_ratio_comm_to_comp = 0.05, seed = -1, adaptive_rate = TRUE,
                   rho = 0.99, epsilon = 1e-08, rate = 0.005, rate_annealing = 1e-06,
                   rate_decay = 1, momentum_start = 0, momentum_ramp = 1e+06,
                   momentum_stable = 0, nesterov_accelerated_gradient = TRUE,
                   input_dropout_ratio = 0, hidden_dropout_ratios = NULL, l1 = 0, l2 = 0,
                   max_w2 = 3.4028235e+38, initial_weight_distribution = c("UniformAdaptive",
                                                                           "Uniform", "Normal"), initial_weight_scale = 1, initial_weights = NULL,
                   initial_biases = NULL, loss = c("Automatic", "CrossEntropy", "Quadratic",
                                                   "Huber", "Absolute", "Quantile"), distribution = c("AUTO", "bernoulli",
                                                                                                      "multinomial", "gaussian", "poisson", "gamma", "tweedie", "laplace",
                                                                                                      "quantile", "huber"), quantile_alpha = 0.5, tweedie_power = 1.5,
                   huber_alpha = 0.9, score_interval = 5, score_training_samples = 10000,
                   score_validation_samples = 0, score_duty_cycle = 0.1,
                   classification_stop = 0, regression_stop = 1e-06, stopping_rounds = 5,
                   stopping_metric = c("AUTO", "deviance", "logloss", "MSE", "RMSE", "MAE",
                                       "RMSLE", "AUC", "lift_top_group", "misclassification",
                                       "mean_per_class_error"), stopping_tolerance = 0, max_runtime_secs = 0,
                   score_validation_sampling = c("Uniform", "Stratified"),
                   diagnostics = TRUE, fast_mode = TRUE, force_load_balance = TRUE,
                   variable_importances = TRUE, replicate_training_data = TRUE,
                   single_node_mode = FALSE, shuffle_training_data = FALSE,
                   missing_values_handling = c("MeanImputation", "Skip"), quiet_mode = FALSE,
                   autoencoder = FALSE, sparse = FALSE, col_major = FALSE,
                   average_activation = 0, sparsity_beta = 0,
                   max_categorical_features = 2147483647, reproducible = FALSE,
                   export_weights_and_biases = FALSE, mini_batch_size = 1,
                   categorical_encoding = c("AUTO", "Enum", "OneHotInternal", "OneHotExplicit",
                                            "Binary", "Eigen", "LabelEncoder", "SortByResponse", "EnumLimited"),
                   elastic_averaging = FALSE, elastic_averaging_moving_rate = 0.9,
                   elastic_averaging_regularization = 0.001)
  
  library(h2o)
  h2o.init()
  iris.hex <- as.h2o(iris)
  iris.dl <- h2o.deeplearning(x = 1:4, y = 5, training_frame = iris.hex)
  # now make a prediction
  predictions <- h2o.predict(iris.dl, iris.hex)
  
  h2o.deepwater #Build a Deep Learning model using multiple native GPU backends
  #Builds a deep neural network on an H2OFrame containing various
  #data sources
  h2o.deepwater(x, y, training_frame, model_id = NULL, checkpoint = NULL,
                autoencoder = FALSE, validation_frame = NULL, nfolds = 0,
                balance_classes = FALSE, max_after_balance_size = 5,
                class_sampling_factors = NULL, keep_cross_validation_predictions = FALSE,
                keep_cross_validation_fold_assignment = FALSE, fold_assignment = c("AUTO",
                                                                                   "Random", "Modulo", "Stratified"), fold_column = NULL,
                offset_column = NULL, weights_column = NULL,
                score_each_iteration = FALSE, categorical_encoding = c("AUTO", "Enum",
                                                                       "OneHotInternal", "OneHotExplicit", "Binary", "Eigen", "LabelEncoder",
                                                                       "SortByResponse", "EnumLimited"), overwrite_with_best_model = TRUE,
                epochs = 10, train_samples_per_iteration = -2,
                target_ratio_comm_to_comp = 0.05, seed = -1, standardize = TRUE,
                learning_rate = 0.001, learning_rate_annealing = 1e-06,
                momentum_start = 0.9, momentum_ramp = 10000, momentum_stable = 0.9,
                distribution = c("AUTO", "bernoulli", "multinomial", "gaussian", "poisson",
                                 "gamma", "tweedie", "laplace", "quantile", "huber"), score_interval = 5,
                score_training_samples = 10000, score_validation_samples = 0,
                score_duty_cycle = 0.1, classification_stop = 0, regression_stop = 0,
                stopping_rounds = 5, stopping_metric = c("AUTO", "deviance", "logloss",
                                                         "MSE", "RMSE", "MAE", "RMSLE", "AUC", "lift_top_group", "misclassification",
                                                         "mean_per_class_error"), stopping_tolerance = 0, max_runtime_secs = 0,
                ignore_const_cols = TRUE, shuffle_training_data = TRUE,
                mini_batch_size = 32, clip_gradient = 10, network = c("auto", "user",
                                                                      "lenet", "alexnet", "vgg", "googlenet", "inception_bn", "resnet"),
                backend = c("mxnet", "caffe", "tensorflow"), image_shape = c(0, 0),
                channels = 3, sparse = FALSE, gpu = TRUE, device_id = c(0),
                cache_data = TRUE, network_definition_file = NULL,
                network_parameters_file = NULL, mean_image_file = NULL,
                export_native_parameters_prefix = NULL, activation = c("Rectifier",
                                                                       "Tanh"), hidden = NULL, input_dropout_ratio = 0,
                hidden_dropout_ratios = NULL, problem_type = c("auto", "image",
                                                               "dataset"))
  
  h2o.deepwater.available  #Ask the H2O server whether a Deep Water model can be built (depends
  #on availability of native backends) Returns TRUE if a Deep Water
  #model can be built, or FALSE otherwise.
  h2o.deepwater.available(h2oRestApiVersion = .h2o.__REST_API_VERSION)
  h2o.describe(frame)
  
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.importFile(path = prosPath)
  h2o.describe(prostate.hex)
  
  h2o.difflag1(object)
  h2o.dim(x)
  h2o.dimnames(x)
  
  h2o.distance(x, y, measure)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  h2o.distance(prostate.hex[11:30,], prostate.hex[1:10,], "cosine")
  
  h2o.downloadAllLogs #Download H2O Log Files to Disk
  h2o.downloadAllLogs(dirname='./your_directory_name/', filename = 'autoh2o_log.zip')
  h2o.downloadCSV #Download H2O Data to Disk
  h2o.downloadCSV(data, filename)
  
  library(h2o)
  h2o.init()
  irisPath <- system.file("extdata", "iris_wheader.csv", package = "h2o")
  iris.hex <- h2o.uploadFile(path = irisPath)
  myFile <- paste(getwd(), "my_iris_file.csv", sep = .Platform$file.sep)
  h2o.downloadCSV(iris.hex, myFile)
  file.info(myFile)
  file.remove(myFile)
  
  h2o.download_mojo #Download the model in MOJO format
  h2o.download_mojo(model, path = getwd(), get_genmodel_jar = FALSE,
                    genmodel_name = "")
  
  h <- h2o.init()
  fr <- as.h2o(iris)
  my_model <- h2o.gbm(x=1:4, y=5, training_frame=fr)
  h2o.download_mojo(my_model) # save to the current working directory
  
  h2o.download_pojo  #Download the Scoring POJO (Plain Old Java Object) of an H2O Model
  h2o.download_pojo(model, path = NULL, getjar = NULL, get_jar = TRUE,
                    jar_name = "")
  
  library(h2o)
  h <- h2o.init()
  fr <- as.h2o(iris)
  my_model <- h2o.gbm(x=1:4, y=5, training_frame=fr)
  h2o.download_pojo(my_model) # print the model to screen
  # h2o.download_pojo(my_model, getwd()) # save the POJO and jar file to the current working
  # directory, NOT RUN
  # h2o.download_pojo(my_model, getwd(), get_jar = FALSE ) # save only the POJO to the current
  # working directory, NOT RUN
  h2o.download_pojo(my_model, getwd()) # save to the current working directory
  
  #h2o.entropy Shannon entropy
  library(h2o)
  h2o.init()
  buys <- as.h2o(c("no", "no", "yes", "yes", "yes", "no", "yes", "no", "yes", "yes","no"))
  buys_entropy <- h2o.entropy(buys)
  
  h2o.exp(x)
  h2o.exportFile(data, path, force = FALSE, parts = 1)
  ## Not run:
  library(h2o)
  h2o.init()
  irisPath <- system.file("extdata", "iris.csv", package = "h2o")
  iris.hex <- h2o.uploadFile(path = irisPath)
  # These aren't real paths
  # h2o.exportFile(iris.hex, path = "/path/on/h2o/server/filesystem/iris.csv")
  # h2o.exportFile(iris.hex, path = "hdfs://path/in/hdfs/iris.csv")
  # h2o.exportFile(iris.hex, path = "s3n://path/in/s3/iris.csv")
  ## End(Not run)
  
  h2o.exportHDFS(object, path, force = FALSE)
  h2o.fillna(x, method = "forward", axis = 1, maxlen = 1L)
  library(h2o)
  h2o.init()
  fr.with.nas = h2o.createFrame(categorical_fraction=0.0,missing_fraction=0.7,rows=6,cols=2,seed=123)
  fr <- h2o.fillna(fr.with.nas, "forward", axis=1, maxlen=2L)
  h2o.filterNACols(data, frac = 0.2)
  
  h2o.findSynonyms(word2vec, word, count = 20)
  h2o.find_row_by_threshold(object, threshold)
  h2o.find_threshold_by_max_metric(object, metric)
  h2o.floor(x)
  h2o.flow()  #Open H2O Flow in your browser
  h2o.gainsLift #Access H2O Gains/Lift Tables
  h2o.gainsLift(object, ...)
  ## S4 method for signature 'H2OModel'
  h2o.gainsLift(object, newdata, valid = FALSE,
                xval = FALSE, ...)
  ## S4 method for signature 'H2OModelMetrics'
  h2o.gainsLift(object)
  
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  hex <- h2o.uploadFile(prosPath)
  hex[,2] <- as.factor(hex[,2])
  model <- h2o.gbm(x = 3:9, y = 2, distribution = "bernoulli",
                   training_frame = hex, validation_frame = hex, nfolds=3)
  h2o.gainsLift(model) ## extract training metrics
  h2o.gainsLift(model, valid=TRUE) ## extract validation metrics (here: the same)
  h2o.gainsLift(model, xval =TRUE) ## extract cross-validation metrics
  h2o.gainsLift(model, newdata=hex) ## score on new data (here: the same)
  # Generating a ModelMetrics object
  perf <- h2o.performance(model, hex)
  h2o.gainsLift(perf) ## extract from existing metrics object
  
  
  
  h2o.gbm #Builds gradient boosted classification trees and gradient boosted regression
  #trees on a parsed data set.
  
  h2o.gbm(x, y, training_frame, model_id = NULL, validation_frame = NULL,
          nfolds = 0, keep_cross_validation_predictions = FALSE,
          keep_cross_validation_fold_assignment = FALSE,
          score_each_iteration = FALSE, score_tree_interval = 0,
          fold_assignment = c("AUTO", "Random", "Modulo", "Stratified"),
          fold_column = NULL, ignore_const_cols = TRUE, offset_column = NULL,
          weights_column = NULL, balance_classes = FALSE,
          class_sampling_factors = NULL, max_after_balance_size = 5,
          max_hit_ratio_k = 0, ntrees = 50, max_depth = 5, min_rows = 10,
          nbins = 20, nbins_top_level = 1024, nbins_cats = 1024,
          r2_stopping = Inf, stopping_rounds = 0, stopping_metric = c("AUTO",
                                                                      "deviance", "logloss", "MSE", "RMSE", "MAE", "RMSLE", "AUC", "lift_top_group",
                                                                      "misclassification", "mean_per_class_error"), stopping_tolerance = 0.001,
          max_runtime_secs = 0, seed = -1, build_tree_one_node = FALSE,
          learn_rate = 0.1, learn_rate_annealing = 1, distribution = c("AUTO",
                                                                       "bernoulli", "multinomial", "gaussian", "poisson", "gamma", "tweedie",
                                                                       "laplace", "quantile", "huber"), quantile_alpha = 0.5,
          tweedie_power = 1.5, huber_alpha = 0.9, checkpoint = NULL,
          sample_rate = 1, sample_rate_per_class = NULL, col_sample_rate = 1,
          col_sample_rate_change_per_level = 1, col_sample_rate_per_tree = 1,
          min_split_improvement = 1e-05, histogram_type = c("AUTO",
                                                            "UniformAdaptive", "Random", "QuantilesGlobal", "RoundRobin"),
          max_abs_leafnode_pred = Inf, pred_noise_bandwidth = 0,
          categorical_encoding = c("AUTO", "Enum", "OneHotInternal", "OneHotExplicit",
                                   "Binary", "Eigen", "LabelEncoder", "SortByResponse", "EnumLimited"),
          calibrate_model = FALSE, calibration_frame = NULL)
  
  library(h2o)
  h2o.init()
  # Run regression GBM on australia.hex data
  ausPath <- system.file("extdata", "australia.csv", package="h2o")
  australia.hex <- h2o.uploadFile(path = ausPath)
  independent <- c("premax", "salmax","minairtemp", "maxairtemp", "maxsst",
                   "maxsoilmoist", "Max_czcs")
  dependent <- "runoffnew"
  h2o.gbm(y = dependent, x = independent, training_frame = australia.hex,
          ntrees = 3, max_depth = 3, min_rows = 2)
  
  h2o.getConnection  #Retrieve an H2O Connection
  h2o.getFutureModel #Get future model
  h2o.getGLMFullRegularizationPath
  #Extract full regularization path from glm model (assuming it was run
                                                   #with lambda search option)
  h2o.getGLMFullRegularizationPath(model)
  h2o.getGrid  #Get a grid object from H2O distributed K/V store.
  h2o.getGrid(grid_id, sort_by, decreasing)
  
  library(h2o)
  library(jsonlite)
  h2o.init()
  iris.hex <- as.h2o(iris)
  h2o.grid("gbm", grid_id = "gbm_grid_id", x = c(1:4), y = 5,
           training_frame = iris.hex, hyper_params = list(ntrees = c(1,2,3)))
  grid <- h2o.getGrid("gbm_grid_id")
  # Get grid summary
  summary(grid)
  # Fetch grid models
  model_ids <- grid@model_ids
  models <- lapply(model_ids, function(id) { h2o.getModel(id)})
  h2o.getId #Get back-end distributed key/value store id from an H2OFrame
  h2o.getModel(model_id)
  
  library(h2o)
  h2o.init()
  iris.hex <- as.h2o(iris, "iris.hex")
  model_id <- h2o.gbm(x = 1:4, y = 5, training_frame = iris.hex)@model_id
  model.retrieved <- h2o.getModel(model_id)
  
  h2o.getTimezone #Get the Time Zone on the H2O Cloud Returns a string
  
  h2o.getTimezone()
  h2o.getTypes(x)
  h2o.getVersion()
  h2o.giniCoef(object, train = FALSE, valid = FALSE, xval = FALSE)
  
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  hex <- h2o.uploadFile(prosPath)
  hex[,2] <- as.factor(hex[,2])
  model <- h2o.gbm(x = 3:9, y = 2, training_frame = hex, distribution = "bernoulli")
  perf <- h2o.performance(model, hex)
  h2o.giniCoef(perf)
  
  h2o.glm(x, y, training_frame, model_id = NULL, validation_frame = NULL,
          nfolds = 0, seed = -1, keep_cross_validation_predictions = FALSE,
          keep_cross_validation_fold_assignment = FALSE, fold_assignment = c("AUTO",
                                                                             "Random", "Modulo", "Stratified"), fold_column = NULL,
          ignore_const_cols = TRUE, score_each_iteration = FALSE,
          offset_column = NULL, weights_column = NULL, family = c("gaussian",
                                                                  "binomial", "quasibinomial", "multinomial", "poisson", "gamma", "tweedie"),
          tweedie_variance_power = 0, tweedie_link_power = 1, solver = c("AUTO",
                                                                         "IRLSM", "L_BFGS", "COORDINATE_DESCENT_NAIVE", "COORDINATE_DESCENT"),
          alpha = NULL, lambda = NULL, lambda_search = FALSE,
          early_stopping = TRUE, nlambdas = -1, standardize = TRUE,
          missing_values_handling = c("MeanImputation", "Skip"),
          compute_p_values = FALSE, remove_collinear_columns = FALSE,
          intercept = TRUE, non_negative = FALSE, max_iterations = -1,
          objective_epsilon = -1, beta_epsilon = 1e-04, gradient_epsilon = -1,
          link = c("family_default", "identity", "logit", "log", "inverse",
                   "tweedie"), prior = -1, lambda_min_ratio = -1, beta_constraints = NULL,
          max_active_predictors = -1, interactions = NULL,
          balance_classes = FALSE, class_sampling_factors = NULL,
          max_after_balance_size = 5, max_hit_ratio_k = 0, max_runtime_secs = 0)
  
  h2o.init()
  # Run GLM of CAPSULE ~ AGE + RACE + PSA + DCAPS
  prostatePath = system.file("extdata", "prostate.csv", package = "h2o")
  prostate.hex = h2o.importFile(path = prostatePath, destination_frame = "prostate.hex")
  h2o.glm(y = "CAPSULE", x = c("AGE","RACE","PSA","DCAPS"), training_frame = prostate.hex,
          family = "binomial", nfolds = 0, alpha = 0.5, lambda_search = FALSE)
  # Run GLM of VOL ~ CAPSULE + AGE + RACE + PSA + GLEASON
  myX = setdiff(colnames(prostate.hex), c("ID", "DPROS", "DCAPS", "VOL"))
  h2o.glm(y = "VOL", x = myX, training_frame = prostate.hex, family = "gaussian",
          nfolds = 0, alpha = 0.1, lambda_search = FALSE)
  # GLM variable importance
  # Also see:
  # https://github.com/h2oai/h2o/blob/master/R/tests/testdir_demos/runit_demo_VI_all_algos.R
  data.hex = h2o.importFile(
    path = "https://s3.amazonaws.com/h2o-public-test-data/smalldata/demos/bank-additional-full.csv",
    destination_frame = "data.hex")
  myX = 1:20
  myY="y"
  my.glm = h2o.glm(x=myX, y=myY, training_frame=data.hex, family="binomial", standardize=TRUE,
                   lambda_search=TRUE)
  
  
  
  h2o.glrm   #Generalized low rank decomposition of an H2O data frame
  h2o.glrm(training_frame, cols = NULL, model_id = NULL,
           validation_frame = NULL, ignore_const_cols = TRUE,
           score_each_iteration = FALSE, loading_name = NULL, transform = c("NONE",
                                                                            "STANDARDIZE", "NORMALIZE", "DEMEAN", "DESCALE"), k = 1,
           loss = c("Quadratic", "Absolute", "Huber", "Poisson", "Hinge", "Logistic",
                    "Periodic"), loss_by_col = c("Quadratic", "Absolute", "Huber", "Poisson",
                                                 "Hinge", "Logistic", "Periodic", "Categorical", "Ordinal"),
           loss_by_col_idx = NULL, multi_loss = c("Categorical", "Ordinal"),
           period = 1, regularization_x = c("None", "Quadratic", "L2", "L1",
                                            "NonNegative", "OneSparse", "UnitOneSparse", "Simplex"),
           regularization_y = c("None", "Quadratic", "L2", "L1", "NonNegative",
                                "OneSparse", "UnitOneSparse", "Simplex"), gamma_x = 0, gamma_y = 0,
           max_iterations = 1000, max_updates = 2000, init_step_size = 1,
           min_step_size = 1e-04, seed = -1, init = c("Random", "SVD", "PlusPlus",
                                                      "User"), svd_method = c("GramSVD", "Power", "Randomized"), user_y = NULL,
           user_x = NULL, expand_user_y = TRUE, impute_original = FALSE,
           recover_svd = FALSE, max_runtime_secs = 0)
  
  library(h2o)
  h2o.init()
  ausPath <- system.file("extdata", "australia.csv", package="h2o")
  australia.hex <- h2o.uploadFile(path = ausPath)
  h2o.glrm(training_frame = australia.hex, k = 5, loss = "Quadratic", regularization_x = "L1",
           gamma_x = 0.5, gamma_y = 0, max_iterations = 1000)
  
  h2o.grep #3Searches for matches to argument ‘pattern‘ within each element of a
  #string column.
  
  h2o.grep(pattern, x, ignore.case = FALSE, invert = FALSE,
           output.logical = FALSE)
  
  library(h2o)
  h2o.init()
  addresses <- as.h2o(c("2307", "Leghorn St", "Mountain View", "CA", "94043"))
  zip.codes <- addresses[h2o.grep("[0-9]{5}", addresses, output.logical = TRUE),]
  
  h2o.grid #H2O Grid Support
  
  h2o.grid(algorithm, grid_id, ..., hyper_params = list(),
           is_supervised = NULL, do_hyper_params_check = FALSE,
           search_criteria = NULL)
  
  library(h2o)
  library(jsonlite)
  h2o.init()
  iris.hex <- as.h2o(iris)
  grid <- h2o.grid("gbm", x = c(1:4), y = 5, training_frame = iris.hex,
                   hyper_params = list(ntrees = c(1,2,3)))
  # Get grid summary
  summary(grid)
  # Fetch grid models
  model_ids <- grid@model_ids
  models <- lapply(model_ids, function(id) { h2o.getModel(id)})
  
  h2o.group_by(data, by, ..., gb.control = list(na.methods = NULL, col.names =
                                                  NULL))
  h2o.gsub(pattern, replacement, x, ignore.case = FALSE)
  library(h2o)
  h2o.init()
  string_to_gsub <- as.h2o("r tutorial")
  sub_string <- h2o.gsub("r ","H2O ",string_to_gsub)
  
  h2o.head(x, n = 6L, ...)
  library(h2o)
  h2o.init(ip <- "localhost", port = 54321, startH2O = TRUE)
  ausPath <- system.file("extdata", "australia.csv", package="h2o")
  australia.hex <- h2o.uploadFile(path = ausPath)
  head(australia.hex, 10)
  tail(australia.hex, 10)
  h2o.hist(x, breaks = "Sturges", plot = TRUE)
  h2o.hit_ratio_table(object, train = FALSE, valid = FALSE, xval = FALSE)
  h2o.hour #Convert Milliseconds to Hour of Day in H2O Datasets
  h2o.hour(x)
  hour(x)
  
  h2o.ifelse(test, yes, no)
  ifelse(test, yes, no)
  h2o.init()
  ausPath <- system.file("extdata", "australia.csv", package="h2o")
  australia.hex <- h2o.importFile(path = ausPath)
  australia.hex[,9] <- ifelse(australia.hex[,3] < 279.9, 1, 0)
  summary(australia.hex)
  
  h2o.importFile(path, destination_frame = "", parse = TRUE, header = NA,
                 sep = "", col.names = NULL, col.types = NULL, na.strings = NULL)
  h2o.importFolder(path, pattern = "", destination_frame = "", parse = TRUE,
                   header = NA, sep = "", col.names = NULL, col.types = NULL,
                   na.strings = NULL)
  h2o.importHDFS(path, pattern = "", destination_frame = "", parse = TRUE,
                 header = NA, sep = "", col.names = NULL, na.strings = NULL)
  h2o.uploadFile(path, destination_frame = "", parse = TRUE, header = NA,
                 sep = "", col.names = NULL, col.types = NULL, na.strings = NULL,
                 progressBar = FALSE, parse_type = NULL)
  
  h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
  prosPath = system.file("extdata", "prostate.csv", package = "h2o")
  prostate.hex = h2o.importFile(path = prosPath, destination_frame = "prostate.hex")
  class(prostate.hex)
  summary(prostate.hex)
  #Import files with a certain regex pattern by utilizing h2o.importFolder()
  #In this example we import all .csv files in the directory prostate_folder
  prosPath = system.file("extdata", "prostate_folder", package = "h2o")
  prostate_pattern.hex = h2o.importFolder(path = prosPath, pattern = ".*.csv",
                                          destination_frame = "prostate.hex")
  class(prostate_pattern.hex)
  summary(prostate_pattern.hex)
  
  h2o.import_sql_select #Import SQL table that is result of SELECT SQL query into H2O
  h2o.import_sql_select(connection_url, select_query, username, password,
                        optimize = NULL)
  h2o.import_sql_table #Import SQL Table into H2O
  h2o.import_sql_table(connection_url, table, username, password,
                       columns = NULL, optimize = NULL)
  
  h2o.impute #Basic Imputation of H2O Vectors
  h2o.impute(data, column = 0, method = c("mean", "median", "mode"),
             combine_method = c("interpolate", "average", "lo", "hi"), by = NULL,
             groupByFrame = NULL, values = NULL)
  
  h2o.init()
  fr <- as.h2o(iris, destination_frame="iris")
  fr[sample(nrow(fr),40),5] <- NA # randomly replace 50 values with NA
  # impute with a group by
  fr <- h2o.impute(fr, "Species", "mode", by=c("Sepal.Length", "Sepal.Width"))
  
  h2o.init   #Initialize and Connect to H2O
  h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,
           forceDL = FALSE, enable_assertions = TRUE, license = NULL,
           nthreads = -1, max_mem_size = NULL, min_mem_size = NULL,
           ice_root = tempdir(), strict_version_check = TRUE,
           proxy = NA_character_, https = FALSE, insecure = FALSE,
           username = NA_character_, password = NA_character_,
           cookies = NA_character_, context_path = NA_character_,
           ignore_config = FALSE)
  
  ## Not run:
  # Try to connect to a local H2O instance that is already running.
  # If not found, start a local H2O instance from R with the default settings.
  h2o.init()
  # Try to connect to a local H2O instance.
  # If not found, raise an error.
  h2o.init(startH2O = FALSE)
  # Try to connect to a local H2O instance that is already running.
  # If not found, start a local H2O instance from R with 5 gigabytes of memory.
  h2o.init(max_mem_size = "5g")
  # Try to connect to a local H2O instance that is already running.
  # If not found, start a local H2O instance from R that uses 5 gigabytes of memory.
  h2o.init(max_mem_size = "5g")
  ## End(Not run)
  
  h2o.insertMissingValues
  #Insert Missing Values into an H2OFrame
  
  h2o.insertMissingValues(data, fraction = 0.1, seed = -1)
  
  library(h2o)
  h2o.init()
  irisPath <- system.file("extdata", "iris.csv", package = "h2o")
  iris.hex <- h2o.importFile(path = irisPath)
  summary(iris.hex)
  irismiss.hex <- h2o.insertMissingValues(iris.hex, fraction = 0.25)
  head(irismiss.hex)
  summary(irismiss.hex)
  
  h2o.interaction #Categorical Interaction Feature Creation in H2O
  
  library(h2o)
  h2o.init()
  # Create some random data
  myframe <- h2o.createFrame(rows = 20, cols = 5,
                             seed = -12301283, randomize = TRUE, value = 0,
                             categorical_fraction = 0.8, factors = 10, real_range = 1,
                             integer_fraction = 0.2, integer_range = 10,
                             binary_fraction = 0, binary_ones_fraction = 0.5,
                             missing_fraction = 0.2,
                             response_factors = 1)
  # Turn integer column into a categorical
  myframe[,5] <- as.factor(myframe[,5])
  head(myframe, 20)
  # Create pairwise interactions
  pairwise <- h2o.interaction(myframe, destination_frame = 'pairwise',
                              factors = list(c(1,2),c("C2","C3","C4")),
                              pairwise=TRUE, max_factors = 10, min_occurrence = 1)
  head(pairwise, 20)
  h2o.levels(pairwise,2)
  # Create 5-th order interaction
  higherorder <- h2o.interaction(myframe, destination_frame = 'higherorder', factors = c(1,2,3,4,5),
                                 pairwise=FALSE, max_factors = 10000, min_occurrence = 1)
  head(higherorder, 20)
  # Limit the number of factors of the "categoricalized" integer column
  # to at most 3 factors, and only if they occur at least twice
  head(myframe[,5], 20)
  trim_integer_levels <- h2o.interaction(myframe, destination_frame = 'trim_integers', factors = "C5",
                                         pairwise = FALSE, max_factors = 3, min_occurrence = 2)
  head(trim_integer_levels, 20)
  # Put all together
  myframe <- h2o.cbind(myframe, pairwise, higherorder, trim_integer_levels)
  myframe
  head(myframe,20)
  summary(myframe)
  
  h2o.isax #iSAX Compute the iSAX index for a DataFrame which is assumed to be numeric time series data
  
  h2o.isax(x, num_words, max_cardinality, optimize_card = FALSE)
  h2o.ischaracter(x)
  h2o.isfactor(x)
  h2o.isnumeric(x)
  h2o.is_client()
  h2o.kfold_column(data, nfolds, seed = -1)
  
  h2o.killMinus3 #Dump the stack into the JVM’s stdout.
  
  
  h2o.kmeans(training_frame, x, model_id = NULL, validation_frame = NULL,
             nfolds = 0, keep_cross_validation_predictions = FALSE,
             keep_cross_validation_fold_assignment = FALSE, fold_assignment = c("AUTO",
                                                                                "Random", "Modulo", "Stratified"), fold_column = NULL,
             ignore_const_cols = TRUE, score_each_iteration = FALSE, k = 1,
             estimate_k = FALSE, user_points = NULL, max_iterations = 10,
             standardize = TRUE, seed = -1, init = c("Random", "PlusPlus",
                                                     "Furthest", "User"), max_runtime_secs = 0,
             categorical_encoding = c("AUTO", "Enum", "OneHotInternal", "OneHotExplicit",
                                      "Binary", "Eigen", "LabelEncoder", "SortByResponse", "EnumLimited"))
  
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  h2o.kmeans(training_frame = prostate.hex, k = 10, x = c("AGE", "RACE", "VOL", "GLEASON"))
  
  h2o.kurtosis(x, ..., na.rm = TRUE)
  kurtosis.H2OFrame(x, ..., na.rm = TRUE)
  
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  h2o.kurtosis(prostate.hex$AGE)
  
  h2o.levels(x, i)
  iris.hex <- as.h2o(iris)
  h2o.levels(iris.hex, 5) # returns "setosa" "versicolor" "virginica"
  
  h2o.listTimezones  #List all of the Time Zones Acceptable by the H2O Cloud
  h2o.listTimezones()
  h2o.list_all_extensions #List all of the Time Zones Acceptable by the H2O Cloud.
  h2o.list_core_extensions()
  h2o.loadModel(path)
  
  ## Not run:
  # library(h2o)
  # h2o.init()
  # prosPath = system.file("extdata", "prostate.csv", package = "h2o")
  # prostate.hex = h2o.importFile(path = prosPath, destination_frame = "prostate.hex")
  # prostate.glm = h2o.glm(y = "CAPSULE", x = c("AGE","RACE","PSA","DCAPS"),
  # training_frame = prostate.hex, family = "binomial", alpha = 0.5)
  # glmmodel.path = h2o.saveModel(prostate.glm, dir = "/Users/UserName/Desktop")
  # glmmodel.load = h2o.loadModel(glmmodel.path)
  ## End(Not run)
  
  h2o.log(x)
  h2o.log10(x)
  h2o.log1p(x)
  h2o.log2(x)
  h2o.logAndEcho(message)
  h2o.logloss #Retrieve the Log Loss Value
  h2o.logloss Retrieve the Log Loss Value
  
  h2o.ls #List Keys on an H2O Cluster
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  h2o.ls()
  
  
  h2o.lstrip #Strip set from left
  h2o.lstrip(x, set = " ")
  
  library(h2o)
  h2o.init()
  string_to_lstrip <- as.h2o("1234567890")
  lstrip_string <- h2o.lstrip(string_to_lstrip,"123") #Remove "123"
  
  h2o.mae #Retrieve the Mean Absolute Error Value
  library(h2o)
  h <- h2o.init()
  fr <- as.h2o(iris)
  m <- h2o.deeplearning(x=2:5,y=1,training_frame=fr)
  h2o.mae(m)
  
  h2o.makeGLMModel(model, beta) #Set betas of an existing H2O GLM Model
  
  h2o.make_metrics #Create Model Metrics from predicted and actual values in H2O
  h2o.make_metrics(predicted, actuals, domain = NULL, distribution = NULL)
  
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  prostate.hex$CAPSULE <- as.factor(prostate.hex$CAPSULE)
  prostate.gbm <- h2o.gbm(3:9, "CAPSULE", prostate.hex)
  pred <- h2o.predict(prostate.gbm, prostate.hex)[,3] ## class-1 probability
  h2o.make_metrics(pred,prostate.hex$CAPSULE)
  
  h2o.match #Value Matching in H2O
  h2o.match(x, table, nomatch = 0, incomparables = NULL)
  match.H2OFrame(x, table, nomatch = 0, incomparables = NULL)
  x %in% table
  
  h2o.init()
  hex <- as.h2o(iris)
  h2o.match(hex[,5], c("setosa", "versicolor"))
  
  h2o.max(x, na.rm = FALSE)
  h2o.mean(x, na.rm = FALSE, axis = 0, return_frame = FALSE, ...)
  
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  # Default behavior. Will return list of means per column.
  h2o.mean(prostate.hex$AGE)
  # return_frame set to TRUE. This will return an H2O Frame
  # with mean per row or column (depends on axis argument)
  h2o.mean(prostate.hex,na.rm=TRUE,axis=1,return_frame=TRUE)
  
  h2o.mean_per_class_error #Retrieve the mean per class error
  h2o.mean_per_class_error(object, train = FALSE, valid = FALSE,
                           xval = FALSE)
  
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  hex <- h2o.uploadFile(prosPath)
  hex[,2] <- as.factor(hex[,2])
  model <- h2o.gbm(x = 3:9, y = 2, training_frame = hex, distribution = "bernoulli")
  perf <- h2o.performance(model, hex)
  h2o.mean_per_class_error(perf)
  h2o.mean_per_class_error(model, train=TRUE)
  
  h2o.mean_residual_deviance #Retrieve the Mean Residual Deviance value
  h2o.mean_residual_deviance(object, train = FALSE, valid = FALSE,
                             xval = FALSE)
  
  library(h2o)
  h <- h2o.init()
  fr <- as.h2o(iris)
  m <- h2o.deeplearning(x=2:5,y=1,training_frame=fr)
  h2o.mean_residual_deviance(m)
  
  h2o.median(x, na.rm = TRUE)
  ## S3 method for class 'H2OFrame'
  median(x, na.rm = TRUE)
  
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath, destination_frame = "prostate.hex")
  h2o.median(prostate.hex)
  
  h2o.merge(x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by,
            all = FALSE, all.x = all, all.y = all, method = "hash")
  
  h2o.init()
  left <- data.frame(fruit = c('apple', 'orange', 'banana', 'lemon', 'strawberry', 'blueberry'),
                     color <- c('red', 'orange', 'yellow', 'yellow', 'red', 'blue'))
  right <- data.frame(fruit = c('apple', 'orange', 'banana', 'lemon', 'strawberry', 'watermelon'),
                      citrus <- c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE))
  l.hex <- as.h2o(left)
  r.hex <- as.h2o(right)
  left.hex <- h2o.merge(l.hex, r.hex, all.x = TRUE)
  
  
  h2o.metric #H2O Model Metric Accessor Functions
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  hex <- h2o.uploadFile(prosPath)
  hex[,2] <- as.factor(hex[,2])
  model <- h2o.gbm(x = 3:9, y = 2, training_frame = hex, distribution = "bernoulli")
  perf <- h2o.performance(model, hex)
  h2o.F1(perf)
  
  h2o.mktime #Compute msec since the Unix Epoch
  h2o.mktime(year = 1970, month = 0, day = 0, hour = 0, minute = 0,
             second = 0, msec = 0)
  
  h2o.month #Convert Milliseconds to Months in H2O Datasets
  h2o.mse #Retrieves Mean Squared Error Value
  
  h2o.mse(object, train = FALSE, valid = FALSE, xval = FALSE)
  
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  hex <- h2o.uploadFile(prosPath)
  hex[,2] <- as.factor(hex[,2])
  model <- h2o.gbm(x = 3:9, y = 2, training_frame = hex, distribution = "bernoulli")
  perf <- h2o.performance(model, hex)
  h2o.mse(perf)
  
  h2o.nacnt #Count of NAs per column
  
  h2o.init()
  iris.hex <- as.h2o(iris)
  h2o.nacnt(iris.hex) # should return all 0s
  h2o.insertMissingValues(iris.hex)
  h2o.nacnt(iris.hex)
  
  h2o.naiveBayes  #Compute naive Bayes probabilities on an H2O dataset.
  h2o.naiveBayes(x, y, training_frame, model_id = NULL, nfolds = 0,
                 seed = -1, fold_assignment = c("AUTO", "Random", "Modulo", "Stratified"),
                 fold_column = NULL, keep_cross_validation_predictions = FALSE,
                 keep_cross_validation_fold_assignment = FALSE, validation_frame = NULL,
                 ignore_const_cols = TRUE, score_each_iteration = FALSE,
                 balance_classes = FALSE, class_sampling_factors = NULL,
                 max_after_balance_size = 5, max_hit_ratio_k = 0, laplace = 0,
                 threshold = 0.001, min_sdev = 0.001, eps = 0, eps_sdev = 0,
                 min_prob = 0.001, eps_prob = 0, compute_metrics = TRUE,
                 max_runtime_secs = 0)
  
  h2o.init()
  votesPath <- system.file("extdata", "housevotes.csv", package="h2o")
  votes.hex <- h2o.uploadFile(path = votesPath, header = TRUE)
  h2o.naiveBayes(x = 2:17, y = 1, training_frame = votes.hex, laplace = 3)
  
  h2o.nchar(x)
  
  library(h2o)
  h2o.init()
  string_to_nchar <- as.h2o("r tutorial")
  nchar_string <- h2o.nchar(string_to_nchar)
  h2o.ncol(x)
  h2o.networkTest()
  h2o.nlevels(x)
  h2o.no_progress()
  h2o.nrow(x)
  
  h2o.null_deviance(object, train = FALSE, valid = FALSE, xval = FALSE)
  h2o.null_dof(object, train = FALSE, valid = FALSE, xval = FALSE)
  h2o.num_iterations(object)  #object An H2OClusteringModel object.
  h2o.num_valid_substrings(x, path)
  
  ## Not run:
  h2o.init()
  h2o.startLogging()
  ausPath = system.file("extdata", "australia.csv", package="h2o")
  australia.hex = h2o.importFile(path = ausPath)
  h2o.stopLogging()
  # Not run to avoid windows being opened during R CMD check
  # h2o.openLog("Command")
  # h2o.openLog("Error")
  ## End(Not run)
  
  h2o.parseRaw #H2O Data Parsing
  h2o.parseRaw(data, pattern = "", destination_frame = "", header = NA,
               sep = "", col.names = NULL, col.types = NULL, na.strings = NULL,
               blocking = FALSE, parse_type = NULL, chunk_size = NULL)
  
  h2o.parseSetup #Get a parse setup back for the staged data.
  
  h2o.parseSetup(data, pattern = "", destination_frame = "", header = NA,
                 sep = "", col.names = NULL, col.types = NULL, na.strings = NULL,
                 parse_type = NULL, chunk_size = NULL)
  
  h2o.partialPlot #Partial Dependence Plots
  h2o.partialPlot(object, data, cols, destination_key, nbins = 20,
                  plot = TRUE, plot_stddev = TRUE)
  
  library(h2o)
  h2o.init()
  prostate.path <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prostate.path, destination_frame = "prostate.hex")
  prostate.hex[, "CAPSULE"] <- as.factor(prostate.hex[, "CAPSULE"] )
  prostate.hex[, "RACE"] <- as.factor(prostate.hex[,"RACE"] )
  prostate.gbm <- h2o.gbm(x = c("AGE","RACE"),
                          y = "CAPSULE",
                          training_frame = prostate.hex,
                          ntrees = 10,
                          max_depth = 5,
                          learn_rate = 0.1)
  h2o.partialPlot(object = prostate.gbm, data = prostate.hex, cols = c("AGE", "RACE"))
  
  h2o.performance(model, newdata = NULL, train = FALSE, valid = FALSE,
                  xval = FALSE, data = NULL)
  
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  prostate.hex$CAPSULE <- as.factor(prostate.hex$CAPSULE)
  prostate.gbm <- h2o.gbm(3:9, "CAPSULE", prostate.hex)
  h2o.performance(model = prostate.gbm, newdata=prostate.hex)
 
  ## If model uses balance_classes
  ## the results from train = TRUE will not match the results from newdata = prostate.hex
  prostate.gbm.balanced <- h2o.gbm(3:9, "CAPSULE", prostate.hex, balance_classes = TRUE)
  h2o.performance(model = prostate.gbm.balanced, newdata = prostate.hex)
  h2o.performance(model = prostate.gbm.balanced, train = TRUE)
  
  h2o.pivot
  h2o.pivot(x, index, column, value)
  h2o.prcomp #Principal components analysis of an H2O data frame using the power
             #method to calculate the singular value decomposition of the Gram matrix.
  
  h2o.prcomp(training_frame, x, model_id = NULL, validation_frame = NULL,
             ignore_const_cols = TRUE, score_each_iteration = FALSE,
             transform = c("NONE", "STANDARDIZE", "NORMALIZE", "DEMEAN", "DESCALE"),
             pca_method = c("GramSVD", "Power", "Randomized", "GLRM"), k = 1,
             max_iterations = 1000, use_all_factor_levels = FALSE,
             compute_metrics = TRUE, impute_missing = FALSE, seed = -1,
             max_runtime_secs = 0)
  
  library(h2o)
  h2o.init()
  ausPath <- system.file("extdata", "australia.csv", package="h2o")
  australia.hex <- h2o.uploadFile(path = ausPath)
  h2o.prcomp(training_frame = australia.hex, k = 8, transform = "STANDARDIZE")
  
  h2o.predict_json #H2O Prediction from R without having H2O running
  h2o.predict_json(model, json, genmodelpath, labels, classpath, javaoptions)
  
  library(h2o)
  h2o.predict_json('~/GBM_model_python_1473313897851_6.zip', '{"C7":1}')
  h2o.predict_json('~/GBM_model_python_1473313897851_6.zip', '{"C7":1}', c(".", "lib"))
  
  
  h2o.print(x, n = 6L)
  h2o.prod(x)  #Return the product of all the values present in its arguments
  h2o.proj_archetypes  #Convert Archetypes to Features from H2O GLRM Model
  h2o.proj_archetypes(object, data, reverse_transform = FALSE)
  library(h2o)
  h2o.init()
  irisPath <- system.file("extdata", "iris_wheader.csv", package="h2o")
  iris.hex <- h2o.uploadFile(path = irisPath)
  iris.glrm <- h2o.glrm(training_frame = iris.hex, k = 4, loss = "Quadratic",
                        multi_loss = "Categorical", max_iterations = 1000)
  iris.parch <- h2o.proj_archetypes(iris.glrm, iris.hex)
  head(iris.parch)
  
  h2o.quantile #Quantiles of H2O Frames
  h2o.quantile(x, probs = c(0.001, 0.01, 0.1, 0.25, 0.333, 0.5, 0.667, 0.75,
                            0.9, 0.99, 0.999), combine_method = c("interpolate", "average", "avg",
                                                                  "low", "high"), weights_column = NULL, ...)
  
  # Request quantiles for an H2O parsed data set:
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  # Request quantiles for a subset of columns in an H2O parsed data set
  quantile(prostate.hex[,3])
  for(i in 1:ncol(prostate.hex))
    quantile(prostate.hex[,i])
  
  #Retrieve the R2 value
  h2o.r2(object, train = FALSE, valid = FALSE, xval = FALSE)
  library(h2o)
  
  h <- h2o.init()
  fr <- as.h2o(iris)
  
  m <- h2o.glm(x=2:5,y=1,training_frame=fr)
  
  h2o.r2(m)
  
  h2o.randomForest #Builds a Random Forest Model on an H2OFrame
  h2o.randomForest(x, y, training_frame, model_id = NULL,
                   validation_frame = NULL, nfolds = 0,
                   keep_cross_validation_predictions = FALSE,
                   keep_cross_validation_fold_assignment = FALSE,
                   score_each_iteration = FALSE, score_tree_interval = 0,
                   fold_assignment = c("AUTO", "Random", "Modulo", "Stratified"),
                   fold_column = NULL, ignore_const_cols = TRUE, offset_column = NULL,
                   weights_column = NULL, balance_classes = FALSE,
                   class_sampling_factors = NULL, max_after_balance_size = 5,
                   max_hit_ratio_k = 0, ntrees = 50, max_depth = 20, min_rows = 1,
                   nbins = 20, nbins_top_level = 1024, nbins_cats = 1024,
                   r2_stopping = Inf, stopping_rounds = 0, stopping_metric = c("AUTO",
                                                                               "deviance", "logloss", "MSE", "RMSE", "MAE", "RMSLE", "AUC", "lift_top_group",
                                                                               "misclassification", "mean_per_class_error"), stopping_tolerance = 0.001,
                   max_runtime_secs = 0, seed = -1, build_tree_one_node = FALSE,
                   mtries = -1, sample_rate = 0.6320000291, sample_rate_per_class = NULL,
                   binomial_double_trees = FALSE, checkpoint = NULL,
                   col_sample_rate_change_per_level = 1, col_sample_rate_per_tree = 1,
                   min_split_improvement = 1e-05, histogram_type = c("AUTO",
                                                                     "UniformAdaptive", "Random", "QuantilesGlobal", "RoundRobin"),
                   categorical_encoding = c("AUTO", "Enum", "OneHotInternal", "OneHotExplicit",
                                            "Binary", "Eigen", "LabelEncoder", "SortByResponse", "EnumLimited"),
                   calibrate_model = FALSE, calibration_frame = NULL)
  
  h2o.range(x, na.rm = FALSE, finite = FALSE)
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  prostate.cbind <- h2o.rbind(prostate.hex, prostate.hex)
  head(prostate.cbind)
  
  h2o.reconstruct(object, data, reverse_transform = FALSE)
  
  library(h2o)
  h2o.init()
  irisPath <- system.file("extdata", "iris_wheader.csv", package="h2o")
  iris.hex <- h2o.uploadFile(path = irisPath)
  iris.glrm <- h2o.glrm(training_frame = iris.hex, k = 4, transform = "STANDARDIZE",
                        loss = "Quadratic", multi_loss = "Categorical", max_iterations = 1000)
  iris.rec <- h2o.reconstruct(iris.glrm, iris.hex, reverse_transform = TRUE)
  head(iris.rec)
  
  h2o.relevel  #Reorders levels of an H2O factor
  h2o.relevel(x, y)
  
  h2o.removeAll(timeout_secs = 0)
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package = "h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  h2o.ls()
  h2o.removeAll()
  h2o.ls()
  
  h2o.removeVecs #Delete Columns from an H2OFrame
  h2o.rep_len(x, length.out)
  h2o.residual_deviance(object, train = FALSE, valid = FALSE, xval = FALSE)
  
  h2o.residual_dof(object, train = FALSE, valid = FALSE, xval = FALSE)
  h2o.rm(ids)
  h2o.rmse  #Retrieves Root Mean Squared Error Value
  h2o.rmse(object, train = FALSE, valid = FALSE, xval = FALSE)
  
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  hex <- h2o.uploadFile(prosPath)
  hex[,2] <- as.factor(hex[,2])
  model <- h2o.gbm(x = 3:9, y = 2, training_frame = hex, distribution = "bernoulli")
  perf <- h2o.performance(model, hex)
  h2o.rmse(perf)
  
  h2o.rmsle #Retrieve the Root Mean Squared Log Error
  h2o.rmsle(object, train = FALSE, valid = FALSE, xval = FALSE)
  library(h2o)
  h <- h2o.init()
  fr <- as.h2o(iris)
  m <- h2o.deeplearning(x=2:5,y=1,training_frame=fr)
  h2o.rmsle(m)
  
  h2o.round(x, digits = 0)
  round(x, digits = 0)
  h2o.rstrip #Strip set from right
  h2o.rstrip(x, set = " ")
  
  library(h2o)
  h2o.init()
  string_to_rstrip <- as.h2o("1234567890")
  rstrip_string <- h2o.rstrip(string_to_rstrip,"890") #Remove "890"
  
  h2o.runif(x, seed = -1)
  
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.importFile(path = prosPath, destination_frame = "prostate.hex")
  s <- h2o.runif(prostate.hex)
  summary(s)
  prostate.train <- prostate.hex[s <= 0.8,]
  prostate.train <- h2o.assign(prostate.train, "prostate.train")
  prostate.test <- prostate.hex[s > 0.8,]
  prostate.test <- h2o.assign(prostate.test, "prostate.test")
  nrow(prostate.train) + nrow(prostate.test)
  
  h2o.saveModel #Save an H2O Model Object to Disk
  ## Not run:
  # library(h2o)
  # h2o.init()
  # prostate.hex <- h2o.importFile(path = paste("https://raw.github.com",
  # "h2oai/h2o-2/master/smalldata/logreg/prostate.csv", sep = "/"),
  # destination_frame = "prostate.hex")
  # prostate.glm <- h2o.glm(y = "CAPSULE", x = c("AGE","RACE","PSA","DCAPS"),
  # training_frame = prostate.hex, family = "binomial", alpha = 0.5)
  # h2o.saveModel(object = prostate.glm, path = "/Users/UserName/Desktop", force=TRUE)
  ## End(Not run)
  
  
  h2o.saveModelDetails  #Save an H2O Model Details
  h2o.saveModelDetails(object, path = "", force = FALSE)
  ## Not run:
  # library(h2o)
  # h2o.init()
  # prostate.hex <- h2o.uploadFile(path = system.file("extdata", "prostate.csv", package="h2o"))
  # prostate.glm <- h2o.glm(y = "CAPSULE", x = c("AGE","RACE","PSA","DCAPS"),
  # training_frame = prostate.hex, family = "binomial", alpha = 0.5)
  # h2o.saveModelDetails(object = prostate.glm, path = "/Users/UserName/Desktop", force=TRUE)
  ## End(Not run)
  
  h2o.saveMojo(object, path = "", force = FALSE)
  ## Not run:
  # library(h2o)
  # h2o.init()
  # prostate.hex <- h2o.uploadFile(path = system.file("extdata", "prostate.csv", package="h2o"))
  # prostate.glm <- h2o.glm(y = "CAPSULE", x = c("AGE","RACE","PSA","DCAPS"),
  # training_frame = prostate.hex, family = "binomial", alpha = 0.5)
  # h2o.saveMojo(object = prostate.glm, path = "/Users/UserName/Desktop", force=TRUE)
  ## End(Not run)
  
  h2o.scale #Scaling and Centering of an H2OFrame
  h2o.scale(x, center = TRUE, scale = TRUE)
  library(h2o)
  h2o.init()
  irisPath <- system.file("extdata", "iris_wheader.csv", package="h2o")
  iris.hex <- h2o.uploadFile(path = irisPath, destination_frame = "iris.hex")
  summary(iris.hex)
  # Scale and center all the numeric columns in iris data set
  scale(iris.hex[, 1:4])
  
  h2o.scoreHistory #Retrieve Model Score History
  h2o.scoreHistory(object)
  h2o.sd(x, na.rm = FALSE)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  sd(prostate.hex$AGE)
  
  h2o.sdev(object)
  h2o.setLevels(x, levels)
  h2o.setTimezone(tz)
  h2o.show_progress
  h2o.shutdown(prompt = TRUE)
  # Don't run automatically to prevent accidentally shutting down a cloud
  ## Not run:
  library(h2o)
  h2o.init()
  h2o.shutdown()
  ## End(Not run)
  
  
  h2o.signif(x, digits = 6)
  signif(x, digits = 6)
  
  h2o.sin
  h2o.skewness(x, ..., na.rm = TRUE)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  h2o.skewness(prostate.hex$AGE)
  
  h2o.splitFrame #Split an H2O Data Set
  h2o.splitFrame(data, ratios = 0.75, destination_frames, seed = -1)
  
  library(h2o)
  h2o.init()
  irisPath <- system.file("extdata", "iris.csv", package = "h2o")
  iris.hex <- h2o.importFile(path = irisPath)
  iris.split <- h2o.splitFrame(iris.hex, ratios = c(0.2, 0.5))
  head(iris.split[[1]])
  summary(iris.split[[1]])
  
  
  h2o.sqrt(x)
  h2o.stackedEnsemble #Build a stacked ensemble (aka. Super Learner) using the H2O
  h2o.stackedEnsemble(x, y, training_frame, model_id = NULL,
                      validation_frame = NULL, base_models = list())
  
  h2o.startLogging(file)
  
  library(h2o)
  h2o.init()
  h2o.startLogging()
  ausPath = system.file("extdata", "australia.csv", package="h2o")
  australia.hex = h2o.importFile(path = ausPath)
  h2o.stopLogging()
  
  h2o.std_coef_plot
  
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.importFile(prosPath)
  prostate.hex[,2] <- as.factor(prostate.hex[,2])
  prostate.glm <- h2o.glm(y = "CAPSULE", x = c("AGE","RACE","PSA","DCAPS"),
                          training_frame = prostate.hex, family = "binomial",
                          nfolds = 0, alpha = 0.5, lambda_search = FALSE)
  h2o.std_coef_plot(prostate.glm)
  
  h2o.stopLogging #Stop Writing H2O R Logs
  library(h2o)
  h2o.init()
  h2o.startLogging()
  ausPath = system.file("extdata", "australia.csv", package="h2o")
  australia.hex = h2o.importFile(path = ausPath)
  h2o.stopLogging()
  
  h2o.str(object, ..., cols = FALSE)
  h2o.stringdist(x, y, method = c("lv", "lcs", "qgram", "jaccard", "jw",
                                  "soundex"))
  h2o.init()
  x <- as.h2o(c("Martha", "Dwayne", "Dixon"))
  y <- as.character(as.h2o(c("Marhta", "Duane", "Dicksonx")))
  h2o.stringdist(x, y, method = "jw")
  
  h2o.strsplit(x, split)
  library(h2o)
  h2o.init()
  string_to_split <- as.h2o("Split at every character.")
  split_string <- h2o.strsplit(string_to_split,"")
  h2o.sub(pattern, replacement, x, ignore.case = FALSE)
  library(h2o)
  h2o.init()
  string_to_sub <- as.h2o("r tutorial")
  sub_string <- h2o.sub("r ","H2O ",string_to_sub)
  
  h2o.substring(x, start, stop = "[]")
  h2o.substr(x, start, stop = "[]")
  library(h2o)
  h2o.init()
  string_to_substring <- as.h2o("1234567890")
  substr <- h2o.substring(string_to_substring,2) #Get substring from second index onwards
  
  h2o.sum(x, na.rm = FALSE, axis = 0, return_frame = FALSE)
  h2o.summary(object, factors = 6L, exact_quantiles = FALSE, ...)
  
  
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.importFile(path = prosPath)
  summary(prostate.hex)
  summary(prostate.hex$GLEASON)
  summary(prostate.hex[,4:6])
  summary(prostate.hex, exact_quantiles=TRUE)
  
  #Singular value decomposition of an H2O data frame using the power
  #method.
  h2o.svd(training_frame, x, destination_key, model_id = NULL,
          validation_frame = NULL, ignore_const_cols = TRUE,
          score_each_iteration = FALSE, transform = c("NONE", "STANDARDIZE",
                                                      "NORMALIZE", "DEMEAN", "DESCALE"), svd_method = c("GramSVD", "Power",
                                                                                                        "Randomized"), nv = 1, max_iterations = 1000, seed = -1,
          keep_u = TRUE, u_name = NULL, use_all_factor_levels = TRUE,
          max_runtime_secs = 0)
  
  library(h2o)
  h2o.init()
  ausPath <- system.file("extdata", "australia.csv", package="h2o")
  australia.hex <- h2o.uploadFile(path = ausPath)
  h2o.svd(training_frame = australia.hex, nv = 8)
  
  h2o.table #Cross Tabulation and Table Creation in H2O
  
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath, destination_frame = "prostate.hex")
  summary(prostate.hex)
  # Counts of the ages of all patients
  head(h2o.table(prostate.hex[,3]))
  h2o.table(prostate.hex[,3])
  # Two-way table of ages (rows) and race (cols) of all patients
  head(h2o.table(prostate.hex[,c(3,4)]))
  h2o.table(prostate.hex[,c(3,4)])
  
  h2o.tabulate #Tabulation between Two Columns of an H2OFrame
  
  h2o.tabulate(data, x, y, weights_column = NULL, nbins_x = 50,
               nbins_y = 50)
  library(h2o)
  h2o.init()
  df <- as.h2o(iris)
  tab <- h2o.tabulate(data = df, x = "Sepal.Length", y = "Petal.Width",
                      weights_column = NULL, nbins_x = 10, nbins_y = 10)
  plot(tab)
  
  h2o.toFrame #Converts a given word2vec model into H2OFrame
  h2o.toFrame(word2vec) 
  
  h2o.init()
  # Build a dummy word2vec model
  data <- as.character(as.h2o(c("a", "b", "a")))
  w2v.model <- h2o.word2vec(data, sent_sample_rate = 0, min_word_freq = 0, epochs = 1, vec_size = 2)
  # Transform words to vectors and return average vector for each sentence
  h2o.toFrame(w2v.model) # -> Frame made of 2 rows and 2 columns
  
  h2o.tokenize(x, split)
  
  
  
  library(h2o)
  h2o.init()
  string_to_tokenize <- as.h2o("Split at every character and tokenize.")
  tokenize_string <- h2o.tokenize(as.character(string_to_tokenize),"")
  
  h2o.tolower(x)
  library(h2o)
  h2o.init()
  string_to_lower <- as.h2o("ABCDE")
  lowered_string <- h2o.tolower(string_to_lower)
  h2o.topN(x, column, nPercent)
  h2o.totss(object, train = FALSE, valid = FALSE, xval = FALSE)
  h2o.tot_withinss(object, train = FALSE, valid = FALSE, xval = FALSE)
  h2o.toupper(x)
  library(h2o)
  h2o.init()
  string_to_upper <- as.h2o("abcde")
  upper_string <- h2o.toupper(string_to_upper)
  
  h2o.transform(word2vec, words, aggregate_method = c("NONE", "AVERAGE"))
  
  h2o.init()
  # Build a dummy word2vec model
  data <- as.character(as.h2o(c("a", "b", "a")))
  w2v.model <- h2o.word2vec(data, sent_sample_rate = 0, min_word_freq = 0, epochs = 1, vec_size = 2)
  
  # Transform words to vectors without aggregation
  sentences <- as.character(as.h2o(c("b", "c", "a", NA, "b")))
  h2o.transform(w2v.model, sentences) # -> 5 rows total, 2 rows NA ("c" is not in the vocabulary)
  
  # Transform words to vectors and return average vector for each sentence
  h2o.transform(w2v.model, sentences, aggregate_method = "AVERAGE") # -> 2 rows
  
  library(h2o)
  h2o.init()
  string_to_trim <- as.h2o("r tutorial")
  trim_string <- h2o.trim(string_to_trim)
  
  h2o.trunc(x)
  h2o.unique(x)
  h2o.var(x, y = NULL, na.rm = FALSE, use)
  var(x, y = NULL, na.rm = FALSE, use)
  
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  var(prostate.hex$AGE)
  h2o.
  
  h2o.varimp(object)
  h2o.varimp_plot(model, num_of_features = NULL)
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  hex <- h2o.importFile(prosPath)
  hex[,2] <- as.factor(hex[,2])
  model <- h2o.gbm(x = 3:9, y = 2, training_frame = hex, distribution = "bernoulli")
  h2o.varimp_plot(model)
  # for deep learning set the variable_importance parameter to TRUE
  iris.hex <- as.h2o(iris)
  iris.dl <- h2o.deeplearning(x = 1:4, y = 5, training_frame = iris.hex,
                              variable_importances = TRUE)
  h2o.varimp_plot(iris.dl)
  
  h2o.week(x)
  h2o.weights(object, matrix_id = 1)
  h2o.which(x)
  
  h2o.init()
  iris.hex <- as.h2o(iris)
  h2o.which(iris.hex[,1]==4.4)
  
  h2o.which_max(x, na.rm = TRUE, axis = 0)
  which.max.H2OFrame(x, na.rm = TRUE, axis = 0)
  which.min.H2OFrame(x, na.rm = TRUE, axis = 0)
  
  h2o.which_min(x, na.rm = TRUE, axis = 0)
  h2o.withinss(object)
  h2o.word2vec(training_frame = NULL, model_id = NULL, min_word_freq = 5,
               word_model = c("SkipGram"), norm_model = c("HSM"), vec_size = 100,
               window_size = 5, sent_sample_rate = 0.001, init_learning_rate = 0.025,
               epochs = 5, pre_trained = NULL)
  
  h2o.xgboost(x, y, training_frame, model_id = NULL, validation_frame = NULL,
              nfolds = 0, keep_cross_validation_predictions = FALSE,
              keep_cross_validation_fold_assignment = FALSE,
              score_each_iteration = FALSE, fold_assignment = c("AUTO", "Random",
                                                                "Modulo", "Stratified"), fold_column = NULL, ignore_const_cols = TRUE,
              offset_column = NULL, weights_column = NULL, stopping_rounds = 0,
              stopping_metric = c("AUTO", "deviance", "logloss", "MSE", "RMSE", "MAE",
                                  "RMSLE", "AUC", "lift_top_group", "misclassification",
                                  "mean_per_class_error"), stopping_tolerance = 0.001, max_runtime_secs = 0,
              seed = -1, distribution = c("AUTO", "bernoulli", "multinomial",
                                          "gaussian", "poisson", "gamma", "tweedie", "laplace", "quantile", "huber"),
              tweedie_power = 1.5, ntrees = 50, max_depth = 5, min_rows = 10,
              min_child_weight = 0, learn_rate = 0.1, eta = 0, sample_rate = 1,
              subsample = 0, col_sample_rate = 1, colsample_bylevel = 0,
              col_sample_rate_per_tree = 1, colsample_bytree = 0,
              max_abs_leafnode_pred = 3.4028235e+38, max_delta_step = 0,
              score_tree_interval = 0, min_split_improvement = 0, max_bin = 255,
              num_leaves = 255, min_sum_hessian_in_leaf = 100, min_data_in_leaf = 0,
              tree_method = c("auto", "exact", "approx", "hist"),
              grow_policy = c("depthwise", "lossguide"), booster = c("gbtree",
                                                                     "gblinear", "dart"), gamma = 0, reg_lambda = 1, reg_alpha = 0,
              dmatrix_type = c("auto", "dense", "sparse"), backend = c("auto", "gpu",
                                                                       "cpu"), gpu_id = 0)
  h2o.xgboost.available()
  h2o.year(x)
  year(x)
  show(object)
  is.character(x)
  is.h2o
  is.numeric
  "||"(x, y)
  getParms(object)
  ## S4 method for signature 'H2OModel'
  getParms(object)
  getCenters(object)
  getCentersStd(object)
  getWithinSS(object)
  getTotWithinSS(object)
  getBetweenSS(object)
  getTotSS(object)
  getIterations(object)
  getClusterSizes(object)
  
  plot.H2OModel #Plot an H2O Model
  if (requireNamespace("mlbench", quietly=TRUE)) {
    library(h2o)
    h2o.init()
    df <- as.h2o(mlbench::mlbench.friedman1(10000,1))
    rng <- h2o.runif(df, seed=1234)
    train <- df[rng<0.8,]
    valid <- df[rng>=0.8,]
    gbm <- h2o.gbm(x = 1:10, y = "y", training_frame = train, validation_frame = valid,
                   ntrees=500, learn_rate=0.01, score_each_iteration = TRUE)
    plot(gbm)
    plot(gbm, timestep = "duration", metric = "deviance")
    plot(gbm, timestep = "number_of_trees", metric = "deviance")
    plot(gbm, timestep = "number_of_trees", metric = "rmse")
    plot(gbm, timestep = "number_of_trees", metric = "mae")
  }
  
  
  plot.H2OTabulate  #Plot an H2O Tabulate Heatmap
  
  plot(x, xlab = x$cols[1], ylab = x$cols[2],
       base_size = 12, ...)
  
  library(h2o)
  h2o.init()
  df <- as.h2o(iris)
  tab <- h2o.tabulate(data = df, x = "Sepal.Length", y = "Petal.Width",
                      weights_column = NULL, nbins_x = 10, nbins_y = 10)
  plot(tab)
  
  h2o.predict(object, newdata, ...)
  
  library(h2o)
  h2o.init()
  prosPath <- system.file("extdata", "prostate.csv", package="h2o")
  prostate.hex <- h2o.uploadFile(path = prosPath)
  prostate.hex$CAPSULE <- as.factor(prostate.hex$CAPSULE)
  prostate.gbm <- h2o.gbm(3:9, "CAPSULE", prostate.hex)
  h2o.predict(prostate.gbm, prostate.hex)
  h2o.predict_leaf_node_assignment(prostate.gbm, prostate.hex)
  
  print(x, header = TRUE, ...)
  range(..., na.rm = TRUE)
  summary(object, show_stack_traces = FALSE)
  summary(object, ...)
  
  library(h2o)
  h2o.init()
  h2o.shutdown(prompt = FALSE)
  Sys.sleep(3)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  