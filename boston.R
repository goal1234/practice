data("Boston", package = "MASS")

dim(Boston)
colnames(Boston)
?Boston

# crim -> 城市犯罪率
# zn -> 规划超25000
# indus -> 城市非零售面积占比
# chas -> 查理斯河附近(0, 1)
# nox -> 氮氧化物浓度
# rm -> 平均每户住房数
# age -> 1940年以前的自用建造比例
# dis -> 加权的5个就业中心
# rad -> 公路可达指数
# tax -> 全额财产税税率
# ptratio -> 城镇师生比例
# black -> 城镇黑人的比例
# lstat -> 较低的人口状况（百分比）
# medv -> 业主中值占据1000s

# medv进行聚类
# 设定参数
x <- Boston[, 14]
n <- 5
algo <- "MacQueen"

##################        #################
tapply(Boston$medv, Boston$zn, mean)
tapply(Boston$medv, Boston$chas, mean)  # 

# 弹性变化
mean(Boston$medv/Boston$age)
(diff(Boston$age)/Boston$age[-1])/(diff(Boston$medv)/Boston$medv[-1])

km <- 
  kmeans(Boston[, 14], centers = n, iter.max = 30, nstart = 2,
         algorithm = algo)
km$centers;km$size


#相关性
cor(Boston)

library(Hmisc)
library(corrplot)

# 显著性水平
rcorr(as.matrix(Boston))
corrplot(Boston[, c(1,2,3)])

library(PerformanceAnalytics)
chart.Correlation(Boston[, c(1,2,3)], histogram = TRUE, pch=19)


# ---feature selection
filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 5)
set.seed(10)
rfWithFilter <- sbf(Boston[, -14], Boston[, 14], sbfControl = filterCtrl)
rfWithFilter  # 竟然全部都选

obj1 <- gafs(x = predictors, 
             y = outcome,
             iters = 100)
obj1

ctrl <- gafsControl(functions = caretGA)
obj <- gafs(x = Boston[, -14], 
            y = Boston[, 14],
            iters = 100,
            gafsControl = ctrl,
            method = "lm",
            parallel = T)

# sim
obj_s <- safs(x = Boston[, -14], 
              y = Boston[, 14],
              iters = 100)

ctrl <- safsControl(functions = caretSA)
obj_s1 <- safs(x = Boston[, -14], 
               y = Boston[, 14],
               iters = 100,
               safsControl = ctrl,
               method = "lm",
               parallel = T)

# ---Split data
library(caret)
set.seed(1024)

trainindex <- createDataPartition(Boston$medv, p = .8,
                                  list = FALSE,
                                  times = 1)

Boston.train <- Boston[trainindex, ]
Boston.valid <- Boston[-trainindex, ]

# ---fitcontrol
fitcontrol <- trainControl(method = 'repeatedcv',
                           number = 15,
                           repeats = 10)
set.seed(100)
rfFit <- train(medv~., data = Boston.train,
               method = 'rf',
               trControl = fitcontrol)

# ---gbm进行
fitControl <- trainControl(## 10-fold CV
                          method = "repeatedcv",
                          number = 10,
                          ## repeated ten times
                          repeats = 10)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                       n.trees = (1:30)*50, 
                       shrinkage = 0.1,
                       n.minobsinnode = 20)

nrow(gbmGrid)

set.seed(100)
training <- scale(Boston.train)
gbmFit <- train(medv~., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit

# --- 
glmFit <- train(medv~., data = Boston.train,
                method = 'glmnet',
                trControl = fitControl)

plsFit <- train(medv~., data = Boston.train,
                  method = 'pls',
                  trControl = fitControl)

trellis.par.set(caretTheme())
plot(rfFit)

predict(gbmFit, newdata = Boston.valid)

library(ggplot2)
ggplot(Boston, aes(x= 1: nrow(Boston),y = medv)) + geom_line()




