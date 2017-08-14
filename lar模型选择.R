library(lars) #
library(ridge) #lm.ridge函数在ridge包
library(MASS) ##linearRidge函数在MASS包

cement<-data.frame(
  X1=c(7, 1,11,11,7,11,3,1,2,21,1,11,10),
  X2=c(26, 29, 56, 31,52,55,71,31,54,47,40,66,68),
  X3=c(6,15, 8, 8,6,9,17,22,18,4,23,9,8),
  X4=c(60, 52, 20, 47,33,22,6,44,22,26,34,12,12),
  Y =c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2, 102.7, 72.5,
       93.1,115.9, 83.8, 113.3, 109.4) 
)

#所有变量参与回归
summary(lm(Y~.,data=cement)) 
kappa(cor(cement),exact=TRUE)

lm.ridge(Y~.,cement,lambda=seq(0,0.1,0.001))
plot(lm.ridge(Y~.,cement,lambda=seq(0,0.1,0.001))) 

summary(lm(Y~.-X3,data=cement))

summary(lm(Y~.-X3-X4,data=cement))
summary(linearRidge(Y~.,data=cement));#自动取了岭参数Ridge parameter: 0.01473162,舍弃X3，

summary(linearRidge(Y~.-X3,data=cement));#发现X4还只有一个点
summary(linearRidge(Y~.-X3-X4,data=cement));#发现X4还只有一个点

w=as.matrix(cement)
lars(w[,1:4],w[,5])
summary(lars(w[,1:4],w[,5]))
summary(lm(Y~.-X3,data=cement))
