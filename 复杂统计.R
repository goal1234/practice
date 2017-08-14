  url <- 'http://www.statsci.org/data/general/cofreewy.txt'
  w <- read.table(url,header = TRUE)
  a <- lm(CO~.,w)
  summary(a)
  b <- step(a,direction = 'backward')
  summary(b)
  shapiro.test(b$res)
  qqnorm(b$res)
  qqline(b$res)
  plot(w)
  attach(w)
  cor(cbind(CO,Traffic,Tsq = Traffic^2,Tcub = Traffic^3,
            Hour,Hsq = Hour^2,Hcub = Hour^3,Wind,Wsq = Wind^2,Wcub = Wind^3))
  a <- lm(CO~Traffic+Wind +I(Wind^2)+I(Wind^3) +sin((2*pi/24)*Hour)+
            cos((2*pi/24)*Hour)+sin((4*pi/24)*Hour)+cos((4*pi/24)*Hour))
  b <- step(a)
  summary(b);anova(b);shapiro.test(b$res)
  
  b1 <- lm(CO~Traffic+I(Wind^2) +cos((2*pi/24)*Hour)+cos((4*pi)*Hour))
  summary(b1)
  anova(b1)
  shapiro.test(b1$res)
  qqnorm(b1$res);qqline(b1$res)
  
  set.seed(441010)
  x <- c(rnorm(100),50);y <- c(rnorm(100),-50)
  a <-lm(y~x);summary(a)
  shapiro.test(a$res)
  
  library(quantreg)
  par(mfrow =c(1,2))
  data(engel);plot(engel)
  plot(log10(foodexp)~log10(income),data=engel,main = 'engel data (log10-transform)')
  taus <- c(.15,.25,.50,.75,.95,.99)
  rqs <- as.list(taus)
  for (i in seq(along =taus)){
    rqs[[i]] = rq(log10(foodexp)~log10(income),tau=taus[i],data=engel)
    lines(log10(engel$income),fitted(rqs[[i]]),col=i+1)
  }
  legend('bottomright',paste("tau=",taus),inset = .04,col=2:(length(taus)+1),lty=1)
  
  files <- 'G:/data/pharynx.csv'
  w <- read.csv(files)
  w <- w[,-c(1,11)]
  w <- w[w$COND !=9 & w$GRADE !=9,]
  w$COND[w$COND==3|w$COND==4] =2;w$COND[w$COND==1]=0
  write.csv(w,"pyarynx1.csv",quote=F,row.names = F)
  
  #标准回归
  u <- read.csv('pyarynx1.csv')
  x <- 1:11;(x =x[-c(5,11)])
  for(i in x)u[,i]=factor(u[,i])
  a <- lm(TIME~.,data=u);summary(a)
  
  library(MASS)
  boxcox(TIME~.,data=u) #lambda =0.3
  a <- lm(TIME^0.3~INST+SEX+TX+AGE+COND+T.STAGE+N.STAGE+STATUS,data = u);b<-step(a)
  summary(b);anova(b)
  shapiro.test(b$res)
  
  library(survival)
  fit <- survfit(Surv(TIME,as.numeric(STATUS))~TX,data=u)
  plot(fit,lty=1:2,ylab='S(t)',xlab='t',main='Survival Functions')
  legend(1500,1,c('TX=1','TX=2'),lty=1:2)
  
  coxph(Surv(TIME,as.numeric(STATUS))~.,data=u)
  plot(survfit(fit))
  summary(fit)
  
  library(mfp)
  f <- mfp(Surv(TIME,as.numeric(STATUS))~fp(AGE,df=4,select=0.05)+
             INST+SEX+TX+GRADE+COND+SITE+T.STAGE+N.STAGE,family = cox,data=u)
  print(f)
  (rsq <-1-sum(f$residuals)^2/sum((u$TIME-mean(u$TIME))^2))
  
  library(lars)
  data(diabetes)
  w<-diabetes
  kappa(w[,12:75])
  vif(lm(y~.,w[,11:75]))
  
  library(ridge)
  a <- linearRidge(y~.,data=w[,11:75])
  
  library(lars)
  x <- as.matrix(w[,1:10])
  y <- as.matrix(w[,11])
  x2 <- as.matrix(w[,12:75])
  laa <- lar(x2,y)
  plot(laa)
  summary(laa)
  cva <-cv.lars(x2,y,K = 10)
  best <- cva$index[which.min(cva$cv)]
  coef <- coef.lars(laa,mode='fraction',s=best)
  min(laa$Cp)
  coef1<- coef.lars(laa,mode='step',s = 15)
  #adaptive lasso
  library(msgps)
  a1 <- msgps(x2,y,penalty='alasso',gamma=1,lambda=0)
  
  #偏最小二乘回归
  library(lars)
  library(pls)
  ap <- plsr(y~x2,64,validation = 'CV') #求出所有可能的64个因子
  ap$loadings  #看代表性
  ap$coef  #个因子作为线性组合的系数
  validationplot(ap)  #挑选因子
  RMSEP(ap);MSEP(ap);R2(ap)
  
  url <-'https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/regression/mg'
  w <-read.csv(url)
  a <- lm(y~.,w)
  cor(w)
  
  n<nrow(w);zz1 <- 1:n  #所有观察的下标
  zz2 = rep(1:5,ceiling(1385/5))[1:n]
  set.seed(100);zz2 <-sample(zz2,n) #为1-5的所及排序
  NMSE <-rep(0,5);NMSEO =NMSE
  for (i in 1:5){
    m =zz1[zz2==i]
    a =lm(y~.,data=w[-m,])
    y0=predict(a,w[-m,])
    y1 = predict(a,w[m,])
    NMSEO[i] =  mean((w$y[-m]-y0)^2)/mean((w$y[-m] -mean(w$y[-m]))^2)
    NMSE[i] =  mean((w$y[-m]-y1)^2)/mean((w$y[-m] -mean(w$y[-m]))^2)
  }
  MNMSEO <-mean(NMSEO);MNMSE <- mean(NMSE)
  
  library(rpart.plot)
  a <- rpart(y~.,w);a
  rpart.plot(a,type=2,faclen)
  
  library(rpart)
  NMSE <- rep(0,5);NMSEO <- NMSE
  for(i in 1:5){
    m=zz1[zz2==1]
    a=rpart(y~.,w[-m,])
    y0=predict(a,w[-m,])
    y1 =predict(a,w[m,])
    NMSEO[i] =  mean((w$y[-m]-y0)^2)/mean((w$y[-m] -mean(w$y[-m]))^2)
    NMSE[i] =  mean((w$y[-m]-y1)^2)/mean((w$y[-m] -mean(w$y[-m]))^2)
  }
  MNMSEO <-mean(NMSEO);MNMSE <- mean(NMSE)
  
  library(mboost)  #boosting
  NMSE <- rep(0,5);NMSEO <- NMSE
  for(i in 1:5){
    m=zz1[zz2==1]
    a=mboost(y~btree(x1)+btree(x2)+btree(x3)
             +btree(x4) +btree(x5)+btree(x6),data=w[-m,])
    y0=predict(a,w[-m,])
    y1 =predict(a,w[m,])
    NMSEO[i] =  mean((w$y[-m]-y0)^2)/mean((w$y[-m] -mean(w$y[-m]))^2)
    NMSE[i] =  mean((w$y[-m]-y1)^2)/mean((w$y[-m] -mean(w$y[-m]))^2)
  }
  MNMSEO <-mean(NMSEO);MNMSE <- mean(NMSE)
  
  library(ipred)  #bagging
  set.seed(120316)
  NMSE <- rep(0,5);NMSEO <- NMSE
  for(i in 1:5){
    m=zz1[zz2==1]
    a=bagging(y~.,data=w[-m,],coob=T,control=rpart.control(xval=10))
    y0=predict(a,w[-m,])
    y1 =predict(a,w[m,])
    NMSEO[i] =  mean((w$y[-m]-y0)^2)/mean((w$y[-m] -mean(w$y[-m]))^2)
    NMSE[i] =  mean((w$y[-m]-y1)^2)/mean((w$y[-m] -mean(w$y[-m]))^2)
  }
  MNMSEO <-mean(NMSEO);MNMSE <- mean(NMSE)
  
  library(randomForest)
  set.seed(101)
  NMSE <- rep(0,5);NMSEO <- NMSE
  for(i in 1:5){
    m=zz1[zz2==1]
    a=randomForest(y~.,data=w[-m,],importance=TRUE,proximity=TRUE)
    y0=predict(a,w[-m,])
    y1 =predict(a,w[m,])
    NMSEO[i] =  mean((w$y[-m]-y0)^2)/mean((w$y[-m] -mean(w$y[-m]))^2)
    NMSE[i] =  mean((w$y[-m]-y1)^2)/mean((w$y[-m] -mean(w$y[-m]))^2)
  }
  MNMSEO <-mean(NMSEO);MNMSE <- mean(NMSE)
  SS <-randomForest(y~.,data=w,importance=TRUE,proximity=TRUE)
  SS$importance
  
  library(nnet)  #人工神经网络
  set.seed(444)
  NMSE <- rep(0,5);NMSEO <- NMSE
  for(i in 1:5){
    m=zz1[zz2==1]
    a=nnet(w[-m,-1],w[-m,1],size=5,rang=0.1,decay=6e-4,maxit=200)
    y0=predict(a,w[-m,])
    y1 =predict(a,w[m,])
    NMSEO[i] =  mean((w$y[-m]-y0)^2)/mean((w$y[-m] -mean(w$y[-m]))^2)
    NMSE[i] =  mean((w$y[-m]-y1)^2)/mean((w$y[-m] -mean(w$y[-m]))^2)
  }
  MNMSEO <-mean(NMSEO);MNMSE <- mean(NMSE)
  
  library(rminer)
  set.seed(444)
  NMSE <- rep(0,5);NMSEO <- NMSE
  for(i in 1:5){
    m=zz1[zz2==1]
    a=fit(y~.,w[-m],model='svm')
    y0=predict(a,w[-m,])
    y1 =predict(a,w[m,])
    NMSEO[i] =  mean((w$y[-m]-y0)^2)/mean((w$y[-m] -mean(w$y[-m]))^2)
    NMSE[i] =  mean((w$y[-m]-y1)^2)/mean((w$y[-m] -mean(w$y[-m]))^2)
  }
  MNMSEO <-mean(NMSEO);MNMSE <- mean(NMSE)
  plot(1:n,NMSE,type='l',ylim=c(min(NMSE,NMSEO),max(NMSE,NMSEO)),xlab='NUMBER OF TREE',
       ylab='NMSE',main = 'Random Forests NMSE',lty=2)
  lines(1:n,NMSEO)
  