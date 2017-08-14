  files <-'G:/data/column.2C.dat'
  files3 <-'G:/data/column.3C.dat'
  w2 <- read.csv(files)
  ch <-lm(V6~.,w2[-116,])
  w[116,6] <- predict(ch,w2[116,-6])
  
  a <- glm(V7~>,w2,family='binomial')
  b <-step(a)
  summary(b)
  z<-(predict(b,w2,type='response')>0.5)
  u <- rep('NO',310);u[!z] = 'AB'
  (zz =table(w2[,7],u))
  (sum(zz)-sum(diag(zz)))/sum(zz)  #计算错判率
  
  a <- glm(V7~>,w2,family=quasibinomial(link='probit'))
  b <-step(a)
  summary(b)
  z<-(predict(b,w2,type='response')>0.5)
  u <- rep('NO',310);u[!z] = 'AB'
  (zz =table(w2[,7],u))
  (sum(zz)-sum(diag(zz)))/sum(zz)  #计算错判率
  
  library(MASS)
  a <- lda(V7~.,data=w2)
  b <- predict(a.w2)$class
  (zz =table(w2[,7],u))
  (sum(zz)-sum(diag(zz)))/sum(zz)  #计算错判率
  
  library(mda)
  a <- mda(V7~.,data=w2)
  b <- predict(a.w2)$class
  (zz =table(w2[,7],u))
  (sum(zz)-sum(diag(zz)))/sum(zz)  #计算错判率
  
  library(MASS)
  w3 <-read.table(files3)
  ch <-lm(V6~.,w3[-116,])
  w3[116,6] <- predict(ch,w3[116,-6])
  a <-lda(V7~.,data=w3)
  b<-predict(a,w3)$class
  z <-table(w3[,7],b)
  (zz =z[oreder(dimnames(z)[[1]]),order(dimnames(z)[[2]])])
  (sum(zz) - sum(diag(zz)))/sum(zz)
  
  file4 <-'G:/data/CTG.NAOMIT.csv'
  w <- read.csv(files4)
  w$CLASS <-factor(w$CLASS)
  w$NSP <- factor(w$NSP)
  w$Tendency <- factor(w$Tendency)
  #五折下标
  d<-1:2126;dd<-list();Z=5
  nn <- levels(w$NSP);KL <-length(nn)
  for(i in 1:KL)dd[[i]]=d[w$NSP==nn[i]]
  kk=NULL;for(i in 1:KL)kk=c(kk,round(length(dd[[i]])/Z))
  set.seed(111)
  yy<-list(NULL,NULL,NULL)
  for (i in 1:KL){xx=list();uu=dd[[i]];
  for(j in 1:(Z-1)){xx[[j]]=sample(uu,kk[i])
  uu=setdiff(uu,xx[[j]])};xx[[Z]]=uu
  for(k in 1:Z)yy[[i]][[k]] = xx[[k]]}
  mm <-list(NULL,NULL,NULL,NULL,NULL)
  for(i in 1:z)for(j in 1:KL)mm[[i]] = c(mm[[i]],yy[[j]][[i]])
  
  library(rpart.plot)
  (a =rpart(NSP~.,w))
  rpart.plot(a,type=2,extra = 4)
  wp <- predict(a,w,type='class')
  z <- table(w[,23],wp)
  z0<-z[order(dimnames(z)[[1]],order(dimnames((z)[[2]])))]
  E0 = (sum(z0)-sum(diag(z0)))/sum(z0)
  
  library(rpart)
  E0 <- rep(0,5);E1 =E0
  for(i in 1:5){m ==mm[[i]];
  n0<-2126-length(m);n1 <-length(m)
  a <-rpart(NSP~.,w[-m])
  E0[i] =sum(w[-m,23]!=predict(a,w[-m,],type='class'))/n0
  E1[i] = sum(w[m,23]!=predict(a,w[m,],type='class'))/n1}
  mean(E0);mean(E1)
  
  library(adabag)  #adaboost 分类
  set.seed(4410)
  a <- boosting(NSP~.,w)
  wp <- predict(a,w)$class
  z <-table(w[,23],wp)
  z0 <-z[order(dimnames(z)[[i]]),order(dimnames(z)[[2]])]
  E0<-(sum(z0)-sum(diag(z0)))/sum(z0)
  barplot(a$importance,cex.names = .6)
  a$trees #打印所有的树
  a$trees[[1]]
  rpart.plot(a$trees[[100]],type=2,extra = 4)
  
  set.seed(4410)
  E0 <- rep(0,5);E1 =E0
  for(i in 1:5){m==mm[[i]]
    no=2126-length(n);n1 =length(m)
    a = boosting(NSP~.,w[-m,])
    E0[i]=sum(w[-m,23]!=predict(a,w[-m,])$class)/no
    E1[J]=sum(w[m,23]!=predict(a,w[m,])$class)/n1}
  mean(E0);mean(E1)
  #---------------------------
  set.seed(1044)
  a=bagging(NSP~.,w)
  wp<-predict(a,w)$class
  z <- table(w[,23],wp)
  z0 <-z[order(dimnames(z)[[i]]),order(dimnames(z)[[2]])]
  E0<-(sum(z0)-sum(diag(z0)))/sum(z0)
  barplot(a$importance,cex.names = .6)
  
  set.seed(4410)
  E0 <- rep(0,5);E1 =E0
  for(i in 1:5){m==mm[[i]]
    no=2126-length(n);n1 =length(m)
    a = bagging(NSP~.,w[-m,])
    E0[i]=sum(w[-m,23]!=predict(a,w[-m,])$class)/no
    E1[J]=sum(w[m,23]!=predict(a,w[m,])$class)/n1}
  mean(E0);mean(E1)
  #------------------
  library(randomForest)
  a <- randomForest(NSP~.,data =w,importance=TRUE,proximity=TRUE)
  wp <- predict(a,w)$class
  z <-table(w[,23],wp)
  (z0 <-z[order(dimnames(z)[[i]]),order(dimnames(z)[[2]])])
  (E0<-(sum(z0)-sum(diag(z0)))/sum(z0))
  
  par(mfrow=c(3,1))
  matplot(importance(a)[,1:3],type='o',pch=1:3,lty=1:3,col=1,
          xlab='variable number',ylab='importance')
  title('variable importance for three levels of response')
  legend('topleft',legend = paste('nsp=',1:3,sep=''),pch=1:3,lty=1:3,col=1)
  barplot(importance(a)[,4],cex.names = 0.6)
  title('variable importance according to mean decrease accuracy')
  barplot(importance(a)[,5],cex.names = 0.6)
  title('variable importance according to mean decrease gini')
  par(mfrow=c(1,1))
  
  library(randomForest)
  set.seed(23451111)
  E0 <- rep(0,5);E1 =E0
  for(i in 1:5){m==mm[[i]]
    no=2126-length(n);n1 =length(m)
    a = randomForest(NSP~.,data=w[-m,])
    E0[i]=sum(w[-m,23]!=predict(a,w[-m,])$class)/no
    E1[J]=sum(w[m,23]!=predict(a,w[m,])$class)/n1}
  mean(E0);mean(E1)
  #-------------SVM
  library(e1071)
  a <- svm(NSP~.,data =w,kernal='sigmoid')
  wp <- predict(a,w)
  z <-table(w[,23],wp)
  (z0 <-z[order(dimnames(z)[[i]]),order(dimnames(z)[[2]])])
  (E0<-(sum(z0)-sum(diag(z0)))/sum(z0))
  
  set.seed(23451111)
  E0 <- rep(0,5);E1 =E0
  for(i in 1:5){m==mm[[i]]
    no=2126-length(n);n1 =length(m)
    a = svm(NSP~.,data=w[-m,],kernal='sigmoid')
    E0[i]=sum(w[-m,23]!=predict(a,w[-m,])$class)/no
    E1[J]=sum(w[m,23]!=predict(a,w[m,])$class)/n1}
  mean(E0);mean(E1)
  
  #----------------------最领近
  library(knn)
  a <- kknn(NSP~.,train=w,test=w)
  wp <- a$fit
  z <-table(w[,23],wp)
  (z0 <-z[order(dimnames(z)[[i]]),order(dimnames(z)[[2]])])
  (E0<-(sum(z0)-sum(diag(z0)))/sum(z0))
  
  set.seed(23451111)
  E0 <- rep(0,5);E1 =E0
  for(i in 1:5){m==mm[[i]]
    no=2126-length(n);n1 =length(m)
    a0 <- kknn(NSP~.,train=w[-m,],test=w[-m,])
    z0 <-table(w$NSP[-m],a0$fit)
    a1 <-kknn(NSP~.,train=w[m,],test=w[m,])
    E0[i]=sum(w[-m,23]!=predict(a,w[-m,])$class)/no
    E1[J]=sum(w[m,23]!=predict(a,w[m,])$class)/n1}
  mean(E0);mean(E1)
  
  #-------------因变量为频数
  url <-'http://www.stat.berkeley.edu/~statlabs/data/hemophilia.data'
  w <- read.csv(url)
  w$factor <- factor(w$factor);w$hiv <- factor(w$hiv)
  w$year <- factor(w$year)
  
  d= 1:2144;dd=list();Z=5
  nn <- levels(w$factor);KL<-length(nn)
  for(i in 1:KL)dd[[i]]=d[w$factor==n[[i]]]
  kk=NULL;for(i in 1:Z) kk = c(kk,round(length(dd[[i]]))/Z)
  set.seed(111);yy=list(NULL,NULL,NULL,NULL)
  for(i in 1:Z){xx=list();uu=dd[[i]];for(j in 1:(Z-1))
  {xx[[j]]=sample(uu,kk[i]);uu=setdiff(uu,xx[[j]])};xx[[Z]]=uu
    for(k in 1:Z)yy[[i]][[k]]=xx[[k]]}
  mm<-list(NULL,NULL,NULL,NULL)
  for(i in 1:Z)for(j in 1:KL)mm[[i]]=c(mm[[i]],y[[j]][[i]])
  
  ap<- glm(deaths~.,family = 'possion',data=w)
  summary(ap);AIC(ap)
  ap2 = glm(deaths~hiv+factor+py+age,family = 'poisson',data=w)
  summary(ap2);AIC(ap2)
  ap3 <- glm(deaths~hiv+factor+py+age+I(age^2)+I(py^2)+hiv:age+hiv:factor,family = 'poisson',data=w)
  AIC(ap3);summary(ap3)
  anova(ap,ap2,ap3) #个模型比较
  
  #----------------------------
  NMSE <- rep(0,5);NMSEP <- NMSE
  for( i in 1:5){m = mm[[i]]
  a = glm(deaths~.,family = 'possion',data=w[-m,])
  y0 = predict(a,w[-m])
  y1 = predict(a,w[m,])
  NMSEO[i] = mean((w[-m,6]-y0)^2)/mean(w[-m,6]-mean(w[-m,6])^2)
  NMSE[i] =  mean((w[-m,6]-y1)^2)/mean(w[-m,6]-mean(w[-m,6])^2)}
  (MNMSEO = mean(NMSEO));(NMMSE=mean(NMSE))
  
  library(dglm)
  as <- dglm(deaths~hiv + factor+age+py,~hiv+factor+age+py,family = tweedie(var.power =1,link.power =0.2),data=2)
  summary(at)
  summary(at$dispersion.fit)
  
  NMSE <- rep(0,5);NMSEP <- NMSE
  for( i in 1:5){m = mm[[i]]
  a = dglm(deaths~hiv + factor+age+py,~hiv+factor+age+py,family = tweedie(var.power =1,link.power =0.2),data=2)
  y0 = predict(a,w[-m])
  y1 = predict(a,w[m,])
  NMSEO[i] = mean((w[-m,6]-y0)^2)/mean(w[-m,6]-mean(w[-m,6])^2)
  NMSE[i] =  mean((w[-m,6]-y1)^2)/mean(w[-m,6]-mean(w[-m,6])^2)}
  (MNMSEO = mean(NMSEO));(NMMSE=mean(NMSE))
  
  
  library(pscl)
  az <- zeroninf1
   