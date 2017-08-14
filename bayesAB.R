  #bayesAB testing
  library(bayesAB)
  
  #testing 用一个分布去模拟其情况,distribution说明了后面的缝补,n_samples是抽样的
  #次数,prior是一个list里面记录了各种各样的东东
  
  A_binom <- rbinom(100, 1, .5)
  B_binom <- rbinom(100, 1, .6)
  
  A_norm <- rnorm(100, 6, 1.5)
  B_norm <- rnorm(100, 5, 2.5)
  
  AB1 <- bayesTest(A_binom, B_binom, priors = c('alpha' = 1, 'beta' = 1), distribution = 'bernoulli')
  AB2 <- bayesTest(A_norm, B_norm, 
                   priors = c('m0' = 5, 'k0' = 1, 's_sq0' = 3, 'v0' = 1), distribution = 'normal')
  
  print(AB1)
  summary(AB1)
  plot(AB1)
  
  print(AB2)
  summary(AB2)
  plot(AB2)
  
  # Create a new variable that is the probability multiiplied 
  # by the normally distributed variable (expected value of something)
  AB3 <- combine(AB1, AB2, f = `*`, params = c('Probability', 'Mu'), newName = 'Expectation')
  
  print(AB3)
  summary(AB3)
  plot(AB3)
  
  #对于两个bayseAB测试选择在给定的f()下运算
  A_binom <- rbinom(100, 1, .5)
  B_binom <- rbinom(100, 1, .6)
  
  A_norm <- rnorm(100, 6, 1.5)
  B_norm <- rnorm(100, 5, 2.5)
  
  AB1 <- bayesTest(A_binom, B_binom, priors = c('alpha' = 1, 'beta' = 1), distribution = 'bernoulli')
  AB2 <- bayesTest(A_norm, B_norm, 
                   priors = c('m0' = 5, 'k0' = 1, 's_sq0' = 3, 'v0' = 1), distribution = 'normal')
  
  AB3 <- combine(AB1, AB2, f = `*`, params = c('Probability', 'Mu'), newName = 'Expectation')
  
  print(AB3)
  summary(AB3)
  plot(AB3)
  
  ##############################################################################
  #bayes logistic regerssion
  
  ## From UCI Machine Learning Repository.
  data(spambase);
  
  ## A subset of the data.
  sbase = spambase[seq(1,nrow(spambase),10),];
  
  X = model.matrix(is.spam ~ word.freq.free + word.freq.1999, data=sbase);
  y = sbase$is.spam;
  
  ## Run logistic regression.
  output = logit(y, X, samp=1000, burn=100);
  
  ## Run logistic regression.
  output = logit.EM(y, X);
  
  ## Use the iris dataset.
  data(iris)
  N = nrow(iris)
  P = ncol(iris)
  J = nlevels(iris$Species)
  
  X     = model.matrix(Species ~ ., data=iris);
  y.all = model.matrix(~ Species - 1, data=iris);
  y     = y.all[,-J];
  
  out = mlogit(y, X, samp=1000, burn=100);
  
  #Polya-Gamma Random Variates
  rpg(num=1, h=1, z=0.0)
  
  rpg.gamma(num=1, h=1, z=0.0, trunc=200)
  
  rpg.devroye(num=1, n=1, z=0.0)
  
  rpg.alt(num=1, h=1, z=0.0)
  
  rpg.sp(num=1, h=1, z=0.0, track.iter=FALSE)
  
  #Draw Indicators
  draw.indicators(res, nmix)
  draw.indicators.C(res, nmix)
  draw.indicators.R(res, nmix)
  
  #############################################################
  #Bayesian Additive Regression Trees
  ##simulate data (example from Friedman MARS paper)
  f = function(x){
    10*sin(pi*x[,1]*x[,2]) + 20*(x[,3]-.5)^2+10*x[,4]+5*x[,5]
  }
  sigma = 1.0  #y = f(x) + sigma*z , z~N(0,1)
  n = 100      #number of observations
  set.seed(99)
  x=matrix(runif(n*10),n,10) #10 variables, only first 5 matter
  Ey = f(x)
  y=Ey+sigma*rnorm(n)
  lmFit = lm(y~.,data.frame(x,y)) #compare lm fit to BART later
  ##run BART
  set.seed(99)
  bartFit = bart(x,y,ndpost=200) #default is ndpost=1000, this is to run example fast.
  plot(bartFit) # plot bart fit
  ##compare BART fit to linear matter and truth = Ey
  fitmat = cbind(y,Ey,lmFit$fitted,bartFit$yhat.train.mean)
  colnames(fitmat) = c('y','Ey','lm','bart')
  print(cor(fitmat))
  
  ##simulate data 
  f = function(x) { return(.5*x[,1] + 2*x[,2]*x[,3]) }
  sigma=.2 # y = f(x) + sigma*z
  n=100 #number of observations
  set.seed(27)
  x = matrix(2*runif(n*3)-1,ncol=3) ; colnames(x) = c('rob','hugh','ed')
  Ey = f(x)
  y = Ey +  sigma*rnorm(n)
  lmFit = lm(y~.,data.frame(x,y)) #compare lm fit to BART later
  par(mfrow=c(1,3)) #first two for pdbart, third for pd2bart
  ##pdbart: one dimensional partial dependence plot
  set.seed(99)
  pdb1 = pdbart(x,y,xind=c(1,2),
                levs=list(seq(-1,1,.2),seq(-1,1,.2)),pl=FALSE,
                keepevery=10,ntree=100,nskip=100,ndpost=200) #should run longer!
  plot(pdb1,ylim=c(-.6,.6))
  ##pd2bart: two dimensional partial dependence plot
  set.seed(99)
  pdb2 = pd2bart(x,y,xind=c(2,3),
                 levquants=c(.05,.1,.25,.5,.75,.9,.95),pl=FALSE,
                 ntree=100,keepevery=10,verbose=FALSE,nskip=100,ndpost=200) #should run longer!
  plot(pdb2)
  ##compare BART fit to linear model and truth = Ey
  fitmat = cbind(y,Ey,lmFit$fitted,pdb1$yhat.train.mean)
  colnames(fitmat) = c('y','Ey','lm','bart')
  print(cor(fitmat))
  ## plot.bart(pdb1) displays the BART run used to get the plot.
  
  ################################elrm: exact-like inference in logistic regression models
  ###############################################################################
  # Drug dataset example with sex and treatment as the variables of interest
  data(drugDat);
  drug.elrm = elrm(formula=recovered/n~sex+treatment, interest=~sex+treatment, 
                   r=4,iter=50000, burnIn=1000, dataset=drugDat);
  
  ## Not run: 
  # crash dataset example where the terms of interest are age and 
  # the interaction of age and velocity.
  data(crashDat);
  # The following call produces the error message shown below.
  crash.elrm = elrm(formula=y/n~vel+age+acl+age:vel, interest=~age+age:vel, r=4,
                    iter=5000, dataset=crash, burnIn=100);
  
  # Error in getDesignMatrix(formula, interest, dataset = dataset) : 
  # the 'term.labels' attribute of 'terms.formula(interest)' must match those 
  # found in 'terms.formula(formula)'
  
  # The error occurs, because the variables within the interaction term in a formula 
  # are re-ordered by the ordering in which the variables occur. Thus, the interaction 
  # between age and velocity is labeled as vel:age in the 'formula' model and as age:vel 
  # in the 'interest' model.
  
  attr(terms.formula(y/n~vel+age+acl+age:vel),"term.labels");
  # [1] "vel"     "age"     "acl"     "vel:age"
  
  attr(terms.formula(~age+age:vel),"term.labels");
  # [1] "age"     "age:vel"
  
  # To get around this problem, place age before vel in the 'formula' model.
  crash.elrm = elrm(formula=y/n~age+vel+acl+age:vel, interest=~age+age:vel, r=4, 
                    iter=5000, dataset=crash, burnIn=100);
  
  ## End(Not run)
  
  ## Not run: 
  # Urinary tract dataset example with dia as the variable of interest
  data(utiDat); 
  uti.elrm = elrm(uti/n~age+current+dia+oc+pastyr+vi+vic+vicl+vis, interest=~dia,r=4, 
                  iter=30000,burnIn=1000, dataset=utiDat);
  ## End(Not run)
  
  ## Not run: 
  # Titanic dataset example where the variable of interest, class, is coded as a factor
  data(titanDat); 
  titanic.elrm = elrm(surv/n~as.factor(class)+age+sex, interest=~as.factor(class), r=4, 
                      iter=50000, burnIn=1000, dataset=titanDat);
  ## End(Not run)