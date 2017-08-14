  xulie <- read.delim('clipboard',header = T)
  date <- as.Date(xulie[,1])
  fund <- cbind(date,xulie[,-1])
  library(PerformanceAnalytics)
  library(zoo)
  library(timeDate)
  library(timeSeries)
  fund <- zoo(xulie[,-1],date)                                                                      #转换时不怎么变
  fund.ts <- as.timeSeries(fund)
  fund.ret <- 100*returns(fund.ts,method=c('continuous'),na.rm=T)
  library(fPortfolio)
  ## B-L 
  library(MASS);library(quadprog)
  library(BLCOP)
  pickMatrix <- matrix(c(0.5,0.4),nrow = 1,ncol = 2)
  views <- BLViews(P = pickMatrix,q=0.06,confidences = 100,
                   assetNames = colnames(fund.ret))
  views
  priorMeans <- rep(0, 2)
  priorVarcov <- cov.mve(fund.ret)$cov
  marketPosterior <- posteriorEst(views = views, sigma = priorVarcov,
                                  mu = priorMeans,tau = 1/2)
  finViews <- matrix(ncol = 2, nrow = 1, dimnames = list(NULL, c("E","D")))
  finViews[,1:2] <- rep(1/2,2)
  views <- addBLViews(finViews, 0.15, 90, views)
  views
  marketPosterior <- BLPosterior(as.matrix(fund.ret), views, tau = 1/2,
                                 marketIndex = as.matrix(fund.ret[,1]),riskFree = NULL)
  optPorts <- optimalPortfolios.fPort(marketPosterior, optimizer = "tangencyPortfolio")
  par(mfcol = c(2, 1))
  weightsPie(optPorts$priorOptimPortfolio)
  weightsPie(optPorts$posteriorOptimPortfolio)
  optPorts2 <- optimalPortfolios.fPort(marketPosterior,
                                       constraints = "minW[1:6]=0.1", optimizer = "minriskPortfolio")
  optPorts2
  densityPlots(marketPosterior)
  ###