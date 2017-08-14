  #时序上ACF,PACF df检验看平稳性，然后是ARCH.test（）检验，建立ar,ma模型
  #在条件方差下用garch
  library(rugarch)
  args(ugarchspec)
  
  armaOrder (default = (1,1). The order of the ARMA model.)
  include.mean (default = TRUE. Whether the mean is modelled.)
  archm (default = FALSE. The ARCH-in-mean parameter.)
  archpow (default = 1 for standard deviation, else 2 for variance.)
  arfima (default = FALSE. Whether to use fractional differencing.)
  external.regressors (default = NULL. A matrix of external regressors of the same length
                       as the data.)
  archex (default = FALSE. Either FALSE or integer denoting the number of external regressors
          from the end of the matrix to multiply by the conditional standard deviation.).
  AR parameters are 'ar1', 'ar2', ...,
  MA parameters are 'ma1', 'ma2', ...,
  mean parameter is 'mu'
  archm parameter is 'archm'
  the arfima parameter is 'arfima'
  the external regressor parameters are 'mxreg1', 'mxreg2', ...,
  
  model (default = 'sGARCH' (vanilla GARCH). Valid models are 'iGARCH', 'gjrGARCH',
         'eGARCH', 'apARCH','fGARCH','csGARCH' and 'mcsGARCH').
  garchOrder (default = c(1,1). The order of the GARCH model.)
  submodel (default = NULL. In the case of the 'fGARCH' omnibus model, valid choices are
            'GARCH', 'TGARCH', 'GJRGARCH', 'AVGARCH', 'NGARCH', 'NAGARCH', 'APARCH'
            and 'ALLGARCH')
  external.regressors (default = NULL. A matrix of external regressors of the same length
                       as the data).
  variance.targeting (default = FALSE. Whether to include variance targeting. It is also
                      possible to pass a numeric value instead of a logical, in which case it is used for the
                      calculation instead of the variance of the conditional mean equation residuals).
  
  ARCH(q) parameters are 'alpha1', 'alpha2', ...,
  GARCH(p) parameters are 'beta1', 'beta2', ...,
  variance intercept parameter is 'omega'
  the external regressor parameters are 'vxreg1', 'vxreg2', ...,
  
  #SAMPLE
  spec = ugarchspec()
  data(sp500ret)
  fit = ugarchfit(spec = spec, data = sp500ret)
  show(fit)

  #fitering
  data(sp500ret)
  spec = ugarchspec(variance.model = list(model = 'apARCH'), distribution.model = 'std')
  setfixed(spec) <- list(mu = 0.01, ma1= 0,2, ar1 = 0,5, omega = 1e-05, alpha1 = 0.03, beta1=0.9,
                         gamma1 = 0.01, delta = 1, shape = 5)
  filt = ugarchfilter(spec = spec, data = sp500ret)
  show(filt)
  
  #forecasting the garch bootsrap
  data(sp500ret)
  spec = ugarchspec(variance.model=list(model="csGARCH"), distribution="std")
  fit = ugarchfit(spec, sp500ret)
  bootp = ugarchboot(fit, method = c("Partial", "Full")[1],
                       + n.ahead = 500, n.bootpred = 500)
  show(bootp)
  
  #simulation
  #rolling estimation
  data(sp500ret)
  library(parallel)
  cl = makePSOCKcluster(10)
  spec = ugarchspec(variance.model = list(model = "eGARCH"), distribution.model = "jsu")
  roll = ugarchroll(spec, sp500ret, n.start = 1000, refit.every = 100,
                      refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE,
                      VaR.alpha = c(0.01, 0.05), cluster = cl, keep.coef = TRUE)

  show(roll)
  stop(cl)
  
  #simulated parameter distribution and rmse
  spec = ugarchspec(variance.model = list(model = "gjrGARCH"),
                      + distribution.model = "ged")
  print(persistence(pars = unlist(list(mu = 0.001, ar1 = 0.4, ma1 = -0.1,
                                         omega = 1e-06, alpha1 = 0.05, beta1 = 0.9, gamma1 = 0.05,
                                         shape = 1.5)), distribution = "ged", model = "gjrGARCH"))
  library(parallel)
  cl = makePSOCKcluster(10)
  setfixed(spec) <- list(mu = 0.001, ar1 = 0.4, ma1 = -0.1, omega = 1e-06,alpha1 = 0.05, beta1 = 0.9, gamma1 = 0.05, shape = 1.5)
  dist = ugarchdistribution(fitORspec = spec, n.sim = 2000, n.start = 1,
                            m.sim = 100, recursive = TRUE, recursive.length = 6000, recursive.window = 1000,
                            rseed = 1066, solver = "solnp", solver.control = list(trace = 0),
                            cluster = cl)
  stopCluster(cl)
  show(dist)
  
  