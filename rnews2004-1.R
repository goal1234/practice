  library(ade4)
  data(USArrests)
  pca1 <- dudi.pca(USArrests,scannf=FALSE,nf=3)
  pca1
  
  score(pca1)
  s.corcircle(pca1$co)
  s.label(pca1$li)
  
  #Distance matrices
  data(yanomama)
  gen <- quasieuclid(as.dist(yanomama$gen))
  geo <- quasieuclid(as.dist(yanomama$geo))
  ant <- quasieuclid(as.dist(yanomama$ant))
  geo1 <- dudi.pco(geo, scann = FALSE, nf = 3)
  gen1 <- dudi.pco(gen, scann = FALSE, nf = 3)
  ant1 <- dudi.pco(ant, scann = FALSE, nf = 3)
  par(mfrow=c(2,2))
  scatter(geo1)
  scatter(gen1)
  scatter(ant1,posi="bottom")
  
  
  #Taking into account groups of individuals
  data(meaudret)
  coa1<-dudi.coa(meaudret$fau, scannf = FALSE)
  bet1<-between(coa1,meaudret$plan$sta,scannf=FALSE)
  plot(bet1)
  
  #Permutation tests Monte-Carlo tests
  test1<-randtest.between(bet1)
  test1
  
  #===The survival package===*
  library(survival)
  
  #Specifying survival data
  data(veteran)
  with(veteran, Surv(time,status))
  with(veteran, Surv(diagtime*30,
                     diagtime*30+time,
                     status))
  #One and two sample summaries
  data(veteran)
  plot(survfit(Surv(time,status)~trt,data=veteran),
       xlab="Years since randomisation",
       xscale=365, ylab="% surviving", yscale=100,
       col=c("forestgreen","blue"))
  
  survdiff(Surv(time,status)~trt, data=veteran)
  
  #Proportional hazards models
  data(pbc)
  mayomodel<-coxph(Surv(time,status)~edtrt+
                     log(bili)+log(protime)+
                     age+platelet,
                   data=pbc, subset=trt>0)
  mayomodel
  
  plot(survfit(Surv(time,status)~edtrt,
               data=pbc,subset=trt==-9))
  lines(survexp(~edtrt+
                  ratetable(edtrt=edtrt,bili=bili,
                            platelet=platelet,age=age,
                            protime=protime),
                data=pbc,
                subset=trt==-9,
                ratetable=mayomodel,
                cohort=TRUE),
        col="purple")
  
  cox.zph(mayomodel)
  
  ## graph for variable 1 (edtrt)
  plot(cox.zph(mayomodel)[1])
  
  