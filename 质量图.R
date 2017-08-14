  setwd("G:/写作大师")
  hist <- read.csv('hist.csv')
  library(ggplot2)
  ggplot(hist,aes(x=hist$P2)) + geom_histogram(binwidth =0.1,bins = 20,col= 'lightblue') +ggtitle('p2的直方图')
  hist(hist$P2,freq=F,col='lightblue',main='密度分布直方图',xlab='p2值大小')
  lines(density(hist$P2),lwd=2)
  
  qqnorm(hist$P2)
  qqline(hist$P2)
  t.test(hist$P2)
  ###############################################异常值模块
  library(qcc)
  sol <- qcc(hist$P2,type = 'xbar.one',nsigmas = 3)
  
  sol3.3 <-qcc(hist$P2,type = 'xbar.one',plot=F)
  col = c('black','green','red','orange')
  x.text = paste('3.',c(1:15),sep = '')
  plot(hist$P2,
       type='b',xaxt='n',col=col[1],xlim = c(0,110),
       ylim = c(min(sol3.3$limits[1],min(hist$P2))-1,max(sol3.3$limits[2],max(hist$P2))+1),
       pch=19,main = list('p2:均值控制图',cex=1.5),ylab = '值大小')
  warrings.3 <- sol3.3$violations$beyond.limits
  abline(h = sol3.3$limits[2],col=col[3])
  abline(h = sol3.3$limits[1],col = col[3])
  
  points(warrings.3,hist$P2[warrings.3],col = col[4],pch=19)
  text(warrings.3,hist$P2[warrings.3],'异常点')
  