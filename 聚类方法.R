  #---------------------------------kmeans--------------------------------
  maxmin <-function(x){
      (x-min(x))/(max(x)-min(x))
  }
  maxmin_norm <- maxmin(data_rate)
  
  maxmin_norm <- maxmin_norm[1:10000,]
  library(fpc)
  #聚几个类
  K <- 2:5
  round <- 1 # 每次迭代30次，避免局部最优
  rst <- sapply(K, function(i){
    print(paste("K=",i))
    mean(sapply(1:round,function(r){
      print(paste("Round",r))
      result <- kmeans(maxmin_norm, i)
      stats <- cluster.stats(dist(maxmin_norm), result$cluster)
      stats$avg.silwidth
    }))
  })
  plot(K,rst,type='l',main='轮廓系数与K的关系', ylab='轮廓系数')
  
  
  # 降纬度观察
  old.par <- par(mfrow = c(1,2))
  k = 2 # 根据上面的评估 k=2最优
  clu <- kmeans(maxmin_norm,k)
  mds = cmdscale(dist(maxmin_norm,method="euclidean"))
  plot(mds, col=clu$cluster, main='kmeans聚类 k=2', pch = 19)
  plot(mds, col=iris$Species, main='原始聚类', pch = 19)
  par(old.par)
  
  
  
  #-----------------------------------------几种方法-----------------
  library(gclus)
  data(wine)
  head(wine)
  dataset <- wine[,1]  #去除分类标签
  dataset <- scale(dataset)
  
  #----mclust包
  library(mclust)
  m_clust<- Mclust(as.matrix(dataset),G=1:3)
  summary(m_clust)
  
  plot(m_clust,'BIC')
  
  #----Nbclust包
  library(NbClust)
  set.seed(1234) #因为method选择的是kmeans，所以如果不设定种子，每次跑得结果可能不同
  nb_clust <- NbClust(data_rate,  distance = "euclidean",
                      min.nc=1, max.nc=4, method = "kmeans",
                      index = "ch", alphaBeale = 0.1)
  barplot(table(nb_clust$Best.nc[1,]),xlab = "聚类数",ylab = "支持指标数")
  
  #----组内平方误差和——拐点图
  wssplot <- function(data, nc=15, seed=1234){
    wss <- (nrow(data)-1)*sum(apply(data,2,var))
    for (i in 2:nc){
      set.seed(seed)
      wss[i] <- sum(kmeans(data, centers=i)$withinss)
    }
    plot(1:nc, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares")}
  
  wssplot(dataset)
  
  library(factoextra)
  library(ggplot2)
  set.seed(1234)
  fviz_nbclust(dataset, kmeans, method = "wss") +
    geom_vline(xintercept = 3, linetype = 2)
  
  km.res <- kmeans(dataset,3)
  fviz_cluster(km.res, data = dataset)
  
  #----PAM(Partitioning Around Medoids) 围绕中心点的分割算法
  library(fpc)
  pamk.best <- pamk(dataset)
  pamk.best$nc
  
  library(cluster)
  clusplot(pam(dataset, pamk.best$nc))
  
  #----Calinsky criterion
  library(vegan)
  ca_clust <- cascadeKM(dataset, 1, 10, iter = 1000)
  ca_clust$results
  
  calinski.best <- as.numeric(which.max(ca_clust$results[2,]))
  calinski.best
  
  plot(fit, sortg = TRUE, grpmts.plot = TRUE)
  
  calinski<-as.data.frame(ca_clust$results[2,])
  calinski$cluster <- c(1:10)
  library(ggplot2)
  ggplot(calinski,aes(x = calinski[,2], y = calinski[,1]))+geom_line()
  
  #----
  library(apcluster)
  ap_clust <- apcluster(negDistMat(r=2), dataset)
  length(ap_clust@clusters)
  
  heatmap(ap_clust)
  
  #---轮廓系数Average silhouette method
  library(factoextra)
  fviz_nbclust(dataset, kmeans, method = "silhouette")
  
  #----Gap Statistic
  library(cluster)
  set.seed(123)
  gap_clust <- clusGap(dataset, kmeans, 10, B = 500, verbose = interactive())
  gap_clust
  
  library(factoextra)
  fviz_gap_stat(gap_clust)
  
  #----层次聚类
  h_dist <- dist(as.matrix(dataset))
  h_clust<-hclust(h_dist)
  plot(h_clust, hang = -1, labels = FALSE)
  rect.hclust(h_clust,3)
  
  #----clustergram
  clustergram(dataset, k.range = 2:8, line.width = 0.004)
  Loading required package: colorspace
  Loading required package: plyr
  
  