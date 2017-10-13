setwd('F:/data')
data <- read.csv('in.csv')

len <- nrow(data)

out_result1 <- vector()
out_result2 <- vector()
for(i in 1000:2000){
  for( j in 1:9){
    j = j + 1
    var <- data[sample(len,i),]
    result <- abs(sum(var$predict)/sum(var$real) -1)
    out_result1 <- c(out_result1, result)
  }
  out_result2 <- c(out_result2, out_result1)
}

plot(sort(out_result2))
t.test(out_result)
plot(out_result, type = 'l')
plot(density(out_result))


out_result1 <- vector()
for( j in 1:1000){
  j = j + 1
  var <- data[sample(len,4000),]
  result <- abs(sum(var$predict)/sum(var$real) -1)
  out_result1 <- c(out_result1, result)
}
a <- as.data.frame(out_result1)
plot(sort(out_result1))
