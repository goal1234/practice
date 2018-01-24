x <- 1:10
a <- "print(x)"
class (a)
eval(parse(text = 'mycar'))


# 生成很多数据框
s <- 'data <- 1:100'
eval(parse(text=s))

for(i in 1:100) {
  df <- data.frame(x = rnorm(100))
  
  name <- paste('d',i, sep ='')
  print(name)
  
  line <- paste(name,'<- df[sample(nrow(df), 100,replace = T),]',sep ='')
  eval(parse(text = line))
}

