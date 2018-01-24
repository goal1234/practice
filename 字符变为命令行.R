x <- 1:10
a <- "print(x)"
class (a)
eval(parse(text = a))
