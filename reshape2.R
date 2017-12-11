##---add_margins Add margins to a data frame.---##
add_margins(df, vars, margins = TRUE)add_margins(df, vars, margins = TRUE)
dcast(data, formula, fun.aggregate = NULL, ..., margins = NULL,
      subset = NULL, fill = NULL, drop = TRUE,
      value.var = guess_value(data))
acast(data, formula, fun.aggregate = NULL, ..., margins = NULL,
      subset = NULL, fill = NULL, drop = TRUE,
      value.var = guess_value(data))

#Air quality example
names(airquality) <- tolower(names(airquality))
aqm <- melt(airquality, id=c("month", "day"), na.rm=TRUE)
acast(aqm, day ~ month ~ variable)
acast(aqm, month ~ variable, mean)
acast(aqm, month ~ variable, mean, margins = TRUE)
dcast(aqm, month ~ variable, mean, margins = c("month", "variable"))
library(plyr) # needed to access . function
acast(aqm, variable ~ month, mean, subset = .(variable == "ozone"))

acast(aqm, variable ~ month, mean, subset = .(month == 5))
#Chick weight example
names(ChickWeight) <- tolower(names(ChickWeight))
chick_m <- melt(ChickWeight, id=2:4, na.rm=TRUE)
dcast(chick_m, time ~ variable, mean) # average effect of time
dcast(chick_m, diet ~ variable, mean) # average effect of diet
acast(chick_m, diet ~ time, mean) # average effect of diet & time
# How many chicks at each time? - checking for balance
acast(chick_m, time ~ diet, length)
acast(chick_m, chick ~ time, mean)
acast(chick_m, chick ~ time, mean, subset = .(time < 10 & chick < 20))
acast(chick_m, time ~ diet, length)
dcast(chick_m, diet + chick ~ time)
acast(chick_m, diet + chick ~ time)
acast(chick_m, chick ~ time ~ diet)
acast(chick_m, diet + chick ~ time, length, margins="diet")
acast(chick_m, diet + chick ~ time, length, drop = FALSE)
#Tips example
dcast(melt(tips), sex ~ smoker, mean, subset = .(variable == "total_bill"))
ff_d <- melt(french_fries, id=1:4, na.rm=TRUE)
acast(ff_d, subject ~ time, length)
acast(ff_d, subject ~ time, length, fill=0)
dcast(ff_d, treatment ~ variable, mean, margins = TRUE)
dcast(ff_d, treatment + subject ~ variable, mean, margins="treatment")
if (require("lattice")) {
  lattice::xyplot(`1` ~ `2` | variable, dcast(ff_d, ... ~ rep), aspect="iso")
}


##---colsplit Split a vector into multiple columns---##
colsplit(string, pattern, names)
x <- c("a_1", "a_2", "b_2", "c_3")
vars <- colsplit(x, "_", c("trt", "time"))
vars
str(vars)

##---french_fries Sensory data from a french fries experiment---##
##---melt Convert an object into a molten data frame.---##
melt(data, ..., na.rm = FALSE, value.name = "value")
##cast
## S3 method for class 'array'
melt(data, varnames = names(dimnames(data)), ...,
     na.rm = FALSE, as.is = FALSE, value.name = "value")
## S3 method for class 'table'
melt(data, varnames = names(dimnames(data)), ...,
     na.rm = FALSE, as.is = FALSE, value.name = "value")
## S3 method for class 'matrix'
melt(data, varnames = names(dimnames(data)), ...,
     na.rm = FALSE, as.is = FALSE, value.name = "value")

a <- array(c(1:23, NA), c(2,3,4))
melt(a)
melt(a, na.rm = TRUE)
melt(a, varnames=c("X","Y","Z"))
dimnames(a) <- lapply(dim(a), function(x) LETTERS[1:x])
melt(a)
melt(a, varnames=c("X","Y","Z"))
dimnames(a)[1] <- list(NULL)
melt(a)

##---melt.data.frame Melt a data frame into form suitable for easy casting.---##
## S3 method for class 'data.frame'
melt(data, id.vars, measure.vars,
     variable.name = "variable", ..., na.rm = FALSE, value.name = "value",
     factorsAsStrings = TRUE)

names(airquality) <- tolower(names(airquality))
melt(airquality, id=c("month", "day"))
names(ChickWeight) <- tolower(names(ChickWeight))
melt(ChickWeight, id=2:4)

##---melt.default Melt a vector. For vectors, makes a column of a data frame---##
## S3 method for class 'list'
melt(data, ..., level = 1)

a <- as.list(c(1:4, NA))
melt(a)
names(a) <- letters[1:4]
melt(a)
a <- list(matrix(1:4, ncol=2), matrix(1:6, ncol=2))
melt(a)
a <- list(matrix(1:4, ncol=2), array(1:27, c(3,3,3)))
melt(a)
melt(list(1:5, matrix(1:4, ncol=2)))
melt(list(list(1:3), 1, list(as.list(3:4), as.list(1:2))))

##---melt_check Check that input variables to melt are appropriate.---##
melt_check(data, id.vars, measure.vars, variable.name, value.name)
##---parse_formula Parse casting formulae Description---##
parse_formula(formula = "... ~ variable", varnames, value.var = "value")

reshape2:::parse_formula("a + ...", letters[1:6])
reshape2:::parse_formula("a ~ b + d")
reshape2:::parse_formula("a + b ~ c ~ .")

##---recast Recast: melt and cast in a single step---##
recast(data, formula, ..., id.var, measure.var)
recast(french_fries, time ~ variable, id.var = 1:4)

