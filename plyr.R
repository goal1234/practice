
##---.(..., .env = parent.frame())---##
.(a, b, c)
.(first = a, second = b, third = c)
.(a ^ 2, b - d, log(c))
as.quoted(~ a + b + c)
as.quoted(a ~ b + c)
as.quoted(c("a", "b", "c"))
# Some examples using ddply - look at the column names
ddply(mtcars, "cyl", each(nrow, ncol))
ddply(mtcars, ~ cyl, each(nrow, ncol))
ddply(mtcars, .(cyl), each(nrow, ncol))
ddply(mtcars, .(log(cyl)), each(nrow, ncol))
ddply(mtcars, .(logcyl = log(cyl)), each(nrow, ncol))
ddply(mtcars, .(vs + am), each(nrow, ncol))
ddply(mtcars, .(vsam = vs + am), each(nrow, ncol))

##---aaply Split array, apply function, and return results in an array.---##
aaply(.data, .margins, .fun = NULL, ..., .expand = TRUE,
      .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE,
      .paropts = NULL)

dim(ozone)
aaply(ozone, 1, mean)
aaply(ozone, 1, mean, .drop = FALSE)
aaply(ozone, 3, mean)
aaply(ozone, c(1,2), mean)
dim(aaply(ozone, c(1,2), mean))
dim(aaply(ozone, c(1,2), mean, .drop = FALSE))

aaply(ozone, 1, each(min, max))
aaply(ozone, 3, each(min, max))
standardise <- function(x) (x - min(x)) / (max(x) - min(x))
aaply(ozone, 3, standardise)
aaply(ozone, 1:2, standardise)
aaply(ozone, 1:2, diff)

##---adply Split array, apply function, and return results in a data frame---##
adply(.data, .margins, .fun = NULL, ..., .expand = TRUE,
      .progress = "none", .inform = FALSE, .parallel = FALSE,
      .paropts = NULL, .id = NA)


##---alply Split array, apply function, and return results in a list.---##
alply(.data, .margins, .fun = NULL, ..., .expand = TRUE,
      .progress = "none", .inform = FALSE, .parallel = FALSE,
      .paropts = NULL, .dims = FALSE)

alply(ozone, 3, quantile)
alply(ozone, 3, function(x) table(round(x)))

##---arrange Order a data frame by its colums---##
arrange(df, ...)
# sort mtcars data by cylinder and displacement
mtcars[with(mtcars, order(cyl, disp)), ]
# Same result using arrange: no need to use with(), as the context is implicit
# NOTE: plyr functions do NOT preserve row.names
arrange(mtcars, cyl, disp)
# Let's keep the row.names in this example
myCars = cbind(vehicle=row.names(mtcars), mtcars)
arrange(myCars, cyl, disp)
# Sort with displacement in descending order
arrange(myCars, cyl, desc(disp))

##---as.quoted Convert input to quoted variables.---##
as.quoted(x, env = parent.frame())
as.quoted(c("a", "b", "log(d)"))
as.quoted(a ~ b + log(d))


##---a_ply Split array, apply function, and discard results---##
a_ply(.data, .margins, .fun = NULL, ..., .expand = TRUE,
      .progress = "none", .inform = FALSE, .print = FALSE,
      .parallel = FALSE, .paropts = NULL)
##---baseball Yearly batting records for all major league baseball players---##
baberuth <- subset(baseball, id == "ruthba01")
baberuth$cyear <- baberuth$year - min(baberuth$year) + 1
calculate_cyear <- function(df) {
  mutate(df,
         cyear = year - min(year),
         cpercent = cyear / (max(year) - min(year))
  )
}
baseball <- ddply(baseball, .(id), calculate_cyear)
baseball <- subset(baseball, ab >= 25)
model <- function(df) {
  lm(rbi / ab ~ cyear, data=df)
}
model(baberuth)
models <- dlply(baseball, .(id), model)

##---colwise Column-wise function.---##
colwise(.fun, .cols = true, ...)
catcolwise(.fun, ...)
numcolwise(.fun, ...)

# Count number of missing values
nmissing <- function(x) sum(is.na(x))
# Apply to every column in a data frame
colwise(nmissing)(baseball)
# This syntax looks a little different. It is shorthand for the
# the following:
f <- colwise(nmissing)
f(baseball)
# This is particularly useful in conjunction with d*ply
ddply(baseball, .(year), colwise(nmissing))
# To operate only on specified columns, supply them as the second
# argument. Many different forms are accepted.
ddply(baseball, .(year), colwise(nmissing, .(sb, cs, so)))
ddply(baseball, .(year), colwise(nmissing, c("sb", "cs", "so")))
ddply(baseball, .(year), colwise(nmissing, ~ sb + cs + so))
# Alternatively, you can specify a boolean function that determines
# whether or not a column should be included
ddply(baseball, .(year), colwise(nmissing, is.character))
ddply(baseball, .(year), colwise(nmissing, is.numeric))
ddply(baseball, .(year), colwise(nmissing, is.discrete))
# These last two cases are particularly common, so some shortcuts are
# provided:
ddply(baseball, .(year), numcolwise(nmissing))
ddply(baseball, .(year), catcolwise(nmissing))

# You can supply additional arguments to either colwise, or the function
# it generates:
numcolwise(mean)(baseball, na.rm = TRUE)
numcolwise(mean, na.rm = TRUE)(baseball)

##---count Count the number of occurences.---##
count(df, vars = NULL, wt_var = NULL)

# Count of each value of "id" in the first 100 cases
count(baseball[1:100,], vars = "id")
# Count of ids, weighted by their "g" loading
count(baseball[1:100,], vars = "id", wt_var = "g")
count(baseball, "id", "ab")
count(baseball, "lg")
# How many stints do players do?
count(baseball, "stint")
# Count of times each player appeared in each of the years they played
count(baseball[1:100,], c("id", "year"))
# Count of counts
count(count(baseball[1:100,], c("id", "year")), "id", "freq")
count(count(baseball, c("id", "year")), "freq")

##---create_progress_bar Create progress bar.---#
create_progress_bar(name = "none", ...)

# No progress bar
l_ply(1:100, identity, .progress = "none")
## Not run:
# Use the Tcl/Tk interface
l_ply(1:100, identity, .progress = "tk")
## End(Not run)
# Text-based progress (|======|)
l_ply(1:100, identity, .progress = "text")
# Choose a progress character, run a length of time you can see
l_ply(1:10000, identity, .progress = progress_text(char = "."))

##---daply Split data frame, apply function, and return results in an array.---##
daply(.data, .variables, .fun = NULL, ..., .progress = "none",
      .inform = FALSE, .drop_i = TRUE, .drop_o = TRUE, .parallel = FALSE,
      .paropts = NULL)

daply(baseball, .(year), nrow)
# Several different ways of summarising by variables that should not be
# included in the summary
daply(baseball[, c(2, 6:9)], .(year), colwise(mean))
daply(baseball[, 6:9], .(baseball$year), colwise(mean))
daply(baseball, .(year), function(df) colwise(mean)(df[, 6:9]))

##---ddply Split data frame, apply function, and return results in a data frame.---##
ddply(.data, .variables, .fun = NULL, ..., .progress = "none",
      .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
# Summarize a dataset by two variables
dfx <- data.frame(
  group = c(rep('A', 8), rep('B', 15), rep('C', 6)),
  sex = sample(c("M", "F"), size = 29, replace = TRUE),
  age = runif(n = 29, min = 18, max = 54)
)
# Note the use of the '.' function to allow
# group and sex to be used without quoting
ddply(dfx, .(group, sex), summarize,
      mean = round(mean(age), 2),
      sd = round(sd(age), 2))
# An example using a formula for .variables
ddply(baseball[1:100,], ~ year, nrow)
# Applying two functions; nrow and ncol
ddply(baseball, .(lg), c("nrow", "ncol"))
# Calculate mean runs batted in for each year
rbi <- ddply(baseball, .(year), summarise,
             mean_rbi = mean(rbi, na.rm = TRUE))
# Plot a line chart of the result
plot(mean_rbi ~ year, type = "l", data = rbi)
# make new variable career_year based on the
# start year for each player (id)
base2 <- ddply(baseball, .(id), mutate,
               career_year = year - min(year) + 1
)

##---defaults Set defaults.---##
desc(1:10)
desc(factor(letters))
first_day <- seq(as.Date("1910/1/1"), as.Date("1920/1/1"), "years")
desc(first_day)

##---dlply Split data frame, apply function, and return results in a list. Description---##
dlply(.data, .variables, .fun = NULL, ..., .progress = "none",
      .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
linmod <- function(df) {
  lm(rbi ~ year, data = mutate(df, year = year - min(year)))
}
models <- dlply(baseball, .(id), linmod)
models[[1]]
coef <- ldply(models, coef)
with(coef, plot(`(Intercept)`, year))
qual <- laply(models, function(mod) summary(mod)$r.squared)
hist(qual)

##---d_ply Split data frame, apply function, and discard results.---##
d_ply(.data, .variables, .fun = NULL, ..., .progress = "none",
      .inform = FALSE, .drop = TRUE, .print = FALSE, .parallel = FALSE,
      .paropts = NULL)

##---each Aggregate multiple functions into a single function.---##
# Call min() and max() on the vector 1:10
each(min, max)(1:10)
# This syntax looks a little different. It is shorthand for the
# the following:
f<- each(min, max)
f(1:10)
# Three equivalent ways to call min() and max() on the vector 1:10
each("min", "max")(1:10)
each(c("min", "max"))(1:10)
each(c(min, max))(1:10)
# Call length(), min() and max() on a random normal vector
each(length, mean, var)(rnorm(100))

##---failwith Fail with specified value.---##
failwith(default = NULL, f, quiet = FALSE)

f <- function(x) if (x == 1) stop("Error!") else 1
## Not run:
f(1)
f(2)
## End(Not run)
safef <- failwith(NULL, f)
safef(1)
safef(2)

##---here Capture current evaluation context.---##
df <- data.frame(a = rep(c("a","b"), each = 10), b = 1:20)
f1 <- function(label) {
  ddply(df, "a", mutate, label = paste(label, b))
}
## Not run: f1("name:")
# Doesn't work because mutate can't find label in the current scope
f2 <- function(label) {
  ddply(df, "a", here(mutate), label = paste(label, b))
}
f2("name:")
# Works :)

##---idata.frame Construct an immutable data frame.---##
system.time(dlply(baseball, "id", nrow))
system.time(dlply(idata.frame(baseball), "id", nrow))

first <- ddply(baseball, "id", summarise, first = min(year))
system.time(b2 <- merge(baseball, first, by = "id", all.x = TRUE))
system.time(b3 <- join(baseball, first, by = "id"))
b2 <- arrange(b2, id, year, stint)
b3 <- arrange(b3, id, year, stint)
stopifnot(all.equal(b2, b3))

##---join_all Recursively join a list of data frames.---##
join_all(dfs, by = NULL, type = "left", match = "all")

dfs <- list(
  a = data.frame(x = 1:10, a = runif(10)),
  b = data.frame(x = 1:10, b = runif(10)),
  c = data.frame(x = 1:10, c = runif(10))
)
join_all(dfs)
join_all(dfs, "x")

##---laply Split list, apply function, and return results in an array---##
laply(.data, .fun = NULL, ..., .progress = "none", .inform = FALSE,
      .drop = TRUE, .parallel = FALSE, .paropts = NULL)

laply(baseball, is.factor)
# cf
ldply(baseball, is.factor)
colwise(is.factor)(baseball)
laply(seq_len(10), identity)
laply(seq_len(10), rep, times = 4)
laply(seq_len(10), matrix, nrow = 2, ncol = 2)

##---ldply Split list, apply function, and return results in a data frame---##
ldply(.data, .fun = NULL, ..., .progress = "none", .inform = FALSE,
      .parallel = FALSE, .paropts = NULL, .id = NA)

##---liply Experimental iterator based version of llply.---##
liply(.iterator, .fun = NULL, ...)

##---llply Split list, apply function, and return results in a list.---##
llply(.data, .fun = NULL, ..., .progress = "none", .inform = FALSE,
      .parallel = FALSE, .paropts = NULL)
llply(llply(mtcars, round), table)
llply(baseball, summary)
# Examples from ?lapply
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
llply(x, mean)
llply(x, quantile, probs = 1:3/4)

##---l_ply Split list, apply function, and discard results---##
l_ply(.data, .fun = NULL, ..., .progress = "none", .inform = FALSE,
      .print = FALSE, .parallel = FALSE, .paropts = NULL)

l_ply(llply(mtcars, round), table, .print = TRUE)
l_ply(baseball, function(x) print(summary(x)))

##---maply Call function with arguments in array or data frame, returning an array.---##
maply(.data, .fun = NULL, ..., .expand = TRUE, .progress = "none",
      .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

maply(cbind(mean = 1:5, sd = 1:5), rnorm, n = 5)
maply(expand.grid(mean = 1:5, sd = 1:5), rnorm, n = 5)
maply(cbind(1:5, 1:5), rnorm, n = 5)

##---mapvalues Replace specified values with new values, in a vector or factor.---##
mapvalues(x, from, to, warn_missing = TRUE)

x <- c("a", "b", "c")
mapvalues(x, c("a", "c"), c("A", "C"))
# Works on factors
y <- factor(c("a", "b", "c", "a"))
mapvalues(y, c("a", "c"), c("A", "C"))
# Works on numeric vectors
z <- c(1, 4, 5, 9)
mapvalues(z, from = c(1, 5, 9), to = c(10, 50, 90))

##---match_df Extract matching rows of a data frame.---##
# count the occurrences of each id in the baseball dataframe, then get the subset with a freq >25
longterm <- subset(count(baseball, "id"), freq > 25)
# longterm
# id freq
# 30 ansonca01 27
# 48 baineha01 27
# ...
# Select only rows from these longterm players from the baseball dataframe

# (match would default to match on shared column names, but here was explicitly set "id")
bb_longterm <- match_df(baseball, longterm, on="id")
bb_longterm[1:5,]

##---mdply Call function with arguments in array or data frame, returning a data frame.---##
mdply(.data, .fun = NULL, ..., .expand = TRUE, .progress = "none",
      .inform = FALSE, .parallel = FALSE, .paropts = NULL)

mdply(data.frame(mean = 1:5, sd = 1:5), rnorm, n = 2)
mdply(expand.grid(mean = 1:5, sd = 1:5), rnorm, n = 2)
mdply(cbind(mean = 1:5, sd = 1:5), rnorm, n = 5)
mdply(cbind(mean = 1:5, sd = 1:5), as.data.frame(rnorm), n = 5)

##---mlply Call function with arguments in array or data frame, returning a list.！！！！！！##
mlply(.data, .fun = NULL, ..., .expand = TRUE, .progress = "none",
      .inform = FALSE, .parallel = FALSE, .paropts = NULL)
mlply(cbind(1:4, 4:1), rep)
mlply(cbind(1:4, times = 4:1), rep)
mlply(cbind(1:4, 4:1), seq)
mlply(cbind(1:4, length = 4:1), seq)
mlply(cbind(1:4, by = 4:1), seq, to = 20)

##---mutate Mutate a data frame by adding new or replacing existing columns.---##
# Examples from transform
mutate(airquality, Ozone = -Ozone)
mutate(airquality, new = -Ozone, Temp = (Temp - 32) / 1.8)
# Things transform can't do
mutate(airquality, Temp = (Temp - 32) / 1.8, OzT = Ozone / Temp)
# mutate is rather faster than transform
system.time(transform(baseball, avg_ab = ab / g))
system.time(mutate(baseball, avg_ab = ab / g))

##---m_ply Call function with arguments in array or data frame, discarding results.---##
m_ply(.data, .fun = NULL, ..., .expand = TRUE, .progress = "none",
      .inform = FALSE, .print = FALSE, .parallel = FALSE, .paropts = NULL)

##---name_rows Toggle row names between explicit and implicit.---##
name_rows(mtcars)
name_rows(name_rows(mtcars))
df <- data.frame(a = sample(10))
arrange(df, a)
arrange(name_rows(df), a)
name_rows(arrange(name_rows(df), a))

##---ozone Monthly ozone measurements over Central America.---##
value <- ozone[1, 1, ]
time <- 1:72
month.abbr <- c("Jan", "Feb", "Mar", "Apr", "May",
                "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month <- factor(rep(month.abbr, length = 72), levels = month.abbr)
year <- rep(1:6, each = 12)
deseasf <- function(value) lm(value ~ month - 1)
models <- alply(ozone, 1:2, deseasf)
coefs <- laply(models, coef)
dimnames(coefs)[[3]] <- month.abbr
names(dimnames(coefs))[3] <- "month"
deseas <- laply(models, resid)
dimnames(deseas)[[3]] <- 1:72
names(dimnames(deseas))[3] <- "time"
dim(coefs)
dim(deseas)

##---plyr plyr: the split-apply-combine paradigm for R.---##
##---progress_text Text progress bar.---##
progress_text(style = 3, ...)

l_ply(1:100, identity, .progress = "text")
l_ply(1:100, identity, .progress = progress_text(char = "-"))

##---progress_time Text progress bar with time.---##
progress_time()

l_ply(1:100, function(x) Sys.sleep(.01), .progress = "time")

##---progress_tk Graphical progress bar, powered by Tk.---##
progress_tk(title = "plyr progress", label = "Working...", ...)
## Not run:
l_ply(1:100, identity, .progress = "tk")
l_ply(1:100, identity, .progress = progress_tk(width=400))
l_ply(1:100, identity, .progress = progress_tk(label=""))
## End(Not run)

##---progress_win Graphical progress bar, powered by Windows.---##
progress_win(title = "plyr progress", ...)

if(exists("winProgressBar")) {
  l_ply(1:100, identity, .progress = "win")
  l_ply(1:100, identity, .progress = progress_win(title="Working..."))
}

##---raply Replicate expression and return results in a array.---##
raply(.n, .expr, .progress = "none", .drop = TRUE)

raply(100, mean(runif(100)))
raply(100, each(mean, var)(runif(100)))
raply(10, runif(4))
raply(10, matrix(runif(4), nrow=2))
# See the central limit theorem in action
hist(raply(1000, mean(rexp(10))))
hist(raply(1000, mean(rexp(100))))
hist(raply(1000, mean(rexp(1000))))

##---rbind.fill Combine data.frames by row, filling in missing columns.---##
rbind.fill(...)
rbind.fill(mtcars[c("mpg", "wt")], mtcars[c("wt", "cyl")])
A <- matrix (1:4, 2)
B <- matrix (6:11, 2)
A
B
rbind.fill.matrix (A, B)
colnames (A) <- c (3, 1)
A
rbind.fill.matrix (A, B)
rbind.fill.matrix (A, 99)

##---rdply Replicate expression and return results in a data frame.---##
rdply(.n, .expr, .progress = "none", .id = NA)
rdply(20, mean(runif(100)))
rdply(20, each(mean, var)(runif(100)))
rdply(20, data.frame(x = runif(2)))


#---rename Modify names by name, not position---##
x <- c("a" = 1, "b" = 2, d = 3, 4)
# Rename column d to "c", updating the variable "x" with the result
x <- rename(x, replace = c("d" = "c"))
x
# Rename column "disp" to "displacement"
rename(mtcars, c("disp" = "displacement"))

##---revalue Replace specified values with new values, in a factor or character vector.---##
revalue(x, replace = NULL, warn_missing = TRUE)
x <- c("a", "b", "c")
revalue(x, c(a = "A", c = "C"))
revalue(x, c("a" = "A", "c" = "C"))
y <- factor(c("a", "b", "c", "a"))
revalue(y, c(a = "A", c = "C"))

##---rlply Replicate expression and return results in a list.---##
mods <- rlply(100, lm(y ~ x, data=data.frame(x=rnorm(100), y=rnorm(100))))
hist(laply(mods, function(x) summary(x)$r.squared))

##---round_any Round to multiple of any number.---##
round_any(135, 10)
round_any(135, 100)
round_any(135, 25)
round_any(135, 10, floor)
round_any(135, 100, floor)
round_any(135, 25, floor)
round_any(135, 10, ceiling)
round_any(135, 100, ceiling)
round_any(135, 25, ceiling)
round_any(Sys.time() + 1:10, 5)
round_any(Sys.time() + 1:10, 5, floor)
round_any(Sys.time(), 3600)

##---r_ply Replicate expression and discard results---##
r_ply(10, plot(runif(50)))
r_ply(25, hist(runif(1000)))

##---splat ｀Splat¨ arguments to a function.---##
splat(flat)
hp_per_cyl <- function(hp, cyl, ...) hp / cyl
splat(hp_per_cyl)(mtcars[1,])
splat(hp_per_cyl)(mtcars)
f <- function(mpg, wt, ...) data.frame(mw = mpg / wt)
ddply(mtcars, .(cyl), splat(f))

##---strip_splits Remove splitting variables from a data frame---##
strip_splits(df)
dlply(mtcars, c("vs", "am"))
dlply(mtcars, c("vs", "am"), strip_splits)

##---summarise Summarise a data frame.---##
summarise(.data, ...)
# Let's extract the number of teams and total period of time
# covered by the baseball dataframe
summarise(baseball,
          duration = max(year) - min(year),
          nteams = length(unique(team)))
# Combine with ddply to do that for each separate id
ddply(baseball, "id", summarise,
      duration = max(year) - min(year),
      nteams = length(unique(team)))

##---take Take a subset along an arbitrary dimension---##
take(x, along, indices, drop = FALSE)
x <- array(seq_len(3 * 4 * 5), c(3, 4, 5))
take(x, 3, 1)
take(x, 2, 1)
take(x, 1, 1)
take(x, 3, 1, drop = TRUE)
take(x, 2, 1, drop = TRUE)
take(x, 1, 1, drop = TRUE)

##---vaggregate Vector aggregate---##
vaggregate(.value, .group, .fun, ..., .default = NULL, .n = nlevels(.group))

# Some examples of use borrowed from ?tapply
n <- 17; fac <- factor(rep(1:3, length.out = n), levels = 1:5)
table(fac)
vaggregate(1:n, fac, sum)
vaggregate(1:n, fac, sum, .default = NA_integer_)
vaggregate(1:n, fac, range)
vaggregate(1:n, fac, range, .default = c(NA, NA) + 0)
vaggregate(1:n, fac, quantile)
# Unlike tapply, vaggregate does not support multi-d output:
tapply(warpbreaks$breaks, warpbreaks[,-1], sum)
vaggregate(warpbreaks$breaks, id(warpbreaks[,-1]), sum)
# But it is about 10x faster
x <- rnorm(1e6)
y1 <- sample.int(10, 1e6, replace = TRUE)
system.time(tapply(x, y1, mean))
system.time(vaggregate(x, y1, mean))

