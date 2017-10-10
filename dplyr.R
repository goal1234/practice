
##all_equal()-----
scramble <- function(x) x[sample(nrow(x)), sample(ncol(x))]
# By default, ordering of rows and columns ignored
all_equal(mtcars, scramble(mtcars))
# But those can be overriden if desired
all_equal(mtcars, scramble(mtcars), ignore_col_order = FALSE)
all_equal(mtcars, scramble(mtcars), ignore_row_order = FALSE)
# By default all_equal is sensitive to variable differences
df1 <- data.frame(x = "a")
df2 <- data.frame(x = factor("a"))
all_equal(df1, df2)
# But you can request dplyr convert similar types
all_equal(df1, df2, convert = TRUE)

##arrange---all,at,if
arrange(mtcars, cyl, disp)
arrange(mtcars, desc(disp))
# grouped arrange ignores groups
by_cyl <- mtcars %>% group_by(cyl)
by_cyl %>% arrange(desc(wt))
# Unless you specifically ask:
by_cyl %>% arrange(desc(wt), .by_group = TRUE)

df <- as_tibble(mtcars)
df
arrange_all(df)
# You can supply a function that will be applied before taking the
# ordering of the variables. The variables of the sorted tibble
# keep their original values.
arrange_all(df, desc)
arrange_all(df, funs(desc(.)))

##between()-----在一个范围内
x <- rnorm(1e2)
x[between(x, -1, 1)]

##bind(),row,col,combine()  rbind_list()------------
one <- mtcars[1:4, ]
two <- mtcars[11:14, ]
# You can supply data frames as arguments:
bind_rows(one, two)
# The contents of lists is automatically spliced:
bind_rows(list(one, two))
bind_rows(split(mtcars, mtcars$cyl))
bind_rows(list(one, two), list(two, one))
# In addition to data frames, you can supply vectors. In the rows
# direction, the vectors represent rows and should have inner
# names:
bind_rows(
  c(a = 1, b = 2),
  c(a = 3, b = 4)
)
# You can mix vectors and data frames:
bind_rows(
  c(a = 1, b = 2),
  data_frame(a = 3:4, b = 5:6),
  c(a = 7, b = 8)
)
# Note that for historical reasons, lists containg vectors are
# always treated as data frames. Thus their vectors are treated as
# columns rather than rows, and their inner names are ignored:
ll <- list(
  a = c(A = 1, B = 2),
  b = c(A = 3, B = 4)
)
bind_rows(ll)
# You can circumvent that behaviour with explicit splicing:
bind_rows(!!! ll)
# When you supply a column name with the
# column is created to link each row to its original data frame
bind_rows(list(one, two), .id = "id")
bind_rows(list(a = one, b = two), .id = "id")
bind_rows("group 1" = one, "group 2" = two, .id = "groups")
# Columns dont need to match when row-binding
bind_rows(data.frame(x = 1:3), data.frame(y = 1:4))
## Not run:
# Rows do need to match when column-binding
bind_cols(data.frame(x = 1), data.frame(y = 1:2))
## End(Not run)
bind_cols(one, two)
bind_cols(list(one, two))
# combine applies the same coercion rules
f1 <- factor("a")
f2 <- factor("b")
c(f1, f2)
unlist(list(f1, f2))
combine(f1, f2)
combine(list(f1, f2))

##case_when()----------------
x <- 1:50
case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  TRUE ~ as.character(x)
)
# Like an if statement, the arguments are evaluated in order, so you must
# proceed from the most specific to the most general. This won't work:
case_when(
TRUE ~ as.character(x),
x %%  5 == 0 ~ "fizz",
x %%  7 == 0 ~ "buzz",
x %% 35 == 0 ~ "fizz buzz"
)
# case_when is particularly useful inside mutate when you want to
# create a new variable that relies on a complex combination of existing
# variables
starwars %>%
select(name:mass, gender, species) %>%
mutate(
type = case_when(
height > 200 | mass > 200 ~ "large",
species == "Droid"        ~ "robot",
TRUE                      ~  "other",
  )
)

# Dots support splicing:
patterns <- list(
  TRUE ~ as.character(x),
  x %%  5 == 0 ~ "fizz",
  x %%  7 == 0 ~ "buzz",
  x %% 35 == 0 ~ "fizz buzz"
)
case_when(!!! patterns)

#coalesce(...)----取代第一个为0的元素
#na_if()可以进行NA值替换
# Use a single value to replace all missing values
x <- sample(c(1:5, NA, NA, NA))
coalesce(x, 0L)

# Or match together a complete vector from missing pieces
y <- c(1, 2, NA, NA, 5)
z <- c(NA, NA, 3, 4, 5)
coalesce(y, z)

# Supply lists by splicing them into dots:
vecs <- list(
  c(1, 2, NA, NA, 5),
  c(NA, NA, 3, 4, 5)
)
coalesce(!!! vecs)  #去重和NA值


##copy_to(),collect(),collapse(x...)
if (require(dbplyr)) {
  mtcars2 <- src_memdb() %>%
    copy_to(mtcars, name = "mtcars2-cc", overwrite = TRUE)
  
  remote <- mtcars2 %>%
    filter(cyl == 8) %>%
    select(mpg:drat)
  # Compute query and save in remote table
  compute(remote)
  # Compute query bring back to this session
  collect(remote)
  # Creates a fresh query based on the generated SQL
  collapse(remote)
}

## Not run:
iris2 <- src_memdb() %>% copy_to(iris, overwrite = TRUE)
iris2
## End(Not run)

##cumall() cumany() cummean()
desc(1:10)
desc(factor(letters))
first_day <- seq(as.Date("1910/1/1"), as.Date("1920/1/1"), "years")
desc(first_day)
starwars %>% arrange(desc(mass))

##distinct()选取不同的数字
df <- tibble(
  x = sample(10, 100, rep = TRUE),
  y = sample(10, 100, rep = TRUE)
)
nrow(df)
nrow(distinct(df))
nrow(distinct(df, x, y))
distinct(df, x)
distinct(df, y)
# Can choose to keep all other variables as well
distinct(df, x, .keep_all = TRUE)
distinct(df, y, .keep_all = TRUE)
# You can also use distinct on computed variables
distinct(df, diff = abs(x - y))
# The same behaviour applies for grouped data frames
# except that the grouping variables are always included
df <- tibble(
  g = c(1, 1, 2, 2),
  x = c(1, 1, 2, 1)
) %>% group_by(g)
df %>% distinct()
df %>% distinct(x)

##filter() select() mutate() summarise() arrange()
##do()
by_cyl <- group_by(mtcars, cyl)
do(by_cyl, head(., 2))
models <- by_cyl %>% do(mod = lm(mpg ~ disp, data = .))
models
summarise(models, rsq = summary(mod)$r.squared)
models %>% do(data.frame(coef = coef(.$mod)))
models %>% do(data.frame(
  var = names(coef(.$mod)),
  coef(summary(.$mod)))
)
models <- by_cyl %>% do(
  mod_linear = lm(mpg ~ disp, data = .),
  mod_quad = lm(mpg ~ poly(disp, 2), data = .)
)
models
compare <- models %>% do(aov = anova(.$mod_linear, .$mod_quad))
# compare %>% summarise(p.value = aov$
`
Pr(>F)
`
)
if (require("nycflights13")) {
  # You can use it to do any arbitrary computation, like fitting a linear
  # model. Let 's explore how carrier departure delays vary over the time
carriers <- group_by(flights, carrier)
group_size(carriers)
mods <- do(carriers, mod = lm(arr_delay ~ dep_time, data = .))
mods %>% do(as.data.frame(coef(.$mod)))
mods %>% summarise(rsq = summary(mod)$r.squared)
## Not run:
# This longer example shows the progress bar in action
by_dest <- flights %>% group_by(dest) %>% filter(n() > 100)
library(mgcv)
by_dest %>% do(smooth = gam(arr_delay ~ s(dep_time) + month, data = .))
## End(Not run)
}

##dr_dplyr() detail of tbl这个格式的描述性统计
## Not run:
dr_dplyr()
## End(Not run)

##explain(x, ...)  就像是str()
if (require("dbplyr")) {
  lahman_s <- lahman_sqlite()
  batting <- tbl(lahman_s, "Batting")
  batting %>% show_query()
  batting %>% explain()
  # The batting database has indices on all ID variables:
  # SQLite automatically picks the most restrictive index
  batting %>% filter(lgID == "NL" & yearID == 2000L) %>% explain()
  # OR's will use multiple indexes
batting %>% filter(lgID == "NL" | yearID == 2000) %>% explain()
# Joins will use indexes in both tables
teams <- tbl(lahman_s, "Teams")
batting %>% left_join(teams, c("yearID", "teamID")) %>% explain()
}

##filter()-----还有all() if() at()
##ungroupy()  之中常用的运算 == >=, &|,xor(),is.na() between() near()

filter(starwars, species == "Human")
filter(starwars, mass > 1000)
# Multiple criteria
filter(starwars, hair_color == "none" & eye_color == "black")
filter(starwars, hair_color == "none" | eye_color == "black")
# Multiple arguments are equivalent to and
filter(starwars, hair_color == "none", eye_color == "black")

# While filter() accepts expressions with specific variables, the
# scoped filter verbs take an expression with the pronounand
# replicate it over all variables. This expression should be quoted
# with all_vars() or any_vars():
all_vars(is.na(.))
any_vars(is.na(.))
# You can take the intersection of the replicated expressions:
filter_all(mtcars, all_vars(. > 150))
# Or the union:
filter_all(mtcars, any_vars(. > 150))
# You can vary the selection of columns on which to apply the
# predicate. filter_at() takes a vars() specification:
filter_at(mtcars, vars(starts_with("d")), any_vars((. %% 2) == 0))
# And filter_if() selects variables with a predicate function:
filter_if(mtcars, ~ all(floor(.) == .), all_vars(. != 0))

funs(mean, "mean", mean(., na.rm = TRUE))
# Override default names
funs(m1 = mean, m2 = "mean", m3 = mean(., na.rm = TRUE))
# If you have function names in a vector, use funs_
fs <- c("min", "max")
funs_(fs)

df <- tibble(x = 1, y = 2) %>% group_by(x, y)
group_vars(df)
groups(df)

by_cyl <- mtcars %>% group_by(cyl)
# grouping doesn
'
t change how the data looks (apart from listing
# how it
'
s grouped):
  by_cyl
# It changes how it acts with the other dplyr verbs:
by_cyl %>% summarise(
  disp = mean(disp),
  hp = mean(hp)
)
by_cyl %>% filter(disp == max(disp))
# Each call to summarise() removes a layer of grouping
by_vs_am <- mtcars %>% group_by(vs, am)
by_vs <- by_vs_am %>% summarise(n = n())
by_vs
by_vs %>% summarise(n = sum(n))
# To removing grouping, use ungroup
by_vs %>%
  ungroup() %>%
  summarise(n = sum(n))
# You can group by expressions: this is just short-hand for
# a mutate/rename followed by a simple group_by
mtcars %>% group_by(vsam = vs + am)
# By default, group_by overrides existing grouping
by_cyl %>%
  group_by(vs, am) %>%
  group_vars()
# Use add = TRUE to instead append
by_cyl %>%
  group_by(vs, am, add = TRUE) %>%
  group_vars()

##group_by_all(),group_by_at(),group_by_if()
# Group a data frame by all variables:
group_by_all(mtcars)
# Group by variables selected with a predicate:
group_by_if(iris, is.factor)
# Group by variables selected by name:
group_by_at(mtcars, vars(vs, am))
# Like group_by(), the scoped variants have optional mutate
# semantics. This provide a shortcut for group_by() + mutate():
group_by_all(mtcars, as.factor)
group_by_if(iris, is.factor, as.character)

#if_else
if_else(condition, true, false, missing = NULL)
x <- c(-5:5, NA)
if_else(x < 0, NA_integer_, x)
if_else(x < 0, "negative", "positive", "missing")
# Unlike ifelse, if_else preserves types
x <- factor(sample(letters[1:5], 10, replace = TRUE))
ifelse(x %in% c("a", "b", "c"), x, factor(NA))
if_else(x %in% c("a", "b", "c"), x, factor(NA))
# Attributes are taken from the`true`vector,
inner_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)
left_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)
right_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)
full_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)
semi_join(x, y, by = NULL, copy = FALSE, ...)
anti_join(x, y, by = NULL, copy = FALSE, ...)

# "Mutating" joins add variables to the LHS
band_members %>% inner_join(band_instruments)
band_members %>% left_join(band_instruments)
band_members %>% right_join(band_instruments)
band_members %>% full_join(band_instruments)
# "Filtering" joins keep cases from the LHS
band_members %>% semi_join(band_instruments)
band_members %>% anti_join(band_instruments)
# To suppress the message, supply by
band_members %>% inner_join(band_instruments, by = "name")
# This is good practice in production code
# Use a named`by`if the join variables have different names
band_members %>% full_join(band_instruments2, by = c("name" = "artist"))
# Note that only the key from the LHS is kept

if (require("Lahman")) {
  batting_df <- tbl_df(Batting)
  person_df <- tbl_df(Master)
  uperson_df <- tbl_df(Master[!duplicated(Master$playerID), ])
  # Inner join: match batting and person data
  inner_join(batting_df, person_df)
  inner_join(batting_df, uperson_df)
  # Left join: match, but preserve batting data
  left_join(batting_df, uperson_df)
  # Anti join: find batters without person data
  anti_join(batting_df, person_df)
  # or people who didnt bat
  anti_join(person_df, batting_df)
}

##lead() lag()
lead(1:10, 1)
lead(1:10, 2)

lag(1:10, 1)
lead(1:10, 1)
x <- runif(5)
cbind(ahead = lead(x), x, behind = lag(x))
# Use order_by if data not already ordered
df <- data.frame(year = 2000:2005, value = (0:5) ^ 2)
scrambled <- df[sample(nrow(df)), ]
wrong <- mutate(scrambled, prev = lag(value))
arrange(wrong, year)
right <- mutate(scrambled, prev = lag(value, order_by = year))
arrange(right, year)

# Newly created variables are available immediately
mtcars %>% as_tibble() %>% mutate(
  cyl2 = cyl * 2,
  cyl4 = cyl2 * 2
)
# You can also use mutate() to remove variables and
# modify existing variables
mtcars %>% as_tibble() %>% mutate(
  mpg = NULL,
  disp = disp * 0.0163871 # convert to litres
)

# window functions are useful for grouped mutates
mtcars %>%
  group_by(cyl) %>%
  mutate(rank = min_rank(desc(mpg)))
# see`vignette("window-functions")`for more details
# You can drop variables by setting them to NULL
mtcars %>% mutate(cyl = NULL)
# mutate() vs transmute --------------------------
# mutate() keeps all existing variables
mtcars %>%
  mutate(displ_l = disp / 61.0237)
# transmute keeps only the variables you create
mtcars %>%
  transmute(displ_l = disp / 61.0237)
# mutate() supports quasiquotation. You can unquote quosures, which
# can refer to both contextual variables and variable names:
var <- 100
as_tibble(mtcars) %>% mutate(cyl = !! quo(cyl * var))

##n()
if (require("nycflights13")) {
  carriers <- group_by(flights, carrier)
  summarise(carriers, n())
  mutate(carriers, n = n())
  filter(carriers, n() < 100)
}

na_if(1:5, 5:1)
x <- c(1, -1, 0, 10)
100 / x
100 / na_if(x, 0)

y <- c("abc", "def", "", "ghi")
na_if(y, "")

##near()一个比较的方法,其中tol为容忍程度
near(x, y, tol = .Machine$double.eps^0.5)

##nth(),一个向量在n处的值
nth(x, n, order_by = NULL, default = default_missing(x))
first(x, order_by = NULL, default = default_missing(x))
last(x, order_by = NULL, default = default_missing(x))

x <- 1:10
y <- 10:1
first(x)
last(y)
nth(x, 1)
nth(x, 5)
nth(x, -2)
nth(x, 11)
last(x)
# Second argument provides optional ordering
last(x, y)
# These functions always return a single value
first(integer())

##n_distinct Efficiently count the number of unique values in a set of vector
n_distinct(..., na.rm = FALSE)
x <- sample(1:10, 1e5, rep = TRUE)
length(unique(x))
n_distinct(x)

order_by(order_by, call)
order_by(10:1, cumsum(1:10))
x <- 10:1
y <- 1:10
order_by(x, cumsum(y))
df <- data.frame(year = 2000:2005, value = (0:5) ^ 2)
scrambled <- df[sample(nrow(df)), ]
wrong <- mutate(scrambled, running = cumsum(value))
arrange(wrong, year)
right <- mutate(scrambled, running = order_by(year, cumsum(value)))
arrange(right, year)

##pull Pull out a single variable
mtcars %>% pull(-1)
mtcars %>% pull(1)
mtcars %>% pull(cyl)
# Also works for remote sources
if (requireNamespace("dbplyr", quietly = TRUE)) {
  df <- dbplyr::memdb_frame(x = 1:10, y = 10:1, .name = "pull-ex")
  df %>%
    mutate(z = x * y) %>%
    pull()
}

##ranking  Windowed rank functions.
row_number(x)
ntile(x, n)
min_rank(x)
dense_rank(x)
percent_rank(x)
cume_dist(x)

x <- c(5, 1, 3, 2, 2, NA)
row_number(x)
min_rank(x)
dense_rank(x)
percent_rank(x)
cume_dist(x)
ntile(x, 2)
ntile(runif(100), 10)
# row_number can be used with single table verbs without specifying x
# (for data frames and databases that support windowing)
mutate(mtcars, row_number() == 1L)
mtcars %>% filter(between(row_number(), 1, 10))

#recode  switch() 还有if_

# Recode values with named arguments
x <- sample(c("a", "b", "c"), 10, replace = TRUE)
recode(x, a = "Apple")
recode(x, a = "Apple", .default = NA_character_)
# Named arguments also work with numeric values
x <- c(1:5, NA)
recode(x,
       `
       2
       `
       = 20L,
       `
       4
       `
       = 40L)
# Note that if the replacements are not compatible with .x,
# unmatched values are replaced by NA and a warning is issued.
recode(x,
       `
       2
       `
       = "b",
       `
       4
       `
       = "d")
# If you don't name the arguments, recode() matches by position
recode(x, "a", "b", "c")
recode(x, "a", "b", "c", .default = "other")
recode(x, "a", "b", "c", .default = "other", .missing = "missing")
# Supply default with levels() for factors
x <- factor(c("a", "b", "c"))
recode(x, a = "Apple", .default = levels(x))
# Use recode_factor() to create factors with levels ordered as they
# appear in the recode call. The levels in .default and .missing
# come last.
x <- c(1:4, NA)
recode_factor(x,`1`= "z",`2`= "y",`3`= "x")
recode_factor(x,`1`= "z",`2`= "y", .default = "D")
recode_factor(x,`1`= "z",`2`= "y", .default = "D", .missing = "M")
# When the input vector is a compatible vector (character vector or
# factor), it is reused as default.
recode_factor(letters[1:3], b = "z", c = "y")
recode_factor(factor(letters[1:3]), b = "z", c = "y")

##rowwrise()
df <- expand.grid(x = 1:3, y = 3:1)
df %>% rowwise() %>% do(i = seq(.$x, .$y))
.Last.value %>% summarise(n = length(i))

#sample抽样
sample_n(tbl, size, replace = FALSE, weight = NULL, .env = NULL)
by_cyl <- mtcars %>% group_by(cyl)
# Sample fixed number per group
sample_n(mtcars, 10)
sample_n(mtcars, 50, replace = TRUE)
sample_n(mtcars, 10, weight = mpg)
sample_n(by_cyl, 3)
sample_n(by_cyl, 10, replace = TRUE)
sample_n(by_cyl, 3, weight = mpg / mean(mpg))
# Sample fixed fraction per group
# Default is to sample all data = randomly resample rows
sample_frac(mtcars)
sample_frac(mtcars, 0.1)
sample_frac(mtcars, 1.5, replace = TRUE)
sample_frac(mtcars, 0.1, weight = 1 / mpg)
sample_frac(by_cyl, 0.2)
sample_frac(by_cyl, 1, replace = TRUE)


#The verbs with scoped variants are:
  ???mutate(),transmute() and summarise(). See summarise_all().
???filter(). See filter_all().
???group_by(). See group_by_all().
???rename()andselect(). See select_all().
???arrange(). See arrange_all()

##select
#starts_with() ends_with() contain()
#matches() num_range()

iris <- as_tibble(iris) # so it prints a little nicer
select(iris, starts_with("Petal"))
select(iris, ends_with("Width"))
# Move Species variable to the front
select(iris, Species, everything())
df <- as.data.frame(matrix(runif(100), nrow = 10))
df <- tbl_df(df[c(3, 4, 7, 1, 9, 8, 5, 2, 6, 10)])
select(df, V4:V6)
select(df, num_range("V", 4:6))
# Drop variables with -
select(iris, -starts_with("Petal"))
# The .data pronoun is available:
select(mtcars, .data$cyl)
select(mtcars, .data$mpg : .data$disp)
# However it isn't available within calls since those are evaluated
# outside of the data context. This would fail if run:
# select(mtcars, identical(.data$cyl))
# Renaming -----------------------------------------
# * select() keeps only the variables you specify
select(iris, petal_length = Petal.Length)
# * rename() keeps all variables
rename(iris, petal_length = Petal.Length)

# Supply a renaming function:
select_all(mtcars, toupper)
select_all(mtcars, "toupper")
select_all(mtcars, funs(toupper(.)))
# Selection drops unselected variables:
is_whole <- function(x) all(floor(x) == x)
select_if(mtcars, is_whole, toupper)
# But renaming retains them:
rename_if(mtcars, is_whole, toupper)
# The renaming function is optional for selection:
select_if(mtcars, is_whole)


iris <- tbl_df(iris) # so it prints a little nicer
select(iris, starts_with("Petal"))
select(iris, ends_with("Width"))
select(iris, contains("etal"))
select(iris, matches(".t."))
select(iris, Petal.Length, Petal.Width)
select(iris, everything())
vars <- c("Petal.Length", "Petal.Width")
select(iris, one_of(vars))

#setops集合的情况
intersect(x, y, ...)
union(x, y, ...)
union_all(x, y, ...)
setdiff(x, y, ...)
setequal(x, y, ...)

mtcars$model <- rownames(mtcars)
first <- mtcars[1:20, ]
second <- mtcars[10:32, ]
intersect(first, second)
union(first, second)
setdiff(first, second)
setdiff(second, first)
union_all(first, second)
setequal(mtcars, mtcars[32:1, ])


slice(mtcars, 1L)
slice(mtcars, n())
slice(mtcars, 5:n())
by_cyl <- group_by(mtcars, cyl)
slice(by_cyl, 1:2)
# Equivalent code using filter that will also work with databases,
# but won
'
t be as fast for in-memory data. For many databases, you
'
ll
# need to supply an explicit variable to use to compute the row number.
filter(mtcars, row_number() == 1L)
filter(mtcars, row_number() == n())
filter(mtcars, between(row_number(), 5, n()))

#src_dbi Source for database backend

##summarise
???Center:  mean(),median()
???Spread:  sd(),IQR(),mad()
???Range:  min(),max(),quantile()
???Position:  first(),last(),nth(),
???Count:  n(),n_distinct()
???Logical:  any(),all()

# A summary applied to ungrouped tbl returns a single row
mtcars %>%
  summarise(mean = mean(disp), n = n())
# Usually, you ll want to group first
mtcars %>%
group_by(cyl) %>%
summarise(mean = mean(disp), n = n())
# Each summary call removes one grouping level (since that group
# is now just a single row)
mtcars %>%
group_by(cyl, vs) %>%
summarise(cyl_n = n()) %>%
group_vars()
# Note that with data frames, newly created summaries immediately
# overwrite existing variables
mtcars %>%
group_by(cyl) %>%
summarise(disp = mean(disp), sd = sd(disp))

# summarise() supports quasiquotation. You can unquote raw
# expressions or quosures:
var <- quo(mean(cyl))
summarise(mtcars, !! var)

# The scoped variants of summarise() and mutate() make it easy to
# apply the same transformation to multiple variables:
iris %>%
  group_by(Species) %>%
  summarise_all(mean)
# There are three variants.
# * _all affects every variable
# * _at affects variables selected with a character vector or vars()
# * _if affects variables selected with a predicate function:
starwars %>% summarise_at(vars(height:mass), mean, na.rm = TRUE)
starwars %>% summarise_at(c("height", "mass"), mean, na.rm = TRUE)
starwars %>% summarise_if(is.numeric, mean, na.rm = TRUE)
# mutate_if is particularly useful for transforming variables from
# one type to another
iris %>% as_tibble() %>% mutate_if(is.factor, as.character)
iris %>% as_tibble() %>% mutate_if(is.double, as.integer)
# ---------------------------------------------------------------------------
# If you want apply multiple transformations, use funs()
by_species <- iris %>% group_by(Species)
by_species %>% summarise_all(funs(min, max))
# Note that output variable name now includes the function name, in order to
# keep things distinct.
# You can express more complex inline transformations using .
by_species %>% mutate_all(funs(. / 2.54))
# Function names will be included if .funs has names or multiple inputs
by_species %>% mutate_all(funs(cm = . / 2.54))
by_species %>% summarise_all(funs(med = median))
by_species %>% summarise_all(funs(Q3 = quantile), probs = 0.75)
by_species %>% summarise_all(c("min", "max"))

#tally count observations by group
# tally() is short-hand for mutate()
mtcars %>% tally()
# count() is a short-hand for group_by() + tally()
mtcars %>% count(cyl)
# add_tally() is short-hand for mutate()
mtcars %>% add_tally()
# add_count() is a short-hand for group_by() + add_tally()
mtcars %>% add_count(cyl)
# count and tally are designed so that you can call
# them repeatedly, each time rolling up a level of detail
species <- starwars %>% count(species, homeworld, sort = TRUE)
species
species %>% count(species, sort = TRUE)
# add_count() is useful for groupwise filtering
# e.g.: show only species that have a single member
starwars %>%
  add_count(species) %>%
  filter(n == 1)


#tbl  类型判断
tbl(); as.tbl() is.tbl()

#tbl_cube 一个立方体
tbl_cube(dimensions, measures)

# The built in nasa dataset records meterological data (temperature,
# cloud cover, ozone etc) for a 4d spatio-temporal dataset (lat, long,
# month and year)
nasa
head(as.data.frame(nasa))
titanic <- as.tbl_cube(Titanic)
head(as.data.frame(titanic))
admit <- as.tbl_cube(UCBAdmissions)
head(as.data.frame(admit))
as.tbl_cube(esoph, dim_names = 1:3)
# Some manipulation examples with the NASA dataset -------------------------
# select() operates only on measures: it doesn't affect dimensions in any way
select(nasa, cloudhigh:cloudmid)
select(nasa, matches("temp"))
# filter() operates only on dimensions
filter(nasa, lat > 0, year == 2000)
# Each component can only refer to one dimensions, ensuring that you always
# create a rectangular subset
## Not run: filter(nasa, lat > long)
# Arrange is meaningless for tbl_cubes
by_loc <- group_by(nasa, lat, long)
summarise(by_loc, pressure = max(pressure), temp = mean(temperature))

#top_n   前几个数据集
#top_n(x, n, wt)

df <- data.frame(x = c(10, 4, 1, 6, 3, 1, 1))
df %>% top_n(2)
# Negative values select bottom from group. Note that we get more
# than 2 values here because there's a tie: top_n() either takes
# all rows with a value, or none.
df %>% top_n(-2)
if (require("Lahman")) {
  # Find 10 players with most games
  # A little nicer with %>%
  tbl_df(Batting) %>%
    group_by(playerID) %>%
    tally(G) %>%
    top_n(10)
  # Find year with most games for each player
  tbl_df(Batting) %>% group_by(playerID) %>% top_n(1, G)
}

#vars() Select variables





