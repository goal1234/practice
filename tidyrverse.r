install.packages('tidyverse')

library(tidyverse)
install.packages(c("nycflights13", "gapminder", "Lahman"))
devtools::session_info(c("tidyverse"))

library(tidyverse)

#-dataframe-#
mpg
ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
    geom_point(
    mapping = aes(x = displ, y = hwy, color = "blue")
    )

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy))

#--Facets--#
ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy)) +
    facet_wrap(~class, nrow = 2)

ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y =hwy)) +
    facet_grid(drv~cyl)

ggplot(data = mpg) +
    geom_point(mapping = aes(x = drv, y = cyl))

ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy)) +
    facet_grid(drv ~ .)

ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy)) +
    facet_grid(. ~ cyl)

ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy)) +
    facet_wrap(~ class, nrow = 2)

# left
ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy))

# right
ggplot(data = mpg) +
    geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
    geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

ggplot(data = mpg) +
    geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
    geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
    geom_smooth(
        mapping = aes(x = displ, y = hwy, color = drv),
        show.legend = FALSE
    )

ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy)) +
    geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
    geom_point() +
    geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
    geom_point(mapping = aes(color = class)) +
    geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
    geom_point(mapping = aes(color = class)) +
    geom_smooth(
        data = filter(mpg, class == "subcompact"),
        se = FALSE
)

ggplot(
    data = mpg,
    mapping = aes(x = displ, y = hwy, color = drv)
) +
    geom_point() +
    geom_smooth(se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
    geom_point() +
    geom_smooth()

ggplot() +
    geom_point(
        data = mpg,
        mapping = aes(x = displ, y = hwy)
    ) +
    geom_smooth(
        data = mpg,
        mapping = aes(x = displ, y = hwy)
    )


#--Statistical Transformations--#
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) +
  stat_count(mapping = aes(x = cut))

demo <- tribble(
    ~a, ~b,
    "bar_1", 20,
    "bar_2", 30,
    "bar_3", 40
)

ggplot(data = demo) +
  geom_bar(
    mapping = aes(x = a, y = b), stat = "identity"
)

ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, y = ..prop.., group = 1)
)

ggplot(data = diamonds) +
    stat_summary(
        mapping = aes(x = cut, y = depth),
        fun.ymin = min,
        fun.ymax = max,
        fun.y = median
)

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = ..prop..))

ggplot(data = diamonds) +
    geom_bar(
    mapping = aes(x = cut, fill = color, y = ..prop..)
)


#---Position Adjustments---#
ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, color = cut))

ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, fill = clarity))


ggplot(
    data = diamonds,
    mapping = aes(x = cut, fill = clarity)
) +
    geom_bar(alpha = 1/5, position = "identity")

ggplot(
    data = diamonds,
    mapping = aes(x = cut, color = clarity)
) +
    geom_bar(fill = NA, position = "identity")

ggplot(data = diamonds) +
    geom_bar(
        mapping = aes(x = cut, fill = clarity),
        position = "fill"
)

ggplot(data = diamonds) +
    geom_bar(
        mapping = aes(x = cut, fill = clarity),
        position = "dodge"
)

ggplot(data = mpg) +
    geom_point(
      mapping = aes(x = displ, y = hwy),
      position = "jitter"
)

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point()

#---Coordinate Systems---#
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
    geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
    geom_boxplot() +
    coord_flip()

nz <- map_data("nz")
ggplot(nz, aes(long, lat, group = group)) +
    geom_polygon(fill = "white", color = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()

bar <- ggplot(data = diamonds) +
    geom_bar(
        mapping = aes(x = cut, fill = cut),
        show.legend = FALSE,
        width = 1
) +
theme(aspect.ratio = 1) +
labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
    geom_point() +
    geom_abline() +
    coord_fixed()

if(FALSE){
    ggplot(data = <DATA>) +
        <GEOM_FUNCTION>(
            mapping = aes(<MAPPINGS>),
            stat = <STAT>,
            position = <POSITION>
    ) +
    <COORDINATE_FUNCTION> +
    <FACET_FUNCTION>
}

flights
library(dplyr)

#---最常见的---#
#filter()
#arrange()
#select()
#mutate()
#summarize()

filter(flights, month ==1, day == 1)
jan1 <- filter(flights, month == 1, day == 1)
(dec25 <- filter(flights, month == 12, day == 25))

filter(flights, month = 1)
sqrt(2) ^ 2 == 2
near(sqrt(2) ^ 2, 2)
near(1 / 49 * 49, 1)

filter(flights, month == 11 | month == 12)
nov_dec <- filter(flights, month %in% c(11, 12))

filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))

df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))

# Select columns by name
select(flights, year, month, day)
# Select all columns between year and day (inclusive)
select(flights, year:day)
# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))

rename(flights, tail_num = tailnum)
select(flights, time_hour, air_time, everything())
select(flights, contains("TIME"))

#--Add New Variables with mutate()--#
flights_sml <- select(flights,
    year:day,
    ends_with("delay"),
    distance,
    air_time
)

mutate(flights_sml,
    gain = arr_delay - dep_delay,
    speed = distance / air_time * 60
)

mutate(flights_sml,
    gain = arr_delay - dep_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours
)

#-only keep variables-#
transmute(flights,
    gain = arr_delay - dep_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours
)

#-Useful Creation Functions-#
transmute(flights,
    dep_time,
    hour = dep_time %/% 100,
    minute = dep_time %% 100
)

log(), log2(), log10()
(x <- 1:10)
#> [1] 1 2 3 4 5 6 7 8 9 10
lag(x)
#> [1] NA 1 2 3 4 5 6 7 8 9
lead(x)
#> [1] 2 3 4 5 6 7 8 9 10 NA

x
cumsum(x)
cummean(x)

#-Ranking
y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
#> [1] 1 2 2 NA 4 5
min_rank(desc(y))
#> [1] 5 3 3 NA 2 1

row_number(y)
#> [1] 1 2 3 NA 4 5
dense_rank(y)
#> [1] 1 2 2 NA 3 4
percent_rank(y)
#> [1] 0.00 0.25 0.25 NA 0.75 1.00
cume_dist(y)
#> [1] 0.2 0.6 0.6 NA 0.8 1.0


#-Grouped Summaries with summarize()-#
summarize(flights, delay = mean(dep_delay, na.rm = TRUE))
by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))

#-Combining Multiple Operations with the Pipe-#
by_dest <- group_by(flights, dest)
delay <- summarize(by_dest,
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")

# It looks like delays increase with distance up to ~750 miles
# and then decrease. Maybe as flights get longer there's more
# ability to make up delays in the air?
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
    geom_point(aes(size = count), alpha = 1/3) +
    geom_smooth(se = FALSE)
#> `geom_smooth()` using method = 'loess'


delays <- flights %>%
    group_by(dest) %>%
    summarize(
      count = n(),
      dist = mean(distance, na.rm = TRUE),
      delay = mean(arr_delay, na.rm = TRUE)
) %>%
filter(count > 20, dest != "HNL")

#-Missing Values-#
flights %>%
    group_by(year, month, day) %>%
    summarize(mean = mean(dep_delay))

flights %>%
    group_by(year, month, day) %>%
    summarize(mean = mean(dep_delay, na.rm = TRUE))

not_cancelled <- flights %>%
    filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
    group_by(year, month, day) %>%
    summarize(mean = mean(dep_delay))

#-Counts-#
delays <- not_cancelled %>%
    group_by(tailnum) %>%
    summarize(
        delay = mean(arr_delay)
)
ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>%
    group_by(tailnum) %>%
    summarize(
        delay = mean(arr_delay, na.rm = TRUE),
        n = n()
)

ggplot(data = delays, mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

delays %>% 
  filter(n > 25) %>%
    ggplot(mapping = aes(x = n, y = delay)) +
      geom_point(alpha = 1/10)


#---Counts---#
delays <- not_cancelled %>%
    group_by(tailnum) %>%
    summarize(
      delay = mean(arr_delay)
)
ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
)

ggplot(data = delays, mapping = aes(x = n, y = delay)) +
    geom_point(alpha = 1/10)

delays %>%
filter(n > 25) %>%
ggplot(mapping = aes(x = n, y = delay)) +
geom_point(alpha = 1/10)


#Convert to a tibble so it prints nicely
batting <- as_tibble(Lahman::Batting)
batters <- batting %>%
            group_by(playerID) %>%
            summarize(
                ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
                ab = sum(AB, na.rm = TRUE)
)

batters %>%
  filter(ab > 100) %>%
    ggplot(mapping = aes(x = ab, y = ba)) +

geom_point() +
  geom_smooth(se = FALSE)

#> `geom_smooth()` using method = 'gam'
batters %>%
  arrange(desc(ba))

#-Useful Summary Functions-#
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
    # average delay:
    avg_delay1 = mean(arr_delay),
    # average positive delay:
    avg_delay2 = mean(arr_delay[arr_delay > 0])
)

#-Measures of spread sd(x), IQR(x), mad(x)
# Why is distance to some destinations more variable
# than to others?
not_cancelled %>%
  group_by(dest) %>%
  summarize(distance_sd = sd(distance)) %>%
  arrange(desc(distance_sd))

# When do the first and last flights leave each day?
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
    first = min(dep_time),
    last = max(dep_time)
)

not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
    first_dep = first(dep_time),
    last_dep = last(dep_time)
)

not_cancelled %>%
  group_by(year, month, day) %>%
  mutate(r = min_rank(desc(dep_time))) %>%
  filter(r %in% range(r))

# Which destinations have the most carriers?
not_cancelled %>%
  group_by(dest) %>%
  summarize(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))


not_cancelled %>%
  count(dest)

not_cancelled %>% 
  count(tailnum, wt = distance)

not_cancelled %>%
  count(tailnum, wt = distance)

# How many flights left before 5am? (these usually
# indicate delayed flights from the previous day)
not_cancelled %>%
    group_by(year, month, day) %>%
    summarize(n_early = sum(dep_time < 500))

not_cancelled %>%
    group_by(year, month, day) %>%
    summarize(hour_perc = mean(arr_delay > 60))

#-Grouping by Multiple Variables-#
daily <- group_by(flights, year, month, day)
(per_day <- summarize(daily, flights = n()))

(per_month <- summarize(per_day, flights = sum(flights)))
(per_year <- summarize(per_month, flights = sum(flights)))

#-Ungrouping-#
daily %>%
  ungroup() %>% # no longer grouped by date
  summarize(flights = n()) # all flights

flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)


popular_dests <- flights %>%
  group_by(dest) %>% 
    filter(n() > 365)

popular_dests %>%
　filter(arr_delay > 0) %>% 
  mulate(prop_delay = arr_delay/sum(arr_delay)) %>%
  select(year:day, dest, arr_delay, prop_delay)

#################################################################################################
#################################################################################################
#------------------------------------Workflow: Scripts-------------------------------------#


library(dplyr)
library(nycflights13)

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(mean = mean(dep_delay))

###################################
##################
#Exploratory Data Analysis
library(tidyverse)

#Visualizing Distributions
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

diamonds %>%
  count(cut)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

diamonds %>%
  count(cut_width(carat, 0.5))

smaller <- diamonds %>%
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.1)

#-Typical Values-#
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(data = faithful, mapping = aes(x = eruptions)) +
  geom_histogram(binwidth = 0.25)


#-Unsual Values-#
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0,50))


unsual <- diamonds %>% 
  filter(y < 3| y > 20) %>%
  arrange(y)

diamonds2 <- diamonds %>%
  filter(between(y, 3, 20))

diamonds2 <- diamonds %>%
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point()

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)

nycflights13::flights %>%
    mutate(
        cancelled = is.na(dep_time),
        sched_hour = sched_dep_time %/% 100,
        sched_min = sched_dep_time %% 100,
        sched_dep_time = sched_hour + sched_min / 60
    ) %>%
    ggplot(mapping = aes(sched_dep_time)) +
      geom_freqpoly(
        mapping = aes(color = cancelled),
        binwidth = 1/4
)

#---Covariation---#
#-A Categorical and Continuous Variable
ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

ggplot(diamonds) +
  geom_bar(mapping = aes(x = cut))

ggplot(
  data = diamonds,
    mapping = aes(x = price, y = ..density..)
) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(
      mapping = aes(
          x = reorder(class, hwy, FUN = median)
          y = hwy
      )
  )

ggplot(data = mpg) +
geom_boxplot(
mapping = aes(
x = reorder(class, hwy, FUN = median),
y = hwy
)
) +
coord_flip()

#--Two Categorical Variables--#
ggplot(data = diamonds)+
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>%
  count(color, cut)

diamonds %>%
  count(color, cut) %>% 
  ggplot(mapping = aes(x = color, y = cut)) +
    geom_title(mapping = aes(fill = n))

#-Two Continuous Variables-#
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

ggplot(data = diamonds) +
  geom_point(
      mapping = aes(x = caret, y = price)
      alpha = 1/100
  )

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

# install.packages("hexbin")
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))

#> Loading required package: methods

ggplot(data = smaller, mapping = aes(x = carat, y =price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
    coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

#--Patterns and Models--#
ggplot(data = faithful) +
  geom_point(mapping = aes(x = eruptions, y = waiting))

library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>%
  add_residuals(mod) %>%
  mutate(resid = exp(resid))

ggplot(data = diamonds2) +
  geom_point(mapping = aes(x = carat, y = resid))

ggplot(data = diamonds2) +
  geom_boxplot(mapping = aes(x = cut, y = resid))

#--ggplot2 Calls--#
ggplot(data = faithful, mapping = aes(x = eruptions)) +
  geom_freqpoly(binwidth = 0.25)

ggplot(faithful, aes(eruptions)) +
  geom_freqpoly(binwidth = 0.25)

diamonds %>%
  count(cut, clarity) %>%
  ggplot(aes(clarity, cut, fill = n)) +
    geom_tile()


##############################################################################################
#############################################################################################
#######--------------------------Workflow: Projects-----------------------------------------#

getwd()
setwd("/path/to/my/CoolProject")

#####################################################################################
#-----------------               Tibbles with tibble                      ----------#
#####################################################################################
library(tidyverse)
as.tibble(iris)

tibble(
  x = 1:5,
  y = 1,
  z = x ^ 2 + y
)

tb <- tibble(
  `:)` = "smile",
  ` ` = "space",
  `2000` = "number"
)
tb

tribble(
    ~x, ~y, ~z,
    #--|--|----
    "a", 2, 3.6,
    "b", 1, 8.5
)


#-Printing-#
tibble(
    a = lubridate::now() + runif(1e3) * 86400,
    b = lubridate::today() + runif(1e3) * 30,
    c = 1:1e3,
    d = runif(1e3),
    e = sample(letters, 1e3, replace = TRUE)
)

nycflights13::flights %>%
    print(n = 10, width = Inf)

nycflights13::flights %>%
    View()

#--Subsetting--#
df <- tibble(
    x = runif(5),
    y = rnorm(5)
)

# Extract by name
df$x
#> [1] 0.434 0.395 0.548 0.762 0.254
df[["x"]]
#> [1] 0.434 0.395 0.548 0.762 0.254
# Extract by position
df[[1]]
#> [1] 0.434 0.395 0.548 0.762 0.254

df %>% .$x
df %>% .[['x']]

#-Interacting with Older Code-#
class(as.data.frame(tb))

df <- data.frame(abc = 1, xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]

annoying <- tibble(
    `1` = 1:10,
    `2` = `1` * 2 + rnorm(length(`1`))
)

##################################################################
#--------------------Data Import with readr----------------------#

library(tidyverse)
heights <- read_csv("data/heights.csv")
#> Parsed with column specification:
#> cols(
#> earn = col_double(),
#> height = col_double(),
#> sex = col_character(),
#> ed = col_integer(),
#> age = col_integer(),
#> race = col_character()
#> )

read_csv("a,b,c
    1,2,3
    4,5,6")

read_csv("The first line of metadata
    The second line of metadata
    x,y,z
    1,2,3", skip = 2)

read_csv("# A comment I want to skip
    x,y,z
    1,2,3", comment = "#")
read_csv("1,2,3\n4,5,6", col_names = FALSE)
read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))
read_csv("a,b,c\n1,2,.", na = ".")

read_csv("a,b\n1,2,3\n4,5,6")
read_csv("a,b,c\n1,2\n1,2,3,4")
read_csv("a,b\n\"1")
read_csv("a,b\n1,2\na,b")
read_csv("a;b\n1;3")


#-Parsing a Vector-#
str(parse_logical(c("TRUE", "FALSE", "NA")))
#> logi [1:3] TRUE FALSE NA
str(parse_integer(c("1", "2", "3")))
#> int [1:3] 1 2 3
str(parse_date(c("2010-01-01", "1979-10-14")))
parse_integer(c("1", "231", ".", "456"), na = ".")
x <- parse_integer(c("123", "345", "abc", "123.45"))
problems(x)

parse_double("1.23")
#> [1] 1.23
parse_double("1,23", locale = locale(decimal_mark = ","))

parse_number("$100")
#> [1] 100
parse_number("20%")
#> [1] 20
parse_number("It cost $123.45")
# Used in America
parse_number("$123,456,789")
#> [1] 1.23e+08
# Used in many parts of Europe
parse_number(
"123.456.789",
locale = locale(grouping_mark = ".")
)
#> [1] 1.23e+08
# Used in Switzerland
parse_number(
"123'456'789",
locale = locale(grouping_mark = "'")
)
#> [1] 1.23e+08

charToRaw("Hadley")
x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"

parse_character(x1, locale = locale(encoding = "Latin1"))
#> [1] "El Niño was particularly bad this year"
parse_character(x2, locale = locale(encoding = "Shift-JIS"))
#> [1] "こんにちは"

guess_encoding(charToRaw(x1))
#> encoding confidence
#> 1 ISO-8859-1 0.46
#> 2 ISO-8859-9 0.23
guess_encoding(charToRaw(x2))

fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)

#--dates, times and date-times--#
parse_datetime("2010-10-01T2010")

# If time is omitted, it will be set to midnight
parse_datetime("20101010")

parse_date("2010-10-01")
#> [1] "2010-10-01"
library(hms)
parse_time("01:10 am")
#> 01:10:00
parse_time("20:10:01")
#> 20:10:01

parse_date("01/02/15", "%m/%d/%y")
#> [1] "2015-01-02"
parse_date("01/02/15", "%d/%m/%y")
#> [1] "2015-02-01"
parse_date("01/02/15", "%y/%m/%d")
parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))


d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
t1 <- "1705"
t2 <- "11:15:10.12 PM"

#-Strategy-#
guess_parser("2010-10-01")
#> [1] "date"
guess_parser("15:01")
#> [1] "time"
guess_parser(c("TRUE", "FALSE"))
#> [1] "logical"
guess_parser(c("1", "5", "9"))
#> [1] "integer"
guess_parser(c("12,352,561"))
#> [1] "number"

str(parse_guess("2010-10-10"))
#> Date[1:1], format: "2010-10-10"

challenge <- read_csv(readr_example("challenge.csv"))
#> Parsed with column specification:
problems(challenge)
#> # A tibble: 1,000 × 4

#---read.table---#
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_integer(),
    y = col_character()
  )
)


challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_character()
  )
)
tail(challenge)

challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)

tail(challenge)

#--Other Strategies--#
challenge2 <- read_csv(
                readr_example("challenge.csv"),
                guess_max = 1001
              )

challenge2

challenge2 <- read_csv(readr_example("challenge.csv"),
    col_types = cols(.default = col_character())
)

df <- tribble(
    ~x, ~y,
    "1", "1.21",
    "2", "2.32",
    "3", "4.56"
)
df

type_convert(df)
#> Parsed with column specification:
write_csv(challenge, "challenge.csv")
challenge
write_csv(challenge, "challenge-2.csv")
read_csv("challenge-2.csv")

write_rds(challenge, "challenge.rds")
read_rds("challenge.rds")

library(feather)
write_feather(challenge, "challenge.feather")
read_feather("challenge.feather")

#--Tidy Data with tidyr--#

library(tidyverse)
table1
table2
table3

table4a # cases

# Compute rate per 10,000
table1 %>%
  mutate(rate = cases / population * 10000)

# Compute cases per year
table1 %>%
  count(year, wt = cases)

# Visualize changes over time
library(ggplot2)
ggplot(table1, aes(year, cases)) +
    geom_line(aes(group = country), color = "grey50") +
    geom_point(aes(color = country))

#---Spreading and Gathering---#

#-Gathering
table4a
table4a %>% 
  gather('1999','2000', key = 'year',value = 'cases')

table4b %>%
    gather(`1999`, `2000`, key = "year", value = "population")

tidy4a <- table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")

tidy4b <- table4b %>%
  gather(`1999`, `2000`, key = "year", value = "population")

left_join(tidy4a, tidy4b)

#Spreading
table2

spread(table2, key = type, value = count)

stocks <- tibble(
    year = c(2015, 2015, 2016, 2016),
    half = c( 1, 2, 1, 2),
    return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>%
    spread(year, return) %>%
    gather("year", "return", `2015`:`2016`)

table4a %>%
    gather(1999, 2000, key = "year", value = "cases")
    #> Error in eval(expr, envir, enclos):
    #> Position must be between 0 and n

people <- tribble(
    ~name, ~key, ~value,
    #-----------------|--------|------
    "Phillip Woods", "age", 45,
    "Phillip Woods", "height", 186,
    "Phillip Woods", "age", 50,
    "Jessica Cordero", "age", 37,
    "Jessica Cordero", "height", 156
)

preg <- tribble(
    ~pregnant, ~male, ~female,
    "yes", NA, 10,
    "no", 20, 12
)

#-Separating and Pull
table3
table3 %>%
    separate(rate, into = c("cases", "population"))

table3 %>%
    separate(rate, into = c("cases", "population"), sep = "/")

table3 %>%
    separate(
        rate,
        into = c("cases", "population"),
        convert = TRUE
)
#> # A tibble: 6 × 4

table3 %>%
    separate(year, into = c("century", "year"), sep = 2)

table5 %>%
    unite(new, century, year)

table5 %>% 
    unite(new, century, year, sep = '')

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
    separate(x, c('one', 'two','three'))

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
    separate(x, c("one", "two", "three"))

stocks <- tibble(
    year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
    qtr = c( 1, 2, 3, 4, 2, 3, 4),
    return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
)

stocks %>%
    spread(year, return)

stocks %>%
    spread(year, return) %>%
    gather(year, return, `2015`:`2016`, na.rm = TRUE)

stocks %>%
    complete(year, qtr)

treatment <- tribble(
    ~ person, ~ treatment, ~response,
    "Derrick Whitmore", 1, 7,
    NA, 2, 10,
    NA, 3, 9,
    "Katherine Burke", 1, 4
)

treatment %>%
  fill(person)

who

who1 <- who %>%
    gather(
        new_sp_m014:newrel_f65, key = "key",
        value = "cases",
        na.rm = TRUE
)
who1

who1 %>%
    count(key)

who2 <- who1 %>%
    mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2

who3 <- who2 %>%
    separate(key, c("new", "type", "sexage"), sep = "_")
who3

who3 %>% 
  count(new)

who4 <- who3 %>% 
          select(-new, -iso2, iso3)

who5 <- who4 %>%
    separate(sexage, c("sex", "age"), sep = 1)
who5

who %>%
    gather(code, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>%
    mutate(
        code = stringr::str_replace(code, "newrel", "new_rel")
    ) %>%
    separate(code, c("new", "var", "sexage")) %>%
    select(-new, -iso2, -iso3) %>%
    separate(sexage, c("sex", "age"), sep = 1)

#---Relational Data with dplyr---#
library(tidyverse)
library(nycflights13)

airlines
airports
planes
weather

planes %>%
    count(tailnum) %>%
    filter(n > 1)

weather %>%
    count(year, month, day, hour, origin) %>%
    filter(n > 1)

flights %>%
    count(year, month, day, flight) %>%
    filter(n > 1)

flights %>%
    count(year, month, day, tailnum) %>%
    filter(n > 1)

#-Mutating Joins-#
flights2 <- flights %>%
    select(year:day, hour, origin, dest, tailnum, carrier)
flights2

flights2 %>%
    select(-origin, -dest) %>%
    left_join(airlines, by = "carrier")

#-Understanding Joins-#
x <- tribble(
    ~key, ~val_x,
    1, "x1",
    2, "x2",
    3, "x3"
)

y <- tribble(
    ~key, ~val_y,
    1, "y1",
    2, "y2",
    4, "y3"
)

x %>%
    inner_join(y, by = "key")

#Duplicate Keys
x <- tribble(
    ~key, ~val_x,
    1, "x1",
    2, "x2",
    2, "x3",
    1, "x4"
)
y <- tribble(
    ~key, ~val_y,
    1, "y1",
    2, "y2"
)
left_join(x, y, by = "key")


x <- tribble(
    ~key, ~val_x,
    1, "x1",
    2, "x2",
    2, "x3",
    3, "x4"
)
y <- tribble(
    ~key, ~val_y,
    1, "y1",
    2, "y2",
    2, "y3",
    3, "y4"
)
left_join(x, y, by = "key")

flights2 %>%
    left_join(weather)

flight2 %>% 
    left_join(planes, by = 'tailnum')

flights2 %>%
    left_join(airports, c("dest" = "faa"))

flights2 %>%
    left_join(airports, c("origin" = "faa"))

airports %>%
    semi_join(flights, c("faa" = "dest")) %>%
    ggplot(aes(lon, lat)) +
      borders("state") +
      geom_point() +
      coord_quickmap()

#dplyr merge
#inner_join(x, y) merge(x, y)
#left_join(x, y) merge(x, y, all.x = TRUE)
#right_join(x, y) merge(x, y, all.y = TRUE),
#full_join(x, y) merge(x, y, all.x = TRUE, all.y = TRUE)


#dplyr SQL
#inner_join(x, y, by = "z") SELECT * FROM x INNER JOIN y USING
#(z)
#left_join(x, y, by = "z") SELECT * FROM x LEFT OUTER JOIN y
#USING (z)
#right_join(x, y, by = "z") SELECT * FROM x RIGHT OUTER JOIN y
#USING (z)
#full_join(x, y, by = "z") SELECT * FROM x FULL OUTER JOIN y
#USING (z)


top_dest <- flights %>%
    count(dest, sort = TRUE) %>%
    head(10)
top_dest

flights %>%
    filter(dest %in% top_dest$dest)

fligths %>% 
    semi_join(top_dest)

flights %>%
    anti_join(planes, by = "tailnum") %>%
    count(tailnum, sort = TRUE)

airports %>% count(alt, lon) %>% filter(n > 1)

intersect(x, y)
#Return only observations in both x and y.
union(x, y)
#Return unique observations in x and y.
setdiff(x, y)
#Return observations in x, but not in y.

df1 <- tribble(
    ~x, ~y,
    1, 1,
    2, 1
)
df2 <- tribble(
    ~x, ~y,
    1, 1,
    1, 2
)

intersect(df1, df2)
union(df1, df2)
setdiff(df1, df2)
setdiff(df2, df1)


#---Strings with stringr---#
library(tidyverse)
library(stringr)

#-String Basics-#
string1 <- "This is a string"
string2 <- 'To put a "quote" inside a string, use single quotes'
double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"

x <- c("\"", "\\")
writeLines(x)
c("one", "two", "three")
str_length(c("a", "R for data science", NA))

#-Combining Strings-#
str_c('x','y')
str_c("x", "y", "z")

str_c("x", "y", sep = ", ")
x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")

str_c("prefix-", c("a", "b", "c"), "-suffix")
name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
    "Good ", time_of_day, " ", name,
    if (birthday) " and HAPPY BIRTHDAY",
    "."
)

str_c(c("x", "y", "z"), collapse = ", ")

x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)

# negative numbers count backwards from end
str_sub(x, -3, -1)

str_sub("a", 1, 5)
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))

#-Locales
str_to_upper(c("i", "ı"))
#> [1] "I" "I"
str_to_upper(c("i", "ı"), locale = "tr")
#> [1] "İ" "I"

x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en") # English
#> [1] "apple" "banana" "eggplant"
str_sort(x, locale = "haw") # Hawaiian

x <- c("apple", "banana", "pear")
str_view(x, "an")
str_view(x, ".a.")

dot <- "\\."
# But the expression itself only contains one:
writeLines(dot)
#> \.
# And this tells R to look for an explicit .
str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- "a\\b"
writeLines(x)
str_view(x, "\\\\")

x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")

x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")

str_view(x, "^apple$")

str_view(c("grey", "gray"), "gr(e|a)y")
str_view(x, "CC+")
str_view(x, 'C[LX]+')
str_view(x, "C{2}")
str_view(x, "C{2,3}")

str_view(x, 'C{2,3}?')
str_view(fruit, "(..)\\1", match = TRUE)


#--Detect Matches--#
x <- c("apple", "banana", "pear")
str_detect(x, "e")

sum(str_detect(words, "^t"))
#> [1] 65
# What proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))

# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")
# Find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
identical(no_vowels_1, no_vowels_2)
#> [1] TRUE

words[str_detect(words, "x$")]
#> [1] "box" "sex" "six" "tax"
str_subset(words, "x$")
#> [1] "box" "sex" "six" "tax"

df <- tibble(
    word = words,
    i = seq_along(word)
)
df %>%
filter(str_detect(words, "x$"))

x <- c("apple", "banana", "pear")
str_count(x, "a")
#> [1] 1 3 1

# On average, how many vowels per word?
mean(str_count(words, "[aeiou]"))
#> [1] 1.99

df %>%
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
)
#> # A tibble: 980 × 4

str_count("abababa", "aba")
#> [1] 2
str_view_all("abababa", "aba")
length(sentences)
#> [1] 720
head(sentences)
colors <- c(
"red", "orange", "yellow", "green", "blue", "purple"
)
color_match <- str_c(colors, collapse = "|")
color_match

has_color <- str_subset(sentences, color_match)
matches <- str_extract(has_color, color_match)
head(matches)

more <- sentences[str_count(sentences, color_match) > 1]
str_view_all(more, color_match)
str_extract(more, color_match)

str_extract_all(more, color_match)
str_extract_all(more, color_match, simplify = TRUE)
x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = TRUE)

noun <- "(a|the) ([^ ]+)"

has_noun <- sentences %>%
    str_subset(noun) %>%
    head(10)

has_noun %>%
    str_extract(noun)

has_noun %>%
    str_match(noun)

tibble(sentence = sentences) %>%
    tidyr::extract(
        sentence, c("article", "noun"), "(a|the) ([^ ]+)",
        remove = FALSE
)

#-Replacing Matches-#
x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
#> [1] "-pple" "p-ar" "b-nana"
str_replace_all(x, "[aeiou]", "-")
x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))

sentences %>%
    str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>%
    head(5)

sentences %>%
    head(5) %>%
    str_split(" ")

"a|b|c|d" %>%
    str_split("\\|") %>%
    .[[1]]

sentences %>%
    head(5) %>%
    str_split(" ", simplify = TRUE)

fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n = 2, simplify = TRUE)

x <- "This is a sentence. This is another sentence."
str_view_all(x, boundary("word"))

str_split(x, " ")[[1]]
str_split(x, boundary("word"))[[1]]

#=Other Types of Pattern=#
# The regular call:
str_view(fruit, "nana")
# Is shorthand for
str_view(fruit, regex("nana"))

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas, regex("banana", ignore_case = TRUE))
x <- "Line 1\nLine 2\nLine 3"
str_extract_all(x, "^Line")[[1]]
#> [1] "Line"
str_extract_all(x, regex("^Line", multiline = TRUE))[[1]]

phone <- regex("
    \\(? # optional opening parens
    (\\d{3}) # area code
    [)- ]? # optional closing parens, dash, or space
    (\\d{3}) # another three numbers
    [ -]? # optional space or dash
    (\\d{3}) # three more numbers
    ", comments = TRUE)
str_match("514-791-8141", phone)

microbenchmark::microbenchmark(
    fixed = str_detect(sentences, fixed("the")),
    regex = str_detect(sentences, "the"),
    times = 20
)

a1 <- "\u00e1"
a2 <- "a\u0301"
c(a1, a2)

a1 == a2
str_detect(a1, fixed(a2))
#> [1] FALSE
str_detect(a1, coll(a2))

# when doing case-insensitive matches:
i <- c("I", "İ", "i", "ı")
i
#> [1] "I" "İ" "i" "ı"
str_subset(i, coll("i", ignore_case = TRUE))
#> [1] "I" "i"
str_subset(
i,
coll("i", ignore_case = TRUE, locale = "tr")
)

stringi::stri_locale_info()
x <- "This is a sentence."
str_view_all(x, boundary("word"))
str_extract_all(x, boundary("word"))
apropos("replace")
head(dir(pattern = "\\.Rmd$"))


################################################################################
###############################################################################
#--------Factors with forcats

library(tidyverse)
library(forcats)
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")
sort(x1)

month_levels <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

y1 <- factor(x1, levels = month_levels)
y1
#> [1] Dec Apr Jan Mar
#> Levels: Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
sort(y1)
y2 <- factor(x2, levels = month_levels)
y2
y2 <- parse_factor(x2, levels = month_levels)
factor(x1)
f1 <- factor(x1, levels = unique(x1))
f1
f2 <- x1 %>% factor() %>% fct_inorder()
f2

levels(f2)
#-General Social Survey-#
gss_cat
gss_cat %>%
    count(race)
#> # A tibble: 3 × 2

ggplot(gss_cat, aes(race)) +
    geom_bar()


ggplot(gss_cat, aes(race)) +
    geom_bar() +
    scale_x_discrete(drop = FALSE)

#---Modifying Factor Order---#
relig <- gss_cat %>%
    group_by(relig) %>%
    summarize(
        age = mean(age, na.rm = TRUE),
        tvhours = mean(tvhours, na.rm = TRUE),
        n = n()
)
ggplot(relig, aes(tvhours, relig)) + geom_point()

ggplot(relig, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()


relig %>%
    mutate(relig = fct_reorder(relig, tvhours)) %>%
    ggplot(aes(tvhours, relig)) +
        geom_point()

rincome <- gss_cat %>%
    group_by(rincome) %>%
    summarize(
        age = mean(age, na.rm = TRUE),
        tvhours = mean(tvhours, na.rm = TRUE),
        n = n()
)
ggplot(
    rincome,
    aes(age, fct_reorder(rincome, age))
) + geom_point()

ggplot(
    rincome,
    aes(age, fct_relevel(rincome, "Not applicable"))
) +
    geom_point()

by_age <- gss_cat %>%
filter(!is.na(age)) %>%
    group_by(age, marital) %>%
    count() %>%
    mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, color = marital)) +
  geom_line(na.rm = TRUE)
ggplot(
    by_age,
    aes(age, prop, color = fct_reorder2(marital, age, prop))
) +
geom_line() +
    labs(color = "marital")

gss_cat %>%
    mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
    ggplot(aes(marital)) +
        geom_bar()

#--Modifying Factor Levels--#
gss_cat %>% count(partyid)

gss_cat %>% 
  mutate(partyid = fct_recode(partyid,
    "Republican, strong" = "Strong republican",
    "Republican, weak" = "Not str republican",
    "Independent, near rep" = "Ind,near rep",
    "Independent, near dem" = "Ind,near dem",
    "Democrat, weak" = "Not str democrat",
    "Democrat, strong" = "Strong democrat"
    )) %>%
    count(partyid)

gss_cat %>%
    mutate(partyid = fct_collapse(partyid,
        other = c("No answer", "Don't know", "Other party"),
        rep = c("Strong republican", "Not str republican"),
        ind = c("Ind,near rep", "Independent", "Ind,near dem"),
        dem = c("Not str democrat", "Strong democrat")
)) %>%
count(partyid)

gss_cat %>%
    mutate(relig = fct_lump(relig)) %>%
    count(relig)

gss_cat %>%
    mutate(relig = fct_lump(relig, n = 10)) %>%
    count(relig, sort = TRUE) %>%
    print(n = Inf)
#> # A tibble: 10




#######################################################################
######################################################################
#---Dates and Times with lubridate---#
library(tidyverse)
library(lubridate)
library(nycflights13)

#-Creating Date/Times
today()
now()

#-From Strings
ymd("2017-01-31")
#> [1] "2017-01-31"
mdy("January 31st, 2017")
#> [1] "2017-01-31"
dmy("31-Jan-2017")
ymd(20170131)
ymd_hms("2017-01-31 20:11:59")
#> [1] "2017-01-31 20:11:59 UTC"
mdy_hm("01/31/2017 08:01")
#> [1] "2017-01-31 08:01:00 UTC"

ymd(20170131, tz = "UTC")


#---From Individual Components---#
flights %>%
    select(year, month, day, hour, minute)

flights %>%
    select(year, month, day, hour, minute) %>%
    mutate(
        departure = make_datetime(year, month, day, hour, minute)
)

make_datetime_100 <- function(year, month, day, time) {
    make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>%
    filter(!is.na(dep_time), !is.na(arr_time)) %>%
    mutate(
        dep_time = make_datetime_100(year, month, day, dep_time),
        arr_time = make_datetime_100(year, month, day, arr_time),
        sched_dep_time = make_datetime_100(
        year, month, day, sched_dep_time
    ),
        sched_arr_time = make_datetime_100(
        year, month, day, sched_arr_time
    )
) %>%
    select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt %>%
    ggplot(aes(dep_time)) +
    geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

flights_dt %>%
    filter(dep_time < ymd(20130102)) %>%
    ggplot(aes(dep_time)) +
    geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

#-From Other Types-#
as_datetime(today())
#> [1] "2016-10-10 UTC"
as_date(now())
#> [1] "2016-10-10"

as_datetime(60 * 60 * 10)
#> [1] "1970-01-01 10:00:00 UTC"
as_date(365 * 10 + 2)
#> [1] "1980-01-01"

ymd(c("2010-10-10", "bananas"))
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014

#-Getting Components-#
datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime)
#> [1] 2016
month(datetime)
#> [1] 7
mday(datetime)
#> [1] 8
yday(datetime)
#> [1] 190
wday(datetime)
#> [1] 6
month(datetime, label = TRUE)
#> [1] Jul
#> 12 Levels: Jan < Feb < Mar < Apr < May < Jun < ... < Dec
wday(datetime, label = TRUE, abbr = FALSE)
#> [1] Friday
#> 7 Levels: Sunday < Monday < Tuesday < ... < Saturday

flights_dt %>%
    mutate(wday = wday(dep_time, label = TRUE)) %>%
    ggplot(aes(x = wday)) +
        geom_bar()  

flights_dt %>%
    mutate(minute = minute(dep_time)) %>%
    group_by(minute) %>%
        summarize(
        avg_delay = mean(arr_delay, na.rm = TRUE),
         n = n()) %>%
            ggplot(aes(minute, avg_delay)) +
            geom_line()

sched_dep <- flights_dt %>%
mutate(minute = minute(sched_dep_time)) %>%
group_by(minute) %>%
summarize(
avg_delay = mean(arr_delay, na.rm = TRUE),
n = n())

ggplot(sched_dep, aes(minute, avg_delay)) +
geom_line()


ggplot(sched_dep, aes(minute, n)) +
  geom_line()

#--Rounding--#
flights_dt %>%
    count(week = floor_date(dep_time, "week")) %>%
    ggplot(aes(week, n)) +
        geom_line()

#-Setting Components-#
(datetime <- ymd_hms("2016-07-08 12:34:56"))
#> [1] "2016-07-08 12:34:56 UTC"
year(datetime) <- 2020
datetime
#> [1] "2020-07-08 12:34:56 UTC"
month(datetime) <- 01
datetime
#> [1] "2020-01-08 12:34:56 UTC"
hour(datetime) <- hour(datetime) + 1
update(datetime, year = 2020, month = 2, mday = 2, hour = 2)
#> [1] "2020-02-02 02:34:56 UTC"
ymd("2015-02-01") %>%
update(mday = 30)
#> [1] "2015-03-02"
ymd("2015-02-01") %>%
update(hour = 400)
#> [1] "2015-02-17 16:00:00 UTC"


flights_dt %>%
    mutate(dep_hour = update(dep_time, yday = 1)) %>%
    ggplot(aes(dep_hour)) +
        geom_freqpoly(binwidth = 300)


#---Durations---#
# How old is Hadley?
h_age <- today() - ymd(19791014)
h_age
#> Time difference of 13511 days

as.duration(h_age)
#> [1] "1167350400s (~36.99 years)"

dseconds(15)
#> [1] "15s"
dminutes(10)
#> [1] "600s (~10 minutes)"
dhours(c(12, 24))
#> [1] "43200s (~12 hours)" "86400s (~1 days)"
ddays(0:5)
#> [1] "0s" "86400s (~1 days)"
#> [3] "172800s (~2 days)" "259200s (~3 days)"
#> [5] "345600s (~4 days)" "432000s (~5 days)"
dweeks(3)
#> [1] "1814400s (~3 weeks)"
dyears(1)
#> [1] "31536000s (~52.14 weeks)"

2 * dyears(1)
#> [1] "63072000s (~2 years)"
dyears(1) + dweeks(12) + dhours(15)

tomorrow <- today() + ddays(1)
last_year <- today() - dyears(1)

one_pm <- ymd_hms(
    "2016-03-12 13:00:00",
    tz = "America/New_York"
)

one_pm
#> [1] "2016-03-12 13:00:00 EST"
one_pm + ddays(1)
#> [1] "2016-03-13 14:00:00 EDT"


seconds(15)
#> [1] "15S"
minutes(10)
#> [1] "10M 0S"
hours(c(12, 24))
#> [1] "12H 0M 0S" "24H 0M 0S"
days(7)
#> [1] "7d 0H 0M 0S"
months(1:6)
#> [1] "1m 0d 0H 0M 0S" "2m 0d 0H 0M 0S" "3m 0d 0H 0M 0S"
#> [4] "4m 0d 0H 0M 0S" "5m 0d 0H 0M 0S" "6m 0d 0H 0M 0S"
weeks(3)
#> [1] "21d 0H 0M 0S"
years(1)
#> [1] "1y 0m 0d 0H 0M 0S"
10 * (months(6) + days(1))
#> [1] "60m 10d 0H 0M 0S"
days(50) + hours(25) + minutes(2)
#> [1] "50d 25H 2M 0S"
# A leap year
ymd("2016-01-01") + dyears(1)
#> [1] "2016-12-31"
ymd("2016-01-01") + years(1)
#> [1] "2017-01-01"
# Daylight Savings Time
one_pm + ddays(1)
#> [1] "2016-03-13 14:00:00 EDT"
one_pm + days(1)
#> [1] "2016-03-13 13:00:00 EDT"

flights_dt %>%
  filter(arr_time < dep_time)

flights_dt <- flights_dt %>%
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1)
)

flights_dt %>%
  filter(overnight, arr_time < dep_time)

years(1) / days(1)
next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)
(today() %--% next_year) %/% days(1)

Sys.timezone()
#> [1] "America/Los_Angeles"
length(OlsonNames())
head(OlsonNames())
(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
#> [1] "2015-06-01 12:00:00 EDT"
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
#> [1] "2015-06-01 18:00:00 CEST"
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))
#> [1] "2015-06-02 04:00:00 NZST"

x1 - x2
#> Time difference of 0 secs
x1 - x3
#> Time difference of 0 secs
x4 <- c(x1, x2, x3)
x4
x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
x4a
x4a - x4
x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b

x4b - x4


#---Pipes with magrittr---#
library(magrittr)
foo_foo <- little_bunny()

#-Intermediate Steps-#
foo_foo_1 <- hop(foo_foo, through = forest)
foo_foo_2 <- scoop(foo_foo_1, up = field_mice)
foo_foo_3 <- bop(foo_foo_2, on = head)

diamonds <- ggplot2::diamonds
diamonds2 <- diamonds %>%
    dplyr::mutate(price_per_carat = price / carat)

pryr::object_size(diamonds)
#> 3.46 MB
pryr::object_size(diamonds2)
#> 3.89 MB
pryr::object_size(diamonds, diamonds2)
#> 3.89 MB

diamonds$carat[1] <- NA
pryr::object_size(diamonds)
#> 3.46 MB
pryr::object_size(diamonds2)
#> 3.89 MB
pryr::object_size(diamonds, diamonds2)
#> 4.32 MB

#-Overwrite the Original-#
foo_foo <- hop(foo_foo, through = forest)
foo_foo <- scoop(foo_foo, up = field_mice)
foo_foo <- bop(foo_foo, on = head)

#-Function Composition-#
bop(
  scoop(
    hop(foo_foo, through = forest),
    up = field_mice
  ),
    on = head
)

foo_foo %>%
    hop(through = forest) %>%
    scoop(up = field_mouse) %>%
    bop(on = head)

my_pipe <- function(.) {
    . <- hop(., through = forest)
    . <- scoop(., up = field_mice)
    bop(., on = head)
}
my_pipe(foo_foo)

assign("x", 10)
"x" %>% assign(100)

env <- environment()
"x" %>% assign(100, envir = env)

tryCatch(stop("!"), error = function(e) "An error")
#> [1] "An error"
stop("!") %>%
tryCatch(error = function(e) "An error")
#> Error in eval(expr, envir, enclos): !

rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot() %>%
  str()

rnorm(100) %>%
  matrix(ncol = 2) %T>%
  plot() %>%
  str()

mtcars %$%
  cor(disp, mpg)

mtcars <- mtcars %>%
  transform(cyl = cyl * 2)

mtcars %<>% transform(cyl = cyl * 2)

df <- tibble::tibble(
a = rnorm(10),
b = rnorm(10),
c = rnorm(10),
d = rnorm(10)
)
df$a <- (df$a - min(df$a, na.rm = TRUE)) /
(max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) /
(max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) /
(max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) /
(max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))

(df$a - min(df$a, na.rm = TRUE)) /
(max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
x <- df$a
(x - min(x, na.rm = TRUE)) /
(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])

rescale01 <- function(x) {
    rng <- range(x, na.rm = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(c(0, 5, 10))

rescale01(c(-10, 0, 10))
#> [1] 0.0 0.5 1.0
rescale01(c(1, 2, 3, NA, 5))
#> [1] 0.00 0.25 0.50 NA 1.00

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

x <- c(1:10, Inf)
rescale01(x)


rescale01 <- function(x) {
    rng <- range(x, na.rm = TRUE, finite = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)

mean(is.na(x))
x / sum(x, na.rm = TRUE)
sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)

is_directory <- function(x) file.info(x)$isdir
is_readable <- function(x) file.access(x, 4) == 0

#---Functions Are for Humans and Computers---#
# Too short
f()
# Not a verb, or descriptive
my_awesome_function()
# Long, but clear
impute_missing()
collapse_years()
# Never do this!
col_mins <- function(x, y) {}
rowMaxes <- function(y, x) {}

# Good
input_select()
input_checkbox()
input_text()

# Not so good
select_input()
checkbox_input()
text_input()

# Don't do this!
T <- FALSE
c <- 10
mean <- function(x) sum(x)

f1 <- function(string, prefix) {
    substr(string, 1, nchar(prefix)) == prefix
}
f2 <- function(x) {
    if (length(x) <= 1) return(NULL)
    x[-length(x)]
}
f3 <- function(x, y) {
    rep(y, length.out = length(x))
}

if (condition) {
# code executed when condition is TRUE
} else {
# code executed when condition is FALSE
}


has_name <- function(x) {
    nms <- names(x)
    if (is.null(nms)) {
        rep(FALSE, length(x))
    } else {
    !is.na(nms) & nms != ""
    }
}

#--Conditions--#
if(c(TRUE, FALSE)){}
if(NA){}

identical(0L, 0)

x <- sqrt(2) ^ 2
x
x == 2
#> [1] FALSE
x - 2
#> [1] 4.44e-16

if (this) {
# do that
} else if (that) {
# do something else
} else {
#
}

#> function(x, y, op) {
#> switch(op,
#> plus = x + y,
#> minus = x - y,
#> times = x * y,
#> divide = x / y,
#> stop("Unknown op!")
#> )
#> }

#Good
if (y < 0 && debug) {
    message("Y is negative")
}
if (y == 0) {
    log(x)
} else {
    y ^ x
}
# Bad
if (y < 0 && debug)
    message("Y is negative")
if (y == 0) {
    log(x)
}
else {
    y ^ x
}

y <- 10
x <- if (y < 20) "Too low" else "Too high"

if (y < 20) {
    x <- "Too low"
} else {
    x <- "Too high"
}


if (temp <= 0) {
    "freezing"
} else if (temp <= 10) {
    "cold"
} else if (temp <= 20) {
    "cool"
} else if (temp <= 30) {
    "warm"
} else {
    "hot"
}


switch(x,
    a = ,
    b = "ab",
    c = ,
    d = "cd"
)


#Compute confidence interval around
# mean using normal approximation
mean_ci <- function(x, conf = 0.95) {
    se <- sd(x) / sqrt(length(x))
    alpha <- 1 - conf
    mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}
x <- runif(100)
mean_ci(x)
#> [1] 0.498 0.610
mean_ci(x, conf = 0.99)
#> [1] 0.480 0.628


#Good
mean(1:10, na.rm = TRUE)
# Bad
mean(x = 1:10, , FALSE)
mean(, TRUE, x = c(1:10, NA))

#---Choosing Names---#
#---Checking values---#

wt_mean <- function(x, w) {
    sum(x * w) / sum(x)
}
wt_var <- function(x, w) {
    mu <- wt_mean(x, w)
    sum(w * (x - mu) ^ 2) / sum(w)
}
wt_sd <- function(x, w) {
    sqrt(wt_var(x, w))
}


wt_mean(1:6, 1:3)
#> [1] 2.19

wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
    }
  sum(w * x) / sum(x)
}

wt_mean <- function(x, w, na.rm = FALSE) {
    if (!is.logical(na.rm)) {
      stop("`na.rm` must be logical")
    }
    if (length(na.rm) != 1) {
      stop("`na.rm` must be length 1")
    }
    if (length(x) != length(w)) {
      stop("`x` and `w` must be the same length", call. = FALSE)
    }
    if (na.rm) {
      miss <- is.na(x) | is.na(w)
      x <- x[!miss]
      w <- w[!miss]
  }
  sum(w * x) / sum(x)
}

wt_mean <- function(x, w, na.rm = FALSE) {
    stopifnot(is.logical(na.rm), length(na.rm) == 1)
    stopifnot(length(x) == length(w))
    if (na.rm) {
        miss <- is.na(x) | is.na(w)
        x <- x[!miss]
        w <- w[!miss]
    }
    sum(w * x) / sum(x)
}
wt_mean(1:6, 6:1, na.rm = "foo")
#> Error: is.logical(na.rm) is not TRUE


#Dot-Dot-Dot (…)
sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#> [1] 55
stringr::str_c("a", "b", "c", "d", "e", "f")
commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])
#> [1] "a, b, c, d, e, f, g, h, i, j"
rule <- function(..., pad = "-") {
    title <- paste0(...)
    width <- getOption("width") - nchar(title) - 5
    cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important output")
#> Important output ----------------------------------------

x <- c(1, 2)
sum(x, na.mr = TRUE)

########-----Explicit Return Statements-----#############
complicated_function <- function(x, y, z) {
  if (length(x) == 0 || length(y) == 0) {
    return(0)
  }
  # Complicated code here
}

f <- function() {
    if (x) {
    # Do
    # something
    # that
    # takes
    # many
    # lines
    # to
    # express
    } else {
        # return something short
    }
}   

f <- function() {
    if (!x) {
        return(something_short)
    }
    # Do
    # something
    # that
    # takes
    # many
    # lines
    # to
    # express
}

#--Writing Pipeable Functions---#
show_missings <- function(df) {
    n <- sum(is.na(df))
    cat("Missing values: ", n, "\n", sep = "")
    invisible(df)
}

show_missings(mtcars)
#> Missing values: 0
x <- show_missings(mtcars)
#> Missing values: 0
class(x)
#> [1] "data.frame"
dim(x)
#> [1] 32 11

mtcars %>%
  show_missings() %>%
    mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>%
      show_missings()
f <- function(x) {
x + y
}


y <- 100
f(10)
#> [1] 110

y <- 1000
f(10)


`+` <- function(x, y) {
    if (runif(1) < 0.1) {
        sum(x, y)
} else {
sum(x, y) * 1.1
}
}
table(replicate(1000, 1 + 2))
#>
#> 3 3.3
#> 100 900
rm(`+`)


#-----------Vectors-------------#
library(tidyverse)
#> Loading tidyverse: ggplot2
#> Loading tidyverse: tibble
#> Loading tidyverse: tidyr
#> Loading tidyverse: readr
#> Loading tidyverse: purrr
#> Loading tidyverse: dplyr
#> Conflicts with tidy packages --------------------------------
#> filter(): dplyr, stats
#> lag(): dplyr, stats

typeof(letters)
#> [1] "character"
typeof(1:10)
#> [1] "integer"
x <- list("a", "b", 1:10)
length(x)


#--Important Types of Atomic Vector---#
1:10 %% 3 == 0

c(TRUE, TRUE, FALSE, NA)
typeof(1)
typeof(1L)
1.5L

x <- sqrt(2) ^ 2
x
x - 2
c(-1, 0, 1)/0

is.nan()

#-Character
x <- "This is a reasonably long string."
pryr::object_size(x)
#> 136 B
y <- rep(x, 1000)
pryr::object_size(y)

#-Missing Values
NA # logical
#> [1] NA
NA_integer_ # integer
#> [1] NA
NA_real_ # double
#> [1] NA
NA_character_ # character
#> [1] NA

x <- sample(20, 100, replace = TRUE)
y <- x > 10
sum(y) # how many are greater than 10?
#> [1] 44
mean(y) # what proportion are greater than 10?
if (length(x)) {
    # do something
}

typeof(c(TRUE, 1L))
#> [1] "integer"
typeof(c(1L, 1.5))

#> [1] "double"
typeof(c(1.5, "a"))
sample(10) + 100
runif(10) > 0.5

1:10 + 1:2
1:10 + 1:3
tibble(x = 1:4, y = 1:2)
tibble(x = 1:4, y = rep(1:2, 2))

tibble(x = 1:4, y = rep(1:2, each = 2))

c(x = 1, y = 2, z = 4)
set_names(1:3, c("a", "b", "c"))

#---Subsetting---#
x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]
x[c(1, 1, 5, 5, 5, 2)]

x[c(-1, -3, -5)]
x[c(1, -1)]
x <- c(10, 3, NA, 5, 8, 1, NA)
# All non-missing values of x
x[!is.na(x)]
#> [1] 10 3 5 8 1
# All even (or missing!) values of x
x[x %% 2 == 0]
#> [1] 10 NA 8 NA

x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]

#---Recursive Vectors (Lists)----#
x <- list(1, 2, 3)
str(x)
x_named <- list(a = 1, b = 2, c = 3)
str(x_named)

y <- list("a", 1L, 1.5, TRUE)
str(y)
z <- list(list(1, 2), list(3, 4))
str(z)

x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))
a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
str(a[1:2])
str(a[4])

str(y[[1]])
str(y[[4]])
a$a
#> [1] 1 2 3
x <- 1:10
attr(x, "greeting")
#> NULL
attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
attributes(x)

as.Date
methods("as.Date")
getS3method("as.Date", "default")
getS3method("as.Date", "numeric")

#-Augmented Vectors
#-Factors
x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)
#> [1] "integer"
attributes(x)

x <- as.Date("1971-01-01")
unclass(x)
typeof(x)
#> [1] "double"
attributes(x)
x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)

typeof(x)
#> [1] "double"
attributes(x)
attr(x, "tzone") <- "US/Pacific"
x
#> [1] "1969-12-31 17:00:00 PST"
attr(x, "tzone") <- "US/Eastern"

y <- as.POSIXlt(x)
typeof(y)
#> [1] "list"
attributes(y)

tb <- tibble::tibble(x = 1:5, y = 5:1)
typeof(tb)
#> [1] "list"
attributes(tb)

df <- data.frame(x = 1:5, y = 5:1)
typeof(df)
#> [1] "list"
attributes(df)

###########################################################################################
##########################################################################################
#------------------------Iteration with purrr-----------------------#

library(tidyverse)
df <- tibble(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10)
)
median(df$a)
#> [1] -0.246
median(df$b)
#> [1] -0.287
median(df$c)
#> [1] -0.0567
median(df$d)

output <- vector("double", ncol(df)) # 1. output
for (i in seq_along(df)) { # 2. sequence
    output[[i]] <- median(df[[i]]) # 3. body
}
output

y <- vector("double", 0)
seq_along(y)
#> integer(0)
1:length(y)

out <- ""
for (x in letters) {
    out <- stringr::str_c(out, x)
}

x <- sample(100)
sd <- 0
for (i in seq_along(x)) {
    sd <- sd + (x[i] - mean(x)) ^ 2
}
sd <- sqrt(sd / (length(x) - 1))

x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
    out[i] <- out[i - 1] + x[i]
}


#--For Loop Variations---#
df <- tibble(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10)
)

rescale01 <- function(x) {
    rng <- range(x, na.rm = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
}
df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)


for (i in seq_along(df)) {
    df[[i]] <- rescale01(df[[i]])
}

results <- vector("list", length(x))
names(results) <- names(x)

for (i in seq_along(x)) {
    name <- names(x)[[i]]
    value <- x[[i]]
}

#---Unknown Output Length----#
means <- c(0, 1, 2)
output <- double()
for (i in seq_along(means)) {
    n <- sample(100, 1)
    output <- c(output, rnorm(n, means[[i]]))
}
str(output)

out <- vector("list", length(means))
for (i in seq_along(means)) {
    n <- sample(100, 1)
    out[[i]] <- rnorm(n, means[[i]])
}
str(out)

str(unlist(out))

#-Unknown Sequence Length-#
while (condition) {
# body
}
for (i in seq_along(x)) {
# body
}
# Equivalent to
i <- 1
while (i <= length(x)) {
    # body
    i <- i + 1
}


flip <- function() sample(c("T", "H"), 1)

flips <- 0
nheads <- 0
while (nheads < 3) {
    if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
    flips <- flips + 1
}
flips
#> [1] 3


show_mean(iris)
trans <- list(
    disp = function(x) x * 0.0163871,
    am = function(x) {
    factor(x, labels = c("auto", "manual"))
}
)

for (var in names(trans)) {
    mtcars[[var]] <- trans[[var]](mtcars[[var]])
}

#-For Loops Versus Functionals-#
df <- tibble(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10)
)

output <- vector("double", length(df))
for (i in seq_along(df)) {
    output[[i]] <- mean(df[[i]])
}
output

ol_mean <- function(df) {
    output <- vector("double", length(df))
    for (i in seq_along(df)) {
        output[i] <- mean(df[[i]])
    }
    output
}

col_median <- function(df) {
    output <- vector("double", length(df))
    for (i in seq_along(df)) {
        output[i] <- median(df[[i]])
    }
  output
}

col_sd <- function(df) {
    output <- vector("double", length(df))
    for (i in seq_along(df)) {
      output[i] <- sd(df[[i]])
    }
  output
}


f1 <- function(x) abs(x - mean(x)) ^ 1
f2 <- function(x) abs(x - mean(x)) ^ 2
f3 <- function(x) abs(x - mean(x)) ^ 3

f <- function(x, i) abs(x - mean(x)) ^ i
col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
      out[i] <- fun(df[[i]])
  }
  out
}

col_summary(df, median)
col_summary(df, mean)


#-the map fuction-#
• map() makes a list.
• map_lgl() makes a logical vector.
• map_int() makes an integer vector.
• map_dbl() makes a double vector.
• map_chr() makes a character vector.

map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)
df %>% map_dbl(mean)
df %>% map_dbl(median)
df %>% map_dbl(sd)

map_dbl(df, mean, trim = 0.5)
z <- list(x = 1:3, y = 4:5)
map_int(z, length)


models <- mtcars %>%
  split(.$cyl) %>%
  map(function(df) lm(mpg ~ wt, data = df))

models <- mtcars %>%
    split(.$cyl) %>%
    map(~lm(mpg ~ wt, data = .))

x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2)


x1 <- list(
    c(0.27, 0.37, 0.57, 0.91, 0.20),
    c(0.90, 0.94, 0.66, 0.63, 0.06),
    c(0.21, 0.18, 0.69, 0.38, 0.77)
)
x2 <- list(
    c(0.50, 0.72, 0.99, 0.38, 0.78),
    c(0.93, 0.21, 0.65, 0.13, 0.27),
    c(0.39, 0.01, 0.38, 0.87, 0.34)
)

threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x1 %>% sapply(threshold) %>% str()
x2 %>% sapply(threshold) %>% str()

#-Dealing with Failure-#

safe_log <- safely(log)
str(safe_log(10))
str(safe_log("a"))

x <- list(1, 10, "a")
y <- x %>% map(safely(log))
str(y)

y <- y %>% transpose()
str(y)

is_ok <- y$error %>% map_lgl(is_null)
x[!is_ok]
y$result[is_ok] %>% flatten_dbl()

x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))

x <- list(1, -1)
x %>% map(quietly(log)) %>% str()


#---Mapping over Multiple Arguments---#
mu <- list(5, 10, -3)
mu %>%
    map(rnorm, n = 5) %>%
    str()

sigma <- list(1, 5, 10)
    seq_along(mu) %>%
    map(~rnorm(5, mu[[.]], sigma[[.]])) %>%
    str()


map2(mu, sigma, rnorm, n = 5) %>% str()
map2 <- function(x, y, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], y[[i]], ...)
  }
  out 
}

n <- list(1, 3, 5)
args1 <- list(n, mu, sigma)
args1 %>%
    pmap(rnorm) %>%
    str()

args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>%
    pmap(rnorm) %>%
    str()


params <- tribble(
    ~mean, ~sd, ~n,
    5, 1, 1,
    10, 5, 3,
    -3, 10, 5
)


params %>%
    pmap(rnorm)


#-Invoking Different Functions-#
f <- c("runif", "rnorm", "rpois")
param <- list(
    list(min = -1, max = 1),
    list(sd = 5),
    list(lambda = 10)
)

invoke_map(f, param, n = 5) %>% str()


sim <- tribble(
    ~f, ~params,
    "runif", list(min = -1, max = 1),
    "rnorm", list(sd = 5),
    "rpois", list(lambda = 10)
)

sim %>%
    mutate(sim = invoke_map(f, params, n = 10))

#-Walk-#
x <- list(1, "a", 3)
x %>%
    walk(print)

library(ggplot2)
plots <- mtcars %>%
    split(.$cyl) %>%
    map(~ggplot(., aes(mpg, wt)) + geom_point())
    paths <- stringr::str_c(names(plots), ".pdf")

pwalk(list(paths, plots), ggsave, path = tempdir())


#-Other Patterns of For Loops-#
iris %>%
    keep(is.factor) %>%
    str()

iris %>%
    discard(is.factor) %>%
    str()

x <- list(1:5, letters, list(10))

x %>%
    some(is_character)

x %>%
    every(is_vector)

x <- sample(10)
x
x %>%
    detect(~ . > 5)

x %>%
    detect_index(~ . > 5)

x %>%
    head_while(~ . > 5)

x %>%
    tail_while(~ . > 5)

#---Reduce and Accumulate---#
dfs <- list(
    age = tibble(name = "John", age = 30),
    sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
    trt = tibble(name = "Mary", treatment = "A")
)

dfs %>% reduce(full_join)

vs <- list(
    c(1, 3, 5, 6, 10),
    c(1, 2, 3, 7, 8, 10),
    c(1, 2, 3, 4, 8, 9, 10)
)
vs %>% reduce(intersect)

x <- sample(10)
x
x %>% accumulate(`+`)


col_sum3 <- function(df, f) {
    is_num <- sapply(df, is.numeric)
    df_num <- df[, is_num]
        sapply(df_num, f)
}

df <- tibble(
    x = 1:3,
    y = 3:1,
    z = c("a", "b", "c")
)
# OK

col_sum3(df, mean)
# Has problems: don't always return numeric vector
col_sum3(df[1:2], mean)
col_sum3(df[1], mean)
col_sum3(df[0], mean)


#----Model-----#
library(tidyverse)
library(modelr)
options(na.action = na.warn)
ggplot(sim1, aes(x, y)) +
  geom_point()


models <- tibble(
    a1 = runif(250, -20, 40),
    a2 = runif(250, -5, 5)
)
ggplot(sim1, aes(x, y)) +
    geom_abline(
    aes(intercept = a1, slope = a2),
    data = models, alpha = 1/4
) +
geom_point()

model1 <- function(a, data) {
    a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)

measure_distance <- function(mod, data) {
    diff <- data$y - model1(mod, data)
    sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2) {
    measure_distance(c(a1, a2), sim1)
}

models <- models %>%
    mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

ggplot(sim1, aes(x, y)) +
    geom_point(size = 2, color = "grey30") +
    geom_abline(
        aes(intercept = a1, slope = a2, color = -dist),
        data = filter(models, rank(dist) <= 10)
)

ggplot(models, aes(a1, a2)) +
    geom_point(
    data = filter(models, rank(dist) <= 10),
    size = 4, color = "red"
) +
geom_point(aes(colour = -dist)) 


grid <- expand.grid(
    a1 = seq(-5, 20, length = 25),
    a2 = seq(1, 3, length = 25)
) %>%
    mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>%
    ggplot(aes(a1, a2)) +
    geom_point(
        data = filter(grid, rank(dist) <= 10),
        size = 4, colour = "red"
) +
geom_point(aes(color = -dist))


ggplot(sim1, aes(x, y)) +
    geom_point(size = 2, color = "grey30") +
    geom_abline(
        aes(intercept = a1, slope = a2, color = -dist),
        data = filter(grid, rank(dist) <= 10)
)

best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x, y)) +
    geom_point(size = 2, color = "grey30") +
    geom_abline(intercept = best$par[1], slope = best$par[2])

sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

sim1a <- tibble(
    x = rep(1:10, each = 3),
    y = x * 1.5 + 6 + rt(length(x), df = 2)
)

measure_distance <- function(mod, data) {
    diff <- data$y - make_prediction(mod, data)
    mean(abs(diff))
}

model1 <- function(a, data) {
    a[1] + data$x * a[2] + a[3]
}


#---Visualizing Models---#

#-Predictions-#
grid <- sim1 %>%
    data_grid(x)
grid

grid <- grid %>% 
  add_predictions(stm1_mod)

grid
ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +

geom_line(
    aes(y = pred),
    data = grid,
    colour = "red",
    size = 1
)


sim1 <- sim1 %>%
  add_residuals(sim1_mod)
sim1

ggplot(sim1, aes(resid)) +
  geom_freqpoly(binwidth = 0.5)

ggplot(sim1, aes(x, resid)) +
    geom_ref_line(h = 0) +
    geom_point()


#-Formulas and Model Families-#
df <- tribble(
    ~y, ~x1, ~x2,
    4, 2, 5,
    5, 1, 6
)
model_matrix(df, y ~ x1)

model_matrix(df, y ~ x1 - 1)
model_matrix(df, y ~ x1 + x2)


#-Categorical Variables-#
df <- tribble(
    ~ sex, ~ response,
    "male", 1,
    "female", 2,
    "male", 1
)
model_matrix(df, response ~ sex)

ggplot(sim2) +
  geom_point(aes(x, y))

mod2 <- lm(y ~ x, data = sim2)

grid <- sim2 %>%
    data_grid(x) %>%
    add_predictions(mod2)
grid

ggplot(sim2, aes(x)) +
    geom_point(aes(y = y)) +
    geom_point(
    data = grid,
    aes(y = pred),
    color = "red",
    size = 4
    )
tibble(x = "e") %>%
add_predictions(mod2)

ggplot(sim3, aes(x1, y)) +
  geom_point(aes(color = x2))

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)


grid <- sim3 %>%
    data_grid(x1, x2) %>%
    gather_predictions(mod1, mod2)
grid


ggplot(sim3, aes(x1, y, color = x2)) +
    geom_point() +
    geom_line(data = grid, aes(y = pred)) +
    facet_wrap(~ model)


sim3 <- sim3 %>%
    gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, color = x2)) +
    geom_point() +
    facet_grid(model ~ x2)

#--Interactions (Two Continuous)--#
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>%
  data_grid(
    x1 = seq_range(x1, 5),
    x2 = seq_range(x2, 5)
) %>%
gather_predictions(mod1, mod2)
grid


seq_range(c(0.0123, 0.923423), n = 5)
seq_range(c(0.0123, 0.923423), n = 5, pretty = TRUE)

x1 <- rcauchy(100)
seq_range(x1, n = 5)
seq_range(x1, n = 5, trim = 0.10)
seq_range(x1, n = 5, trim = 0.25)
seq_range(x1, n = 5, trim = 0.50)

x2 <- c(0, 1)
seq_range(x2, n = 5)
seq_range(x2, n = 5, expand = 0.10)
seq_range(x2, n = 5, expand = 0.25)
seq_range(x2, n = 5, expand = 0.50)

ggplot(grid, aes(x1, x2)) +
  geom_tile(aes(fill = pred)) +
    facet_wrap(~ model)

ggplot(grid, aes(x1, pred, color = x2, group = x2)) +
  geom_line() +
  facet_wrap(~ model)

ggplot(grid, aes(x2, pred, color = x1, group = x1)) +
  geom_line() +
  facet_wrap(~ model)

#-Transformations-#
df <- tribble(
    ~y, ~x,
    1, 1,
    2, 2,
    3, 3
)
model_matrix(df, y ~ x^2 + x)
model_matrix(df, y ~ I(x^2) + x)
model_matrix(df, y ~ poly(x, 2))

library(splines)
model_matrix(df, y ~ ns(x, 2))
sim5 <- tibble(
    x = seq(0, 3.5 * pi, length = 50),
    y = 4 * sin(x) + rnorm(length(x))
)
ggplot(sim5, aes(x, y)) +
    geom_point()


mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>%
    data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>%
    gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) +
    geom_point() +
    geom_line(data = grid, color = "red") +
    facet_wrap(~ model)

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

#-Missing Values-#
df <- tribble(
    ~x, ~y,
    1, 2.2,
    2, NA,
    3, 3.5,
    4, 8.3,
    NA, 10
)


mod <- lm(y ~ x, data = df)
mod <- lm(y ~ x, data = df, na.action = na.exclude)
nobs(mod)

#--Other Model Families--#

library(tidyverse)
library(modelr)
options(na.action = na.warn)
library(nycflights13)
library(lubridate)

ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

#-Price and Carat-#
ggplot(diamonds, aes(carat, price)) +
    geom_hex(bins = 50)

diamonds2 <- diamonds %>%
    filter(carat <= 2.5) %>%
    mutate(lprice = log2(price), lcarat = log2(carat))

ggplot(diamonds2, aes(lcarat, lprice)) +
    geom_hex(bins = 50)    

mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

grid <- diamonds2 %>%
    data_grid(carat = seq_range(carat, 20)) %>%
    mutate(lcarat = log2(carat)) %>%
    add_predictions(mod_diamond, "lprice") %>%
    mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) +
    geom_hex(bins = 50) +
    geom_line(data = grid, color = "red", size = 1)

diamonds2 <- diamonds2 %>%
    add_residuals(mod_diamond, "lresid")

ggplot(diamonds2, aes(lcarat, lresid)) +
    geom_hex(bins = 50)

ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()


#-A More Complicated Model-#
mod_diamond2 <- lm(
    lprice ~ lcarat + color + cut + clarity,
    data = diamonds2
)

grid <- diamonds2 %>%
    data_grid(cut, .model = mod_diamond2) %>%
    add_predictions(mod_diamond2)
grid

ggplot(grid, aes(cut, pred)) +
    geom_point()


diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamond2, "lresid2")


ggplot(diamonds2, aes(lcarat, lresid2)) +
  geom_hex(bins = 50)


diamonds2 %>%
    filter(abs(lresid2) > 1) %>%
    add_predictions(mod_diamond2) %>%
    mutate(pred = round(2 ^ pred)) %>%
    select(price, pred, carat:table, x:z) %>%
    arrange(price)
#> # A tibble: 16 ×


#-What Affects the Number of Daily Flights?-#

daily <- flights %>%
    mutate(date = make_date(year, month, day)) %>%
    group_by(date) %>%
    summarize(n = n())
daily

ggplot(daily, aes(date, n)) +
    geom_line()


#-Day of Week-#
daily <- daily %>%
    mutate(wday = wday(date, label = TRUE))
    ggplot(daily, aes(wday, n)) +
geom_boxplot()


mod <- lm(n ~ wday, data = daily)

grid <- daily %>%
    data_grid(wday) %>%
    add_predictions(mod, "n")

ggplot(daily, aes(wday, n)) +
    geom_boxplot() +
    geom_point(data = grid, color = "red", size = 4)


daily <- daily %>%
    add_residuals(mod)

daily %>%
    ggplot(aes(date, resid)) +
    geom_ref_line(h = 0) +
    geom_line()

ggplot(daily, aes(date, resid, color = wday)) +
    geom_ref_line(h = 0) +
    geom_line()

daily %>%
    filter(resid < -100)

daily %>%
    ggplot(aes(date, resid)) +
    geom_ref_line(h = 0) +
    geom_line(color = "grey50") +
    geom_smooth(se = FALSE, span = 0.20)

#---Seasonal Saturday Effect---#
daily %>%
  filter(wday == "Sat") %>%
  ggplot(aes(date, n)) +
    geom_point() +
    geom_line() +
    scale_x_date(
      NULL,
      date_breaks = "1 month",
      date_labels = "%b"
)


term <- function(date) {
  cut(date,
    breaks = ymd(20130101, 20130605, 20130825, 20140101),
    labels = c("spring", "summer", "fall")
  )
}

daily <- daily %>%
  mutate(term = term(date))

daily %>%
    filter(wday == "Sat") %>%
    ggplot(aes(date, n, color = term)) +
    geom_point(alpha = 1/3) +
    geom_line() +
    scale_x_date(
        NULL,
        date_breaks = "1 month",
        date_labels = "%b"
)

daily %>%
    ggplot(aes(wday, n, color = term)) +
    geom_boxplot()

mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>%
    gather_residuals(without_term = mod1, with_term = mod2) %>%
    ggplot(aes(date, resid, color = model)) +
    geom_line(alpha = 0.75)

grid <- daily %>%
  data_grid(wday, term) %>%
  add_predictions(mod2, "n")

ggplot(daily, aes(wday, n)) +
    geom_boxplot() +
    geom_point(data = grid, color = "red") +
    facet_wrap(~ term)

mod3 <- MASS::rlm(n ~ wday * term, data = daily)

daily %>%
    add_residuals(mod3, "resid") %>%
    ggplot(aes(date, resid)) +
    geom_hline(yintercept = 0, size = 2, color = "white") +
    geom_line()

#--Computed Variables--#
compute_vars <- function(data) {
    data %>%
        mutate(
        term = term(date),
        wday = wday(date, label = TRUE)
    )
}

wday2 <- function(x) wday(x, label = TRUE)
mod3 <- lm(n ~ wday2(date) * term(date), data = daily)
library(splines)
mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)

daily %>%
    data_grid(wday, date = seq_range(date, n = 13)) %>%
    add_predictions(mod) %>%
    ggplot(aes(date, pred, color = wday)) +
        geom_line() +
        geom_point()

#===Exercises===#
daily %>%
    top_n(3, resid)


#########################################################################
########################################################################
#---------------------Many Models with purrr and broom -------------------#

library(modelr)
library(tidyverse)

library(gapminder)
gapminder

gapminder %>%
    ggplot(aes(year, lifeExp, group = country)) +
        geom_line(alpha = 1/3)


nz <- filter(gapminder, country == "New Zealand")

nz %>%
    ggplot(aes(year, lifeExp)) +
    geom_line() +
    ggtitle("Full data = ")
    nz_mod <- lm(lifeExp ~ year, data = nz)

nz %>%
    add_predictions(nz_mod) %>%
    ggplot(aes(year, pred)) +
    geom_line() +
    ggtitle("Linear trend + ")

nz %>%
    add_residuals(nz_mod) %>%
    ggplot(aes(year, resid)) +
    geom_hline(yintercept = 0, color = "white", size = 3) +
    geom_line() +
    ggtitle("Remaining pattern")


by_country <- gapminder %>%
    group_by(country, continent) %>%
    nest()

by_country
by_country$data[[1]]


#---List-Columns---#
country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

models <- map(by_country$data, country_model)
by_country <- by_country %>%
  mutate(model = map(data, country_model))
by_country

by_country %>%
  filter(continent == "Europe")

by_country %>%
  arrange(continent, country)

#--Unnesting--#
by_country <- by_country %>%
    mutate(
    resids = map2(data, model, add_residuals)
    )

by_country

resids <- unnest(by_country, resids)
resids

resids %>%
    ggplot(aes(year, resid)) +
        geom_line(aes(group = country), alpha = 1 / 3) +
        geom_smooth(se = FALSE)

#> `geom_smooth()` using method = 'gam'

resids %>%
    ggplot(aes(year, resid, group = country)) +
        geom_line(alpha = 1 / 3) +
        facet_wrap(~continent)

#--Model Quality--#

broom::glance(nz_mod)

by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
    unnest(glance)

glance <- by_country %>%
    mutate(glance = map(model, broom::glance)) %>%
    unnest(glance, .drop = TRUE)
glance

glance %>%
    arrange(r.squared)

glance %>%
    ggplot(aes(continent, r.squared)) +
        geom_jitter(width = 0.5)

bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>%
    semi_join(bad_fit, by = "country") %>%
    ggplot(aes(year, lifeExp, color = country)) +
        geom_line()

#---List-Columns---#
data.frame(x = list(1:3,3:5))

data.frame(
    x = I(list(1:3, 3:5)),
    y = c("1, 2", "3, 4, 5")
)

tibble(
    x = list(1:3, 3:5),
    y = c("1, 2", "3, 4, 5")
)

tribble(
    ~x, ~y,
    1:3, "1, 2",
    3:5, "3, 4, 5"
)


#--With Nesting--#
gapminder %>%
    group_by(country, continent) %>%
    nest()

gapminder %>%
    nest(year:gdpPercap)

df <- tribble(
    ~x1,
    "a,b,c",
    "d,e,f,g"
 )

df %>%
    mutate(x2 = stringr::str_split(x1, ","))

df %>%
    mutate(x2 = stringr::str_split(x1, ",")) %>%
    unnest()

sim <- tribble(
    ~f, ~params,
    "runif", list(min = -1, max = -1),
    "rnorm", list(sd = 5),
    "rpois", list(lambda = 10)
)
sim %>%
    mutate(sims = invoke_map(f, params, n = 10))

mtcars %>%
    group_by(cyl) %>%
    summarize(q = quantile(mpg))

mtcars %>%
    group_by(cyl) %>%
    summarize(q = list(quantile(mpg)))

probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)
    mtcars %>%
        group_by(cyl) %>%
        summarize(p = list(probs), q = list(quantile(mpg, probs))) %>%
        unnest()

x <- list(
    a = 1:5,
    b = 3:4,
    c = 5:6
)

df <- enframe(x)
df

df %>%
    mutate(
        smry = map2_chr(
        name,
        value,
        ~ stringr::str_c(.x, ": ", .y[1])
    )
)

mtcars %>%
    group_by(cyl) %>%
    summarize(q = list(quantile(mpg))) %>%
    unnest()

mtcars %>%
    group_by(cyl) %>%
    summarize_each(funs(list))


#--Simplifying List-Columns--#
df <- tribble(
    ~x,
    letters[1:5],
    1:3,
    runif(5)
)

df %>% mutate(
    type = map_chr(x, typeof),
    length = map_int(x, length)
)

df <- tribble(
    ~x,
    list(a = 1, b = 2),
    list(a = 2, c = 4)
)
df %>% mutate(
    a = map_dbl(x, "a"),
    b = map_dbl(x, "b", .null = NA_real_)
)
#>

tibble(x = 1:2, y = list(1:4, 1)) %>% unnest(y)
df1 <- tribble(
    ~x, ~y, ~z,
    1, c("a", "b"), 1:2,
    2, "c", 3
)
df1
#> #

df1 %>% unnest(y, z)
df2 <- tribble(
    ~x, ~y, ~z,
    1, "a", 1:2,
    2, c("b", "c"), 3
)
df2

df2 %>% unnest(y, z)


#########################################################################
#########################################################################

#---R Markdown---#
mtcars[1:5, 1:10]

knitr::kable(
    mtcars[1:5, ],
    caption = "A knitr kable."
)

knitr::opts_chunk$set(
    comment = "#>",
    collapse = TRUE
)

knitr::opts_chunk$set(
    echo = FALSE
)

comma <- function(x) format(x, digits = 2, big.mark = ",")
comma(3452345)
#> [1] "3,452,345"
comma(.12358124331)
#> [1] "0.12"


rmarkdown::render(
"fuel-economy.Rmd",
params = list(my_class = "suv")
)

reports <- tibble(
    class = unique(mpg$class),
    filename = stringr::str_c("fuel-economy-", class, ".html"),
    params = purrr::map(class, ~ list(my_class = .))
    )
reports

reports %>%
    select(output_file = filename, params) %>%
    purrr::pwalk(rmarkdown::render, input = "fuel-economy.Rmd"\)



##############################################################
#Graphics for Communication with ggplot2

library(tidyverse)

#=Label
ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    geom_smooth(se = FALSE) +
    labs(
        title = paste(
        "Fuel efficiency generally decreases with"
        "engine size"
)

ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    geom_smooth(se = FALSE) +
    labs(
    title = paste(
        "Fuel efficiency generally decreases with"
        "engine size",
    )   
    subtitle = paste(
    "Two seaters (sports cars) are an exception"
    "because of their light weight",
    )
    caption = "Data from fueleconomy.gov"
)

ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    geom_smooth(se = FALSE) +
        labs(
        x = "Engine displacement (L)",
        y = "Highway fuel economy (mpg)",
        colour = "Car type"
)

df <- tibble(
    x = runif(10),
    y = runif(10)
)

ggplot(df, aes(x, y)) +
    geom_point() +
    labs(
      x = quote(sum(x[i] ^ 2, i == 1, n)),
      y = quote(alpha + beta + frac(delta, theta))
)

best_in_class <- mpg %>%
    group_by(class) %>%
    filter(row_number(desc(hwy)) == 1)

ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    geom_text(aes(label = model), data = best_in_class)

ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    geom_label(
        aes(label = model),
        data = best_in_class,
        nudge_y = 2,
        alpha = 0.5
)

ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    geom_point(size = 3, shape = 1, data = best_in_class) +
    ggrepel::geom_label_repel(
        aes(label = model),
        data = best_in_class
)

class_avg <- mpg %>%
    group_by(class) %>%
    summarize(
        displ = median(displ),
        hwy = median(hwy)
)

ggplot(mpg, aes(displ, hwy, color = class)) +
    ggrepel::geom_label_repel(aes(label = class),
    data = class_avg,
    size = 6,
    label.size = 0,
    segment.color = NA
) +
geom_point() +
theme(legend.position = "none")

label <- mpg %>%
  summarize(
    displ = max(displ),
    hwy = max(hwy),
    label = paste(
      "Increasing engine size is \nrelated to"
      "decreasing fuel economy."
)
)
ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_text(
        aes(label = label),
        data = label,
        vjust = "top",
        hjust = "right"
)

label <- tibble(
displ = Inf,
hwy = Inf,
label = paste(
"Increasing engine size is \nrelated to"
"decreasing fuel economy."
)
)
ggplot(mpg, aes(displ, hwy)) +
geom_point() +
geom_text(
aes(label = label),
data = label,
vjust = "top",
hjust = "right"
)

"Increasing engine size related to decreasing fuel economy." %>%
stringr::str_wrap(width = 40) %>%
writeLines()


ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class))

ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    scale_x_continuous() +
    scale_y_continuous() +
    scale_color_discrete()

ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    scale_y_continuous(breaks = seq(15, 40, by = 5))

ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL)

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id)) +
    geom_point() +
    geom_segment(aes(xend = end, yend = id)) +
    scale_x_date(
        NULL,
        breaks = presidential$start,
        date_labels = "'%y"
)

#---Legend Layout---#
base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class))

base + theme(legend.position = "left")
base + theme(legend.position = "top")
base + theme(legend.position = "bottom")
base + theme(legend.position = "right") # the default

ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    geom_smooth(se = FALSE) +
    theme(legend.position = "bottom") +
    guides(
      color = guide_legend(
        nrow = 1,
        override.aes = list(size = 4)
    )
)
#> `geom_smooth()` using method = 'loess'


ggplot(diamonds, aes(carat, price)) +
    geom_bin2d()

ggplot(diamonds, aes(log10(carat), log10(price))) +
    geom_bin2d()

ggplot(diamonds, aes(carat, price)) +
    geom_bin2d() +
    scale_x_log10() +
    scale_y_log10()


ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = drv))

ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = drv)) +
    scale_color_brewer(palette = "Set1")

ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = drv, shape = drv)) +
    scale_color_brewer(palette = "Set1")

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, color = party)) +
    geom_point() +
    geom_segment(aes(xend = end, yend = id)) +
    scale_colour_manual(
      values = c(Republican = "red", Democratic = "blue")
)

df <- tibble(
x = rnorm(10000),
y = rnorm(10000)
)
ggplot(df, aes(x, y)) +
geom_hex() +
coord_fixed()
#> Loading required package: methods
ggplot(df, aes(x, y)) +
geom_hex() +
viridis::scale_fill_viridis() +
coord_fixed()

ggplot(df, aes(x, y)) +
    geom_hex() +
    scale_color_gradient(low = "white", high = "red") +
    coord_fixed()

ggplot(diamonds, aes(carat, price)) +
    geom_point(aes(color = cut), alpha = 1/20)

#---Zooming---#
ggplot(mpg, mapping = aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    geom_smooth() +
    coord_cartesian(xlim = c(5, 7), ylim = c(10, 30))

mpg %>%
    filter(displ >= 5, displ <= 7, hwy >= 10, hwy <= 30) %>%
    ggplot(aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    geom_smooth()

suv <- mpg %>% filter(class == "suv")
compact <- mpg %>% filter(class == "compact")

ggplot(suv, aes(displ, hwy, color = drv)) +
  geom_point()

ggplot(compact, aes(displ, hwy, color = drv)) +
  geom_point()

x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_color_discrete(limits = unique(mpg$drv))

ggplot(suv, aes(displ, hwy, color = drv)) +
    geom_point() +
    x_scale +
    y_scale +
    col_scale

ggplot(compact, aes(displ, hwy, color = drv)) +
    geom_point() +
    x_scale +
    y_scale +
    col_scale


ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    geom_smooth(se = FALSE) +
    theme_bw()


#-Saving Your Plots-
ggplot(mpg, aes(displ, hwy)) + geom_point()
ggsave("my-plot.pdf")
