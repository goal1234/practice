# 在聚合，提取数据  
# 分组函数  
# 更新聚合  
# 链接结果  

# packages

- dplyr
- data.table
- reshape2
- tidyr
- tidyverse


# data.table

- subset
- select
- group
- update
- join

用fread创建了一个data.table对象  

        flights <- fread("https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv")
        flights
        dim(flights)


字符型的列，不会被自动转化成因子  
超过100行，只会输出开头和结尾  
不能设表头名  

        DT <- data.table(ID = c("b","b","b","a","a","c"), a = 1:6, b = 7:12, c=13:18)
        getOptions("datatable.print.nrows")

**不仅仅是行和列：
        DT[i, j, by]
- i: 行
- j: 列
- by: 分组


** 列的条件在行上选出
        ans <- flights[origin == "JFK" & month == 6L]
        head(ans)

不用像data.frame中的 `filghts$origin == 'JFK' & flights$month == 6l`这样的筛选
同时,不是必须选取的

前两行
        ans <- flights[1:2]
        ans

对data.table按照不同的方式排序，多个字段，升序降序
        ans <- flights[order(origin, -dest)]
        head(ans)


forder()更快的排序
        odt = data.table(col=sample(1e7))
        (t1 <- system.time(ans1 <- odt[base::order(col)]))  ## uses order from base R
        (t2 <- system.time(ans2 <- odt[order(col)]))        ## uses data.table's forder
        (identical(ans1, ans2))

选择列出来的对象是什么  

        ans <- flights[, arr_delay]
        head(ans)  # 返回向量

        ans <- flights[, list(arr_delay)]
        head(ans)  # 返回列表

        ans <- flights[, .(arr_delay, dep_delay)]
        head(ans)  # .() 和list()是相同的

        ans <- flights[, .(delay_arr = arr_delay, delay_dep = dep_delay)]
        head(ans)  # 进行一个别名选取

**j里面进行运算，列上提满足行的
        ans <- flights[, sum((arr_delay + dep_delay) < 0)]
        ans

> 在6月份，从'JFK'航班起飞的，计算飞和到达的平均延误时间
> 行上选，列上进行计算


        ans <- flights[origin == "JFK" & month == 6L, 
                .(m_arr=mean(arr_delay), m_dep=mean(dep_delay))]
        ans

> 在六月份，从”JFK”机场起飞的航班一共有多少 
> 行上选，列上进行求和

        ans <- flights[origin == 'JFK' & month == 6L,length(dest)]
        ans

        ans <- flights[origin == "JFK" & month == 6L, .N]
        ans   # 内建对象.N相当可以


***聚合分组***

> by分组

> 每个机场的航班数

        ans <- flights[, .(.N), by=.(origin)]
        ans  # .(.N)

        ans <- flights[, .N, by=origin]
        ans  # 当这有一个的时候退化成这样


> 如何获取美航（carrier code代码是“AA”）在每个机场起飞的航班数
> 先进行了行筛选，再分组计算

        ans <- flights[carrier == "AA", .N, by=origin]
        ans
        ans <- flights[carrier == "AA", .N, by=.(origin,dest)]
        head(ans) # by参数可以接受多个列

> 如何获取美航在所有机场的起／降的平均延误时间
> 行筛选，分组，列运算求值

        ans <- flights[carrier == "AA", 
            .(mean(arr_delay), mean(dep_delay)), 
            by=.(origin, dest, month)]
        ans

> 本身保持了原来的顺序，不过有的时候能够希望进行排序
> 用一个参数keyby

        ans <- flights[carrier == "AA", 
            .(mean(arr_delay), mean(dep_delay)), 
            keyby=.(origin, dest, month)]
        ans

> 行筛选，分组，列运算，排序，这个一个链式的

        ans <- flights[carrier == "AA", .N, by = .(origin, dest)]
        ans <- ans[order(origin, -dest)]
        head(ans)

        ans <- flights[carrier == "AA", .N, by=.(origin, dest)][order(origin, -dest)]
        head(ans, 10)

        DT[...
        ][...
        ][...
        ]

> 满足条件的进入by

        ans <- flights[, .N, .(dep_delay>0, arr_delay>0)]
        ans
        #    dep_delay arr_delay      N
        #1:      TRUE      TRUE  72836
        #2:     FALSE      TRUE  34583
        #3:     FALSE     FALSE 119304
        #4:      TRUE     FALSE  26593

        # 这个只是一堆的TF
        b <- flights[dep_delay>0,arr_delay>0, .N]
        b

> 每次进行输出.SD，如ID = a然后print(), ID = b 然后print() ID = c然后print
        
        DT[, print(.SD), by=ID]

> 每次之后进行均值运算a是

        DT[, lapply(.SD, mean), by=ID]

> 每次都要确保相关的包含 .SDcols = c("arr_delay", "dep_delay")
> 移除的时候用 -或者！
> 对origin,dest,month 在左边然后 arr_delay, dep_delay在右边加上均值等

        flights[carrier == 'AA',
                lapply(.SD, mean),
                by = .(origin, dest, month),
                .SDcols=c("arr_delay", "dep_delay")]

> 返回每个月前两行

        ans <- flights[, head(.SD, 2), by = month]
        head(ans)


> 融数据，从宽的变成长的
> 分组累计
  
        DT
        DT[, (val = c(a,b)),by = ID]

        DT[, .(val = list(c(a,b))), by=ID]

        DT[, print(c(a,b)), by=ID]
        DT[, print(list(c(a,b))), by=ID]

        DT[, head(.SD, 2), by=.]