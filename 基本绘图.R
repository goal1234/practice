  x <- c(3,15,6,19,28,7,14,52,25,5)
  y <- c(26,32,4,56,8,14,63,22,20,10)
  plot(x,y)
  
  #adding labels
  plot(x,y,xlab = 'x-axis',ylab = 'y-axis',main = 'x vs y plot',sub = 'plot of vectors',xlim = c(0,12),ylim=c(0,80))
  
  #adding colors to plot
  plot(x,y,xlab ='x-axis',ylab='y-axis',main = 'x vs y plot',sub = 'plot of vectors',col.lab='red',col.main='green',col.sub='orange',col.axis='blue')
  
  #adding font 
  plot(x,y,xlab = 'x-axis',ylab ='y-axis',main = 'x vs y plot',sub = 'plot of vectors',font.mian=4,font.lab=2.font.axis=3)
  
  #working with lines
  plot(x,y,type='l',lty=3,xlab='x-axis',ylab='y-axis',main='x vs y plot',sub = 'plot of vectors')
  plot(x,y,type = 'l',lty=3,lwd=5,xlab='x-axis',ylab ='y-axis',main = 'x vs y plot',sub = 'plot of vector')\
  
  #text and symbol size
  plot(x,y,lab='x-axis',ylab='y-axis',main = 'x vs y',sub ='plot of vector',cex=2)
  plot(x,y,lab='x-axis',ylab='y-axis',main = 'x vs y',sub ='plot of vector',cex=2,cex.axis=0.7,cex.sub=0.5)
  
  #ploting symbol using pch() function
  plot(x,y,lab='x-axis',ylab='y-axis',main = 'x vs y',sub ='plot of vector',pch=5)
  plot(x,y,lab='x-axis',ylab='y-axis',main = 'x vs y',sub ='plot of vector',pch=8)
  plot(x,y,lab='x-axis',ylab='y-axis',main = 'x vs y',sub ='plot of vector',pch='A')
  
  #use of par() function
  par(mar= c(2,2,2,2),mfrow=c(2,1))
  plot(x,main = 'plot of x')
  plot(y,main = 'plot of y')
  par(mar = c(3,4,5,6),mfrow = c(2,1))
  plot(x,pty = 's ',main ='plot of x')
  plot(x,y,xlab = 'x-axis',ylab='y-axis',main='x vs y',sub = 'plot of vectors',tck=0.2)
  dev.off()
  par(mpg=c(0,1,0))
  plot(x,y,xlab = 'x-axis',ylab = 'y-axis',main ='x vs y',sub ='plot of vectors')
  
  x <- c(12,4,21,17,13,9)
  barplot(x)
  barplot(x,col='slategray3')
  barplot(x,col=colors()[102])
  barplot(x,col=colors()[602])
  barplot(x,col=rgb(.52,.0,.0))
  barplot(x,col = c('red','blue'))
  barplot(x,col = c('red','blue','green','yellow'))
  rm(list=ls())
  
  #exploring color with rcolorbrewer
   #barplot
  x <- c(12,4,21,17,13,9)
  barplot(x)
  
  library(RColorBrewer)
  
  #show all
  display.brewer.all()
  display.brewer.pal(8,'Accent')
  display.brewer.pal(4,'Spectral')
  
  #can save palette as vector or call in function
  blues <- brewer.pal(6,'Blues')  #用Blues中调色板的前6个
  barplot(x,col=blues)
  barplot(x,col=brewer.pal(6,'Greens'))
  barplot(x,col=brewer.pal(6,"YlorRd"))
  barplot(x,col=brewer.pal(6,'RdGy'))
  
  rm(list=ls())
  
  ##############################################################
  x <- c(7,15,09,20,17,13)
  barplot(x)
  barplot(x,col=1:6)
  barplot(x,col=rainbow(6))
  barplot(x,col=heat.colors(6))
  barplot(x,col=terrain.colors(6))
  barplot(x,col=cm.colors(6))
  rm(list=ls())
  
  #####################
  library(ggmap)
  View(Crime)
  plot(Crime$Year1983,type='o',col='black',axes= FALSE)
  
  #Make x axis using crime state labels
  axis(1,at=1:51,lab=Crime$State,las=2)
  
  #Make y axis with horizontal labels that display ticks
  axis(2,las=1)
  
  #plot crime rate of year 1993
  lines(Crime$Year1993,type='o',pch=22,lty=2,col='brown')
  
  #create a title with red,bold/italic font
  title(main='crime rate',col.main='green',font.main=4)
  
  #=====================scatterplot=========================#
  plot(Trees,col='blue')
  
  #scatterplot for single column
  plot(Trees$Height,col='red')
  
  plot(Trees$Height,col='red',cex=1,pch=24)
  
  #add line and points
  plot(Trees$Height,col='red',type='o',cex=1,pch=24)
  
  #plot between volumn and girth
  plot(Volumn~Girth,data=Trees,
       main = 'Girth and volumn for black cherry trees',
       col.main='red',pch=6,cex=1,col='blue',xlim=c(0,25),ylim=c(0,100))
  
  rm(list=ls())
  
  #=================BARRPLOT============================#
  plot(Arthritis$Sex)
  Arth_gender <- table(Arthritis$Sex)
  Arth_gender
  
  #customize the chart
  par(oma=c(1,1,1,1))
  par(mar=c(4,5,2,1))
  
  barplot(Arth_gender[order(Arth_gender)],
          las=1 ,#las gives orientation of axis labels
          col= cm.colors(2),
          border = NA,
          main = 'Number of patients taking Treatment',
          xlab= 'Categoties of Sex',
          ylab= 'Count')
  # show horizontally bars
  barplot(Arth_gender[order(Arth_gender)],horiz = TRUE,
          las=1,
          col=cm.colors(2),
          border = 'black',
          main = 'number of patients taking treatment',
          xlab='Categories of Sex',
          ylab ='Count')
  
  #stack barplot
  counts <- table(Arthritis$Sex,Arthritis$Improved)
  counts
  barplot(counts,
          main='Arthritis',
          xlab='Improvement in Patients',
          col=c('darkblue','red'),
          legend=rownames(counts))
  
  #grouped barplot
  barplot(counts,
          main='Arthritis',
          xlab='Improvement in Patients',
          col=c('darkblue','red'),
          legend=rownames(counts),
          beside = TRUE)
  #================HISTOGRAM==================#
  View(mtcars)
  
  #compute a histogram of the data values in the column of dataframe named 'mtcar'
  hist(mtcars$mpg)
  
  #adding breaks and limits
  hist(mtcars$mpg,breaks = 14,xlim=c(10,35),ylim = c(0,10))
  
  #adding colors to graph
  hist(mtcars$mpg,breaks=14,xlim=c(10,35),
       col=rainbow(14),main='Histogram of Mtcars',
       col.main = 'blue',col.lab='red')
  
  #adding labels to the graph
  hist(mtcars$mpg,breaks=14,xlim=c(10,35),
       col=rainbow(14),main='Histogram of Mtcars',
       col.main='Blue',col.lab='red',labels=TRUE)
  
  #overlapping histogram
  h1 <- mtcars$mpg
  #Histogram Colored (blue and red)
  hist(h1,
       col='red',
       xlim = c(10,30),ylim=c(0,10),breaks = 10,
       main = 'overlapping hisogram',xlab= 'Variabel')
  h2<- mtcars$qsec
  hist(h2,col='blue',xlim = c(0,10),add=T)
  box()
  
  #=====================piechart================#
  View(Employee)
  
  #make pie chart with default
  pie(Employee)
  
  #Modify piechart
  pie(Employee$SAL,
      main='Salary Pie Chart',
      col.main= 'Darkgreen',
      labels=Employee$ENAME,
      col=rainbow(14))
  #plot box
  box(col='blue')
  
  #we can also use percentage of the salary to label the pie chart
  SAL_labels <- round(Employee$SAL/sum(Employee$SAL)*100，1)
  SAL_labels
  lbls <- paste(Employee$ENAME,SAL_labels)
  lbls
  lbls <- paste(labls,"%",sep='') # add % to labels
  
  pie(Employee$SAL,
      main = 'Salary Pie Chart',
      labels=lbsl,
      col = rainbow(14))
  
  #change the text color and size
  pie(Employee$SAL,
      main='Salary Pie Chart',
      labels = lbls,
      col=rainbow(14),cex=0.5)
  
  #=====================TABLEPLOT=====================#
  library(ggplot2);library(tabplot)
  View(diamonds)
  
  #add some NA'S to price and cut variables
  is.na(diamonds$cut) <- diamonds$cut =='Ideal'
  View(diamonds)
  
  #plot tabplot using tabplot function
  tableplot(diamonds,
            select = c(carat,price,cut,color,clarity),
            sortCol = price)
  
  #=====================BOXPLOT=====================#
  View(schooldays)
  
  boxplot(schooldays$absent)
  
  #how many female and male student absent
  boxplot(schooldays$absent~schooldays$gender)
  
  #color the boxplot
  boxplot(schooldays$absent~schooldays$gender,
          col=rainbow(2))
  
  #a notch is drawn in each sied of the boxes
  boxplot(schooldays$absent~schooldays$gender,
          col=rainbow(2),
          notch=TRUE)
  
  #view the boxplot hirzontally with change border color
  boxplot(schooldays$absent~schooldays$gender,
          col=rainbow(2),
          notch=TRUE,
          horizontal = TRUE,
          border= 'blue',las= 2)
  
  #change the width of the boxplot and whiske line type
  boxplot(schooldays$absent~schooldays$gender,
          col=rainbow(2),
          notch=TRUE,
          horizontal = TRUE,
          border= 'blue',
          las =2,
          boxwex=0.5,
          whisklty=1)
  
  #===================organizational chart====================#
  library(googleVis)
  
  #View dataset regions
  View(Regions)
  
  Org1 <- gvisOrgChart(Regions,idvar = "Region",
                       parentvar = "Parent",
                       tipvar= 'Val')
  plot(Org1)
  
  #===================network graph=========================#
  library(igraph)
  #import the data
  View(FriendsNetwork)
  #load (DIRECTED) graph from data frame
  directed_graph <- graph.data.frame(FriendsNetwork,directed = TRUE)
  #plot graph
  plot(directed_graph)
  
  undirected_graph <- graph.data.frame(FriendsNetwork,directed = FALSE)
  plot(undirected_graph)
  
  #==========================ggplot2========================#
  library(ggplot2)
  View(state)
  
  #=======================line graph========================#
  p1 <- ggplot(state,
               aes(x= Income,y=Illiteracy))+geom_line(colour='blue',linestyle='solid',
                                                      size=1)+xlab('Income of the people')+ylab('illiteracy in state')+ggtitle('line graph')
  #=======================scatter plot======================#
  p2 <- ggplot(state,
               aex(x=Income,y=Illiteracy)) + geom_point(aes(color=factor(region)))+
    xlab('income of the people') + ylab('Illiteracy in state') + ggtitle('Scatter plot')
  p2
  #=======================histogram=========================#
  p3<-ggplot(state,
             aes(x=Income)) + geom_histogram(colour='yellow',fill='red')+xlab('Icome of the people')+
    ylab('Frequency') +ggtitle('Histogram')
  #=======================boxplot===========================#
  p4 <- ggplot(state,
               aes(x=region,y=Population)) + geom_boxplot(fill=topo.colors(4)) +
    xlab('Region') + ylab('Population') + ggtitle('Boxplot')
  #======================bar plot===========================#
  p5 <- ggplot(state,
               aes(x=region,y=Murder,fill=grades)) +
    geom_bar(position = 'dodge',stat='identity')
  p5
  
  #=========================================================#
  library(gridExtra)
  grid.arrange(p1,p2,p3,p4,p5)
  
  #==============================ggtheme========================#
  library(ggthemes)
  library(ggplot2)
  g2 <- ggplot(diamonds,aes(x=carat,y=price))+geom_point(aes(color=color))
  g2
  
  g2+theme_economist() + scale_color_economist()
  g2+theme_excel() +scale_color_excel()
  
  #==========================grid and wrap=================#
  library(ggplot2)
  View(tips)
  
  sp <- ggplot(tips,aes(x= total_bill,y=tip/total_bill))+geom_point(shape=1)
  sp+facet_wrap(~sex,nrow=1);sp+facet_wrap(~sex,ncol=1)
  
  p2 <- ggplot(data=tips,aes(x=total_bill,y=tip/total_bill,color=day))+geom_point()
  p2 
  p2+facet_wrap(~sex)
  
  #========================facet_grid function===============#
  #Divide by level of 'time', in the horizontal direction
  p2 + facet_grid(time~.)
  
  #Divide by level of 'time in the vertical direction
  p2 + facet_grid(.~time)
  
  #Divide with 'sex' vertical,'day'horizontal
  p2 + facet_grid(sex~time)
  
  #=======================add legend=========================#
  library(ggplot2)
  View(PlantGrowth)
  
  #Plot a simple box plot using dataset plantgrowth
  bp <- ggplot(date=PlantGrowth,aes(x=group,y = weight,fill=group)) + geom_boxplot()
  bp
  
  #removing the legend
  bp + guides(fill=FALSE)
  
  #HIDING the legend title
  #remove title for fill legend
  bp + guide(fill=guide_legend(title=NULL))
  
  #Modifying the text of legend titles
  bp + scale_fill_discrete(name = 'Experimental \nCondition')
  
  #scatterplot between eruption and waiting
  p <- ggplot(faithful,aes(x=eruptions,y=waiting)) + geom_point()
  p
  
  #annotated the plot by group1 an group2
  p + annotate('text',x=3,y=48,label='Group 1') +
    annotate('text',x=4.5,y=66,label = 'Group 2')
  
  #change the color font of text
  p + annotate('text',x=3,y=48,label='Group 1',
               fontface='italic',colour = 'darked',size=10)+
    annotate('text',x=4.5,y=66,label='Group 2',fontface='bold',colour ='darkblue',size=8)
  faithful <- faithful[1:50,]
  data<-1:50
  ggplot(faithful,aes(x=eruptions,y=waiting)) + geom_point(colour = 'red') + geom_text(aes(label=data),hjust=1,vjust=1)
  
  #===================area chart===================#
  df <- data.frame(alpha=rep(letters[1:3],each=10),year= rep(1:10,3),value=abs(rnorm(30)))
  View(df)
  library(ggplot2)
  qplot(year,value,data=df,geom='area',fill=alpha)
  
  ####################plotrix
  library(plotrix)
  View(Titanic)
  
  barNest(Survied~Class+Age+Sex,Titanic,showall = TRUE,
          main ='Titanic survival by class,age and sex',ylab = 'Proportion surving',
          FUN='propbrk',shrink = 0.20,trueval = 'Yes')
  #=========================GANNTT CHART=====================+
  gantt.chart <- list(labels=c('jim','joe','jim','john','john','jake','joe','jed','jake'),
                      starts = c(8.1,8.7,13,9.1,11.6,9.0,13.6,9.3,13.2),
                      ends = c(12.5,12.7,16.5,10.3,15.6,11.7,18.1,18.2,19))
  View(gantt.chart)
  gantt.chart(gantt.chart,vgridlab = 8:19,vgridpos = 8:19,
              main='All bars having the same color',taskcolors = 'orange')
  #fill colors in gantt chart
  gantt.chart(gantt.chart,vgridlab = 8:19,vgridpos = 8:19,
              main='All bars having the same color',taskcolors = c(2,3,7,4,8))
  
  #border of gantt chart
  gantt.chart(gantt.chart,vgridlab = 8:19,vgridpos = 8:19,
              main='All bars having the same color',taskcolors = c(2,3,7,4,8),border.col='black')
  
  #========================zoom in plot=======================#
  library(plotrix)
  zoomInPlot(rnorm(100),rnorm(100),rxlim = c(-1,1),rylim = c(-1,1))
  
  zoomInPlot(rnorm(100),rnorm(100),rxlim = c(-1,1),rylim = c(-1,1),
             zoomtitle = 'zoom in plot',titlepos = 1.5,
             pch=20,xlab='x-axis',ylab='y-axis',col='blue')
  #=======================fan plot===========================#
  library(plotrix)
  
  #make dataset
  geo_df <- data.frame(continent=c('Africa','Asia','Eurpoe','N&C America','S America','Oceania'),
                       area = c(5994,7737,1987,4716,5097,2093))
  
  #fanplot
  fan.plot(geo_df$area,max.span = pi,ticks=400,
           labels=paste(geo_df$continent,geo_df$area,sep=''),
           main='Countries with their geographical area(fan.plot)')
  
  #==================adding table===================#
  library(plotrix)
  testdf <- data.frame(First=c(10,7,5),Second=c(8,6,2),Third=c(5,3,4))
  rownames(testdf) <- c('red','green','blue')
  
  barp(testdf,main='bar plot',ylab='value',
       names.arg = colnames(testdf),col=2:4)
  
  #show most of the options
  addtable2plot(2,8,testdf,bty = 'o',display.rownames = TRUE,hlines=TRUE,
                title='table')
  
  #==========================radial plot================#
  ions <- c(3.2,5.1,3.1,2.1,4.5)
  ion.names <- c('Na','Ca','Mg','Cl','HCo3','SO4')
  
  #plot the graph
  radial.plot(ions,labels=ion.names,rp.type = 'r',main= 'Dissolved iones in water',
              radial.lim = c(0,7),line.col = 'blue',lwd=2)
  radial.plot(ions,labels=ion.names,rp.type = 'p',main= 'Dissolved iones in water',
              radial.lim = c(0,7),line.col = 'blue',lwd=2)
  radial.plot(ions,labels=ion.names,rp.type = 'p',main= 'Dissolved iones in water',
              radial.lim = c(0,7),line.col = 'blue',poly.col = 'yellow',lwd=2,show.grid.labels = TRUE,
              show.centroid = TRUE)