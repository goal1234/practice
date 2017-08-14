  library(magrittr)
  library(ggplot2)
  library(tibble)
  library(plyr)
  data("cars")
  cars %>% head
  cars %>% ggplot(aes(x = speed,y=dist)) +
    geom_point() +
    geom_smooth(method = 'lm')
  
  #-----
  cars %>% lm(dist~speed,data= .) %>% summary
  cars %>% lm(dist~speed,data= .) %>% coefficients
  cars %>% lm(dist~speed,data= .) %>% confint
  
  #------
  predict_dist <- function(speed,theta_1){
    data.frame(speed=speed,
               dist = theta_1 *speed,
               theta_1 = as.factor(theta_1))
  }
  
  cars %>% ggplot(aes(x= speed,y=dist,colour=theta)) +
    geom_point(colour='black') +
    geom_line(data = predict_dist(cars$speed,2)) +
    geom_line(data = predict_dist(cars$speed,3)) +
    geom_line(data = predict_dist(cars$speed,4)) +
    scale_color_discrete(name=expression(theta[1]))
  #------------------------------------------------
  thetas <- seq(0,5,length.out = 50)
  fitting_error <- Vectorize(function(theta)
    sum((theta *cars$speed-cars$dist)**2))
  
  data.frame(thetas=thetas,errors = fitting_error(theta = thetas)) %>%
    ggplot(aes(x=thetas,y=errors)) +
    geom_line() +
    xlab(expression(theta[1]))+ylab("")
  
  #----------------------------------------------------
  cars %>% lm(dist~speed-1,data = .) %>% coefficients
  cars %>% ggplot(aes(x= speed,y=dist)) +
    geom_point() +
    geom_smooth(method = 'lm',formula = y~x -1)
  
  #-------------logistic
  library(mlbench)
  data("BreastCancer")
  BreastCancer %>% head
  BreastCancer %>% 
    ggplot(aes(x=Cl.thickness,y=Class)) +
    geom_jitter(width = 0.5,height = 0.05,alpha=0.4)
  
  BreastCancer %>% 
    mutate(Cl.thickness.numeric = 
             as.numeric(as.character(Cl.thickness))) %>% 
    mutate(IsMaligant = ifelse(Class=="benign",0,1)) %>%
    ggplot(aes(x= Cl.thickness.numeric,y=IsMaligant)) +
    geom_jitter(height = 0.05,width = 0.3,alpha=0.4) +
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  
  BreastCancer %>% 
    mutate(Cl.thickness.numeric =
             as.numeric(as.character(Cl.thickness))) %>%
    mutate(IsMaligant = ifelse(Class=="benign",0,1)) %>%
    glm(IsMaligant~Cl.thickness.numeric ,
        family = "binomial",
        data=.)
  #---------------------------------------------------------------------------------
  cars %>% 
    model.matrix(dist~speed-1,data=.) %>%
    head(5)
  BreastCancer %>% 
    mutate(Cl.thickness.numeric =
             as.numeric(as.character(Cl.thickness)),
           Cell.size.numeric = 
             as.numeric(as.character(Cell.size))) %>%
    mutate(IsMaligant = ifelse(Class=="benign",0,1)) %>%
    model.matrix(IsMaligant~Cl.thickness.numeric + Cell.size.numeric,
                 data=.) %>%
    head(5)
  
  BreastCancer %>%
    mutate(IsMaligant = ifelse(Class=="benign",0,1)) %>%
    model.matrix(IsMaligant ~ Cl.thickness,data=.) %>%
    head(5)
  
  cars %>% lm(dist~speed + I(speed^2),data=.) %>% 
    head
  
  cars %>% lm(dist~speed +I(speed^2),data=.) %>%
    summary
  
  cars %>% ggplot(aes(x= speed,y=dist)) + 
    geom_point() +
     geom_smooth(method = 'lm',formula = y~x+I(x^2))
  
  line <- cars %>% lm(dist~speed,data=.)
  poly<- cars %>% lm(dist~speed+I(speed^2),data= .)
  predict(line,cars) %>% head
  predict(poly,cars) %>% head
  
  #---------------validating Models-------------------------------------------------
  line <- cars %>% lm(dist~speed,data=.)
  poly<- cars %>% lm(dist~speed+I(speed^2),data= .)
  predict(line,cars) %>% head
  predict(poly,cars) %>% head
  #evaluating regression models
  rmse <- function(x,t)sqrt(mean(sum(t-x)^2))
  rmse(predict(line,cars),cars$dist)
  rmse(predict(poly,cars),cars$dist)
  training_data <- cars[1:25,]
  test_data <- cars[26:50,]
  line <- training_data %>% lm(dist~speed,data=.)
  poly <- training_data %>% lm(dist~speed+I(speed^2),data=.)
  rmse(predict(line,cars),test_data$dist)
  rmse(predict(poly,cars),test_data$dist)
  
  sampled_cars %>% 
    mutate(training = sample(0:1,nrow(cars),replace=TRUE))
  sampled_cars %>%¡¡head
  training_data <- sample_cars %>% filter(training==1)
  test_data <- sampled_cars %>% filter(training==0)
  training_data %>% head
  test_data %>% head
  
  line <- training_data %>% lm(dist~speed,data=.)
  poly <- training_data %>% lm(dist~speed+I(speed^2),data=.)
  rmse(predict(line,cars),test_data$dist)
  rmse(predict(poly,cars),test_data$dist)
  
  #---------evaluating classification models
  formatted_data <- BreastCancer %>%
    mutate(Cl.thickness.numeric =
             as.numeric(as.character(Cl.thickness)),
           Cell.size.numeric = 
             as.numeric(as.character(Cell.size))) %>%
    mutate(IsMaligant = ifelse(Class=="benign",0,1)) 
  fitted_model <- formatted_data %>% 
    glm(IsMaligant~Cl.thickness.numeric+Cell.size.numeric,data=.)
  predict(fitted_model,formatted_data,type='response') %>% head
  classify <- function(probability) ifelse(probability<0.5,0,1)
  classified_malignant <- classify(predict(fitted_model,formatted_data,type='response'))
  #---------------------confusion matrix
  table(formatted_data$IsMaligant,classified_malignant)
  table(formatted_data$IsMaligant,classified_malignant,dnn=c("DATA","Predictions"))
  
  classify <- function(probabiltiy) 
    ifelse( probability <0.5,"benign","malignant")
  classified <- classify(predict(fitted_model,formatted_data))
  table(formatted_data$Class,classified,dnn = c("data","predictions"))
  confusion_matrix <- table(formatted_data$Class,classified,dnn=c('data','prediction'))
  (accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix))
  table(BreastCancer$Class)
  tb1 <- table(BreastCancer$Class)
  tb1['benign'] <- sum(tb1)
  table(BreastCancer$class,sample(BreastCancer$Class))
  accuray <- function(confusion_matrix)sum(diag(confusion_matrix))/sum(confusion_matrix)
  replicate(8,accuray(table(BreastCancer$Class,sample(BreastCancer$Class))))
  
  #-------------------------------sensitivity and specificity
  (specificity <- confusion_matrix[1,1]/
      (confusion_matrix[1,1]+confusion_matrix[1,2]))
  (specificity <- confusion_matrix[2,2]/
      (confusion_matrix[2,1] + confusion_matrix[2,2]))
  
            
  
  
  