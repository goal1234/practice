# german credit data #
# https://onlinecourses.science.psu.edu/stat857/node/215

credit_data <-"https://onlinecourses.science.psu.edu/stat857/sites/onlinecourses.science.psu.edu.stat857/files/german_credit.csv"
train <- "https://onlinecourses.science.psu.edu/stat857/sites/onlinecourses.science.psu.edu.stat857/files/Training50.csv"
test <- "https://onlinecourses.science.psu.edu/stat857/sites/onlinecourses.science.psu.edu.stat857/files/Test50.csv"
name <- "https://onlinecourses.science.psu.edu/stat857/sites/onlinecourses.science.psu.edu.stat857/files/german_credit.csv"

German.Credit <- read.csv(name)

attach(German.Credit)

margin.table(
  prop.table(
    table(Duration.in.Current.address, Most.valuable.available.asset, 
          Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,
          Telephone, Foreign.Worker)),1)

for(i in 1:8) {
  t <- margin.table(
        prop.table(
          table(Duration.in.Current.address, Most.valuable.available.asset, 
                Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,
                Telephone, Foreign.Worker)),i)
  print(t)
}

#install.packages("gmodels")

library(gmodels)
# creating K1 x K2 contingency table.
CrossTable(Creditability, Account.Balance, 
           digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(Creditability, Payment.Status.of.Previous.Credit, 
           digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(Creditability, Purpose, 
           digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

# --- #
# Summary statistics are printed for this variable
summary(Duration.of.Credit..month.) 

# Bins for a nice looking histogram
brksCredit <- seq(0, 80, 10) 

# produces nice looking histogram
hist(Duration.of.Credit..month., breaks=brksCredit, 
     xlab = "Credit Month", ylab = "Frequency", main = " ", cex=0.4) 

boxplot(Duration.of.Credit..month., 
        bty="n",xlab = "Credit Month", cex=0.4) # For boxplot

# --- #
# Random sample of 50% of row numbers created
indexes = sample(1:nrow(German.Credit), size=0.5*nrow(German.Credit))
# Training data contains created indices
Train50 <- German.Credit[indexes,] 
# Test data contains the rest
Test50 <- German.Credit[-indexes,] 
# Using any proportion, other than 0.5 above and size Training and Test data can be constructed


# ---- #
LogisticModel50 <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + 
                         Purpose + Value.Savings.Stocks + Length.of.current.employment + Sex...Marital.Status + 
                         Most.valuable.available.asset + Type.of.apartment + Concurrent.Credits + Duration.of.Credit..month.+ 
                         Credit.Amount + Age..years., family=binomial, data = Train50)

LogisticModel50final <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + 
                              Purpose + Length.of.current.employment + Sex...Marital.Status, 
                            family=binomial, data = Train50)
fit50 <- fitted.values(LogisticModel50S1)

library(h2o)
h2o.init(max_mem_size = '2G')
h2o.init()
datatrain <- as.h2o(Train50)

x.name <- colnames(Train50)[1]
y.name <- colnames(Train50)[2:21]

model <- h2o.glm(y.name, x.name, datatrain, family = "binomial", alpha = 0.03)
h2o.performance(model = model)

Threshold50 <- rep(0,500)

for (i in 1:500){
  if(fit50[i] >= 0.5) Threshold50[i] <- 1
}

CrossTable(Train50$Creditability, Threshold50, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=F, data=Train50)
perf <- performance(pred, "tpr", "fpr")
plot(perf)







