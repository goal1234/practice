###   Kapitel 7:  Statistik mit R   ###

#############################################
### 7.1 Grundlegende Funktionen

x <- c(5, 7, 2, 7, 8, 9)
sort(x)
sort(x, decreasing = TRUE)
rev(sort(x))

(index <- order(x))
x[index]

X <- data.frame(x = c(2, 2, 1, 1), y = c(1, 2, 2, 1))
(temp <- order(X[,1], X[,2]))
X[temp, ]

rank(x)

duplicated(x)
unique(x)

x <- c(3, 5, 7, 6)
diff(range(x))
summary(x)
cumsum(x)
cumprod(x)

choose(6, 4)
factorial(6) / (factorial(4) * factorial(2))
gamma(7) / (gamma(5) * gamma(3))

set.seed(123)                           # Initialisiert den Zufallszahlengenerator
x <- rnorm(10)                          # Erzeugt 10 standard-normalvert. Zufallsz.
(xd <- cut(x, breaks = c(-Inf, -2, -0.5, 0.5, 2, Inf)))
table(xd)



#############################################
### 7.2 Zufallszahlen

set.seed(1234)                          # Startwert definieren
rnorm(2)                                # Ergebnis A
rnorm(2)                                # Ergebnis B
set.seed(1234)                          # Startwert re-definieren
rnorm(2)                                # wieder Ergebnis A
RNGkind("Wichmann-Hill")                # anderen Generator wählen
set.seed(1234)                          # Startwert re-definieren
rnorm(2)                                # nicht Ergebnis A (anderer Generator!)



#############################################
### 7.3 Verteilungen und Stichproben

set.seed(123)
# 5 Pseudo-Zufallszahlen einer R[3,5]-Verteilung:
runif(5, min = 3, max = 5)
# 0.25-Quantil der R[3,5]-Verteilung:
qunif(0.25, min = 3, max = 5)
# Verteilungsfunktion an der Stelle 0 der N(0,1)-Verteilung:
pnorm(0, mean = 0, sd = 1)

set.seed(54321)
# Stichprobe aus den Zahlen 1:10 der Größe 4:
sample(1:10, 4)                         # ohne Zurücklegen
sample(1:10, 4, replace = TRUE)         #  mit Zurücklegen
sample(letters, 5)                      # geht mit beliebigen Objekten



#############################################
### 7.5 Lineare Modelle

data(anscombe)
lm1 <- lm(y1 ~ x1, data = anscombe)
lm2 <- lm(y2 ~ x2, data = anscombe)
lm3 <- lm(y3 ~ x3, data = anscombe)
lm4 <- lm(y4 ~ x4, data = anscombe)

lm1                                     # grobe Information zum Objekt lm1
coef(lm1)                               # geschätzte Koeffizienten
summary(lm1)                            # detaillierte Information zu lm1

with(anscombe, plot(x1, y1))
abline(lm1)

rbind(res = residuals(lm1), rsta = rstandard(lm1),
      rstu = rstudent(lm1))

anscombe$y1 - fitted(lm1)               # Residuen explizit berechnen
                                        
plot(lm1)                               # Grafiken für die Residualanalyse

lm1u <- update(lm1, log(.) ~ . - 1)

formula(lm1u)
lm1u

data(InsectSprays)
str(InsectSprays)
aovobj <- aov(count ~ spray, data = InsectSprays)
summary(aovobj)
model.matrix(aovobj)                    # Design-Matrix (Ausgabe gekürzt!)



#############################################
### 7.6 Überblick: Weitere spezielle Verfahren

data(iris)
set.seed(123)
index <- sample(nrow(iris))
train <- iris[-index[1:15], ]
test  <- iris[ index[1:15], ]

library("MASS")
library("rpart")
ldao <- lda  (Species ~ ., data = train)
rpo  <- rpart(Species ~ ., data = train)

ldap <- predict(ldao, newdata = test)$class
rpp  <- predict(rpo,  newdata = test, type = "class")
mean(ldap != test$Species)
mean(rpp  != test$Species)
