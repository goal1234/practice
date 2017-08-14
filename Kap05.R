###   Kapitel 5:  Effizientes Programmieren   ###

#############################################
### 5.1 Programmierstil

wuerfel <- function(){
    x <- sample(1:10, 100, replace = TRUE)
    sum(x == 8)
}
wuerfel()                               # von Zufallszahlen abhängig!

wuerfel <- function(Seiten, N, Augenzahl){
    # Generieren von N Würfen eines Würfels mit "Seiten" Seiten:
    x <- sample(1:Seiten, N, replace = TRUE)
    # Zählen, wie oft die Augenzahl "Augenzahl" vorkommt:
    sum(x == Augenzahl)
}
wuerfel(10, 100, 8)                     # von Zufallszahlen abhängig!
wuerfel(6, 1000, 6)                     # von Zufallszahlen abhängig!

wuerfel<-function(){x<-sample(1:10,100,replace=TRUE)
sum(x==8)} # sehr schlecht lesbarer Code!



#############################################
### 5.2 Vektorwertiges Programmieren und Schleifen

## 5.2.2 Vektorwertiges Programmieren - mit apply() und Co

(X <- matrix(c(4, 7, 3, 8, 9, 2), nrow = 3))
apply(X, 1, max)
apply(X, 2, function(x) diff(range(x)))

spannweite <- function(x)
    diff(range(x))
apply(X, 2, spannweite)

data(anscombe)                          # Laden des Datensatzes
attach(anscombe)                        # Hinzufügen zum Suchpfad
anscombe                                # Datensatz anschauen

ans.reg <- vector(4, mode = "list")     # leere Liste erzeugen
# 4 Regressionen (y_i gegen x_i) in Liste speichern:
for(i in 1:4){
    x <- get(paste("x", i, sep = ""))
    y <- get(paste("y", i, sep = ""))
    ans.reg[[i]] <- lm(y ~ x)
}
# Datensatz wieder aus dem Suchpfad entfernen:
detach(anscombe)                        

lapply(ans.reg, coef)
sapply(ans.reg, coef)
  
(X <- matrix(1:4, 2))
"["(X, , 2)                             # a) - gleich b)
X[ , 2]

hist(replicate(1000, 
    mean(sample(1:6, 100, replace = TRUE))))

mapply(sum, 1:10, 10:1, 5)              # 1+10+5, 2+9+5, 3+8+5, ...

data(iris)                              # Datensatz laden
attach(iris)                            # Datensatz anhängen
tapply(Sepal.Length, Species, mean)
tapply(Sepal.Width, Species, range)
detach(iris)



#############################################
### 5.3 Hilfsmittel zur Effizienzanalyse

(pt1 <- proc.time())                    # Sitzung läuft seit ca. 5 Min. (300/6)
plot(1:10)                              # R etwas zu tun geben ...
(pt2 <- proc.time())                    
pt2 - pt1                               # wenige hundertstel Sek. wurden benötigt

Zeit1 <- function(n){                   # Beispiel 1: ganz schlecht
    a <- NULL
    for(i in 1:n) a <- c(a, i^2)
}
Zeit2 <- function(n){                   # Beispiel 2: etwas besser
    a <- numeric(n)                     
    for(i in 1:n) a[i] <- i^2           
}                                       
Zeit3 <- function(n){                   # Beispiel 3: viel besser
    a <- (1:n)^2                        
}                                       
system.time(Zeit1(30000))               
system.time(Zeit2(30000))               
system.time(Zeit3(30000))               
                                        
Zeit4 <- function(n){                   # Beispiel 4: ganz schlecht
    a <- numeric(n)
    for(i in 1:n) a[i] <- sin(2 * pi * i)
}
Zeit5 <- function(n){                   # Beispiel 5: etwas besser
    a <- numeric(n)
    for(i in 1:n) a[i] <- sin(i)
    a <- 2 * pi * a
}
Zeit6 <- function(n){                   # Beispiel 6: viel besser
    a <- sin(2 * pi * 1:n)
}
system.time(Zeit4(30000))
system.time(Zeit5(30000))
system.time(Zeit6(30000))

## Windows:
memory.limit()                          # Limit der Speichernutzung in Bytes
memory.limit(2047)                      # Setzt neues Limit (2 GB) in Megabytes
memory.limit()                          # Überprüfen ergibt richtiges Limit



## 5.3.1 Laufzeitanalyse - Profiling

Rprof(interval = 0.01)
example(glm)                            # Auszuwertender Code
Rprof(NULL)                             # Aufzeichnung für Profiling beenden
summaryRprof()                          # Aufzeichnung auswerten
