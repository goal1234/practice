###   Kapitel 2:  Grundlagen   ###

#############################################
### 2.1 R als Taschenrechner

1 + 2 * 3
2 * 5^2 - 10 * 5                        #  ein Kommentar
4 * sin(pi / 2)                         
0 / 0                                   #  nicht definiert (Not a Number)



#############################################
### 2.2 Zuweisungen

x1 <- 3.25                              # dem Objekt x1 wird die Zahl 3.25 zugewiesen
x1                                      # schreibt den Wert von x1 auf die Konsole
x2 = 3.25                               # neu, aber nur eingeschränkt zu empfehlen
6 -> x3                                 # schlecht lesbar und verwirrend

x1 <- 7
x1
print(x1)
(x1 <- 3.25)

neueVariable1 <- x1
neueVariable2=neueVariable1+2#kaum lesbar!
neueVariable2 <- neueVariable1 + 2      # jetzt besser lesbar



#############################################
### 2.3 Objekte

(X <- matrix(1:6, 2))                   # Erzeugen einer Matrix X
mode(X)
attributes(X) 



#############################################
### 2.5 Beispielsitzung

## Beispiel 1:
data(iris)
attach(iris)
summary(iris)
species.n <- as.numeric(Species)
plot(iris, col = species.n)
hist(Petal.Length)
op <- par(mfrow = c(2, 2), mar=c(2,2,2,0) + .5, lend = 1)
for(i in 1:4){
    boxplot(iris[ ,i] ~ Species, main = colnames(iris)[i])
}
par(op)

library("rpart")
(rpo <- rpart(Species ~ ., data = iris))
plot(rpo, margin = 0.1, branch = 0.5)
text(rpo)

library("MASS")
(ldao <- lda(Species ~ ., data = iris))
plot(ldao, abbrev = TRUE, col = species.n)

detach(iris)
ls()
rm(i, iris, ldao, op, rpo, species.n)


# Beispiel 2:
set.seed(123)
x <- rnorm(1000)
y <- x + rnorm(x, sd = 0.5)
plot(x, y)
(lmo <- lm(y ~ x))
summary(lmo)
abline(lmo, lwd = 2)
plot(lmo)                               # Für jedes weitere Bild bestätigen

hist(x, freq = FALSE)
lines(density(x), lwd = 2, col = "red")

qqnorm(x)
qqline(x, lwd = 2, col = "blue")
# q()                                   # R beenden



#############################################
### 2.7 Logik und fehlende Werte

4 < 3
(3 + 1) != 3
-3<-2                                   # Fehler, "<-" ist eine Zuweisung!
-3 < -2                                 # das Leerzeichen fehlte zuvor!
                                        
(3 >= 2) && (4 == (3 + 1))              
                                        
FALSE && TRUE                           
TRUE && FALSE                           
TRUE || FALSE                           
x <- 0                                  
TRUE || (x <- 3)                        
x                                       
FALSE || TRUE                           
FALSE || (x <- 3)                       
x                                       
                                        
c(TRUE, TRUE)   & c(FALSE, TRUE)        # vektorwertig
c(TRUE, TRUE)  && c(FALSE, TRUE)        # nicht vektorwertig
c(FALSE, FALSE) | c(FALSE, TRUE)        # vektorwertig
                                        
T & F                                   
T & T                                   
T <- 0                                  
T & T                                   # Achtung: T wurde auf 0 gesetzt und damit FALSE
                                    
FALSE + TRUE + FALSE                
TRUE + TRUE + FALSE                 
TRUE - FALSE - TRUE                 
                                    
x <- c(-3, 5, 2, 0, -2)             
x < 0                               
sum(x < 0)                          
                                    
x <- c(2, 9, 3, 1, 6)                   # ein einfacher Vektor
einige <- x > 4                         
alle <- x > 0                           
any(einige)                             # Ist "einige" min. einmal TRUE?
any(alle)                               # Ist "alle" min. einmal TRUE?
any(!alle)                              # Ist die Negation von "alle" TRUE?
all(einige)                             # Ist "einige" immer TRUE?
all(alle)                               # Ist "alle" immer TRUE?
which(einige)                           # An welchen Stellen ist "einige" TRUE?
which.max(x)                            # An welcher Stelle ist das Max. von "x"?
                                        
x <- NA                                 
str(x)                                  
y <- c(3, x)                            
str(y)                                  
                                        
x <- c(5, 7, NA, 22)                    
is.na(x)                                
is.na(x[1]) <- TRUE                     # Setze 1. Element von x auf NA
x

mean(x)
mean(x, na.rm = TRUE)
na.omit(x)



#############################################
### 2.8 Datentypen

(x <- pi)
mode(x)
typeof(x)
(y <- as.integer(x))                    # Informationsverlust!
typeof(y)
is.character(y)
x <- -1
# Für eine reelle Zahl x=-1 ist die Quadratwurzel von x nicht definiert:
sqrt(x)                   
sqrt(as.complex(x))   

(trt <- factor(rep(c("Control", "Treated"), c(3, 4))))
str(trt)
mode(trt)
length(trt)



#############################################
### 2.9 Datenstrukturen und deren Behandlung

## 2.9.1 Vektoren

(x <- c(4.1, 5.0, 6.35))
(x <- c(7.9, x, 2))
(y <- c("Hallo", "Leser"))
(y <- c("Hallo", TRUE, x))
length(y)

c(WertA = 5, WertB = 99)

3:10
6:-2

seq(3, -2, by = -0.5)
x <- c(5, 7)
seq(along = x)

rep(3, 12)
rep(c(5, 7), 2)
rep(3:5, 1:3)                           # 1x3, 2x4, 3x5
rep(TRUE, 3)                            
                                        
(x <- c(4.1, 5.0, 6.35) * 2)            # 4.1*2, 5.0*2, 6.35*2
x + 5:7                                 # 8.2+5, 10.0+6, 12.7+7
3:5 - 1:6                               # 3-1, 4-2, 5-3, 3-4, 4-5, 5-6
                                        
3:5 - 2:3
                                        
t(2:4) %*% 1:3                          
                                        
2:4 %*% t(1:3)                          
                                        
x <- c(3, 6, 9, 8, 4, 1, 2)             
length(x)                               
x[3]                                    # das dritte Element
x[c(4, 2)]                              # das 4. und 2. Element (Reihenfolge!)
x[-c(2, 3, 7)]                          # die Elemente 2, 3, 7 ausschließen
(logik.vektor <- x < 4)                 # TRUE, wenn x < 4, sonst FALSE:
x[logik.vektor]                         # alle x, die kleiner als 4 sind
x[x < 4]                                # das Gleiche direkt
y <- c(Wasser = 3, Limo = 5, Cola = 2)
y["Cola"]
y["Cola"] <- 99                         # das Element "Cola" ersetzen
y                                       
x[9:10] <- 10:9                         # das 9. und 10. Element zuweisen
x                                       # Element 8 existierte noch nicht: NA
x[] <- 2                                # alle 10 Elemente ersetzen
x                                       
(x <- 2)                                # x überschreiben (kein "[]")


## 2.9.2 Matrizen

(Z <- matrix(c(4, 7, 3, 8, 9, 2), ncol = 3))
(Y <- matrix(c(4, 7, 3, 8, 9, 2), nrow = 3, byrow = TRUE))
all(t(Z) == Y)                          # Sind alle Elemente von t(Z) und Y gleich?
(A <- matrix(c(4, 7, 0, 5), nrow = 2))
(Ainv <- solve(A))                      # Invertierung von A
Ainv %*% A

(X <- matrix(c(4, 7, 3, 8, 9, 2), nrow = 2))
X[1, 2]
X[2, ]                                  # 2. Zeile
X[, 3]                                  # 3. Spalte
                                        
(X <- matrix(1:4, 2))                   # eine 2x2 Matrix
X[1, 1]                                 # nur noch ein Vektor (1D)
X[1, 1, drop = FALSE]                   # ohne Verlust von Dimensionen (2D)
                                        
diag(X)                                 # Fortsetzung des letzten Beispiels
diag(X) <- 0                            # Hauptdiagonale Null setzen
X                                       
diag(2)                                 # 2x2 Einheitsmatrix (quadratisch!)

(X <- matrix(c(4, 7, 3, 8, 9, 2), nrow = 2))
str(X)

Y <- c(4, 7, 3, 8, 9, 2)
dim(Y) <- c(2, 3)
str(Y)


## 2.9.3 Arrays

(A <- array(1:12, dim = c(2, 3, 2)))
A[2, 2, 2]


## 2.9.4 Listen

(L1 <- list(c(1, 2, 5, 4), matrix(1:4, 2), c("Hallo", "Welt")))
L1[[1]]                                 # 1. Element von L1
L1[[2]][2, 1]                           # Element [2, 1] des 2. Elements von L1
# Rekursiv: zunächst 3. Element von L1, dann davon das 2.:
L1[[c(3,2)]]
                                        
(L2 <- list(Info = "R Buch", Liste1 = L1))
L2[["Info"]]
L2$Info

# L2[[2]] ist identisch zu L1, man erhält also die Elemente 1 und 3 der Liste L1.
L2[[2]][c(1, 3)]                


## 2.9.5 Datensätze - data frames

Einkaufen <- data.frame(Produkt = c("Apfelsaft", "Quark",
        "Joghurt", "Schinken", "Wasser", "Wurst", "Bier"),
    Abteilung = c("Getränke", "Milchprod.", "Milchprod.",
        "Fleischw.", "Getränke", "Fleischw.", "Getränke"),
    Menge = c(4, 2, 2, 1, 3, 1, 2))
Einkaufen                               # so wird es übersichtlicher
str(Einkaufen)                          # Struktur des data frame
                                        
Einkaufen$Menge[2]                      # ... ist das Gleiche wie Einkaufen[[3]][2]
Einkaufen[2,3]                          # und nochmal das selbe Element
                                        
Einkaufen$Menge[2]                      
                                        
attach(Einkaufen)                       
Menge[2]                                
                                        
Menge[2] <- 7                           
Menge[2]                                
Einkaufen$Menge[2]                      # Achtung, hat sich nicht geändert
detach(Einkaufen)

with(Einkaufen, rep(Produkt, Menge))

Einkaufen[Einkaufen[["Abteilung"]] == "Fleischw.", ]

subset(Einkaufen, Abteilung == "Fleischw.")

subset(Einkaufen, Abteilung == "Fleischw.", select = -2)

subset(Einkaufen, Abteilung %in% c("Getränke", "Milchprod."))

split(Einkaufen, Einkaufen$Abteilung)

Markenzuordnung <-
  data.frame(Produkt = c("Quark", "Joghurt", "Wasser", "Limo"),
      Marke = c("R-Milch", "R-Milch", "R-Wasser", "R-Wasser"))
merge(Einkaufen, Markenzuordnung, all.x = TRUE)


## 2.9.6 Objekte für formale S4 Klassen

setClass("neu", representation(x = "numeric", y = "numeric"))
(n1 <- new("neu", x = c(5, 7), y = 1:10))
n1[1]; n1$x; n1[[1]]                    # Kein "herkömmlicher" Zugriff klappt
n1@x                                    # der @-Operator hilft
                                        


#############################################
### 2.10 Konstrukte

## 2.10.1 Bedingte Anweisungen

x <- 5
if(x == 5){                             # falls x == 5 ist:
    x <- x + 1                          #   x um eins erhöhen und
    y <- 3                              #   y auf drei setzen
} else                                  # sonst:
    y <- 7                              #   y auf sieben setzen
c(x = x, y = y)                         # x wurde um eins erhöht und y ist gleich 3
if(x < 99) print("x ist kleiner als 99")

ifelse(x == c(5, 6), c("A1", "A2"), c("A3", "A4"))

switch(2, a=11, b=12, cc=13, d=14)      # Gibt das 2. Objekt in
switch("c", a=11, b=12, cc=13, d=14)    # Kein Objektname passt
switch("cc", a=11, b=12, cc=13, d=14)   # Gibt Objekt "cc" aus


## 2.10.2 Schleifen

i <- 0
repeat{
  i <- i + 1                            # addiere 1 zu i
  if(i == 3) break                      # stoppe, falls i = 3 ist
}                                       
i                                       
                                        
i <- 0                                  
repeat{                                 
  i <- i + 1                            # addiere 1 zu i
  if(i < 3) next                        # springe zum Anfang, falls i < 3
  print(i)                              # gibt aktuelles i aus
  if(i == 3) break                      # stoppe, falls i = 3 ist
}                                       
                                        
i <- 0                                  
while(i < 3)                            
  i <- i + 1                            # Solange i < 3 ist, erhöhe i um 1
i                                       
                                        
x <- c(3, 6, 4, 8, 0)                   # Vektor der Länge 5 (=length(x))
for(i in x)                             # i nimmt nacheinander die Werte von x an
    print(i^2)                          # Ausgabe auf Konsole
                                        
for(i in seq(along = x))                
    print(x[i]^2)                       # für alle i im Vektor seq(along=x)



#############################################
### 2.11 Zeichenketten

x <- 8.25
cat("Das Objekt x hat den Wert:", x, "\n", sep = "\t")

paste("Datei", 1:3, ".txt", sep = "")

x <- "Hermann Müller"
strsplit(x, " ")

x <- "Hermann Müller"
y <- "Hans_Meier"

grep("Hans", c(x, y))                   # "Hans" ist im 2. Element
sub("ü", "ue", c(x, y))                 # Ersetze "ü" durch "ue"
nchar(x)                                # Wie viele Zeichen hat x?
toupper(x)                              # Bitte alles Großbuchstaben
(ep <- parse(text = "z <- 5"))          # Zeichenfolge -> expression
eval(ep)                                # ep kann man jetzt auswerten

"Eine 'Zeichenkette' in einer Zeichenkette"
"Eine \"Zeichenkette\" in einer Zeichenkette"
'Eine "Zeichenkette" in einer Zeichenkette'
'Eine \'Zeichenkette\' in einer Zeichenkette'

`1x / 5y` <- 3
`1x / 5y` - 2



#############################################
### 2.12 Datum und Zeit

dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92")
times <- c("23:03:20", "22:29:56", "01:03:30", "18:21:03")
x <- paste(dates, times)
(z <- strptime(x, "%m/%d/%y %H:%M:%S"))

z[1] - z[2]

as.Date(z)
