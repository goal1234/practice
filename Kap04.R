###   Kapitel 4:  Die Sprache im Detail   ###

#############################################
### 4.1 Funktionen

## 4.1.1 Funktionsaufruf

a <- c(3, 1, 5, NA)
median(a)                               # 1. Möglichkeit
median(a, TRUE)                         # 2. Möglichkeit
median(na.rm = TRUE, x = a)             # 3. Möglichkeit
median(na = TRUE, a)                    # 4. Möglichkeit


## 4.1.2 Eigene Funktionen definieren

median <- function(x, na.rm = FALSE){
  if(mode(x) != "numeric")
    stop("need numeric data")
  if(na.rm)
    x <- x[!is.na(x)]
  else if (any(is.na(x)))
    return(NA)
  n <- length(x)
  if(n == 0)
    return(NA)
  half <- (n + 1)/2
  if(n %% 2 == 1){
    sort(x, partial = half)[half]
  } else{
    sum(sort(x, partial =
        c(half, half + 1))[c(half, half + 1)]) / 2
  }
}

punkte <- function(x, ...){
    matrix(x, ...)
}
a <- c(3, 5, 7, 2)                      # Es gibt kein Argument,
punkte(a)                               #  das dem "..." zugeordnet wird
punkte(a, ncol = 2, byrow = TRUE)       # ncol und byrow werden an matrix() weitergeleitet

Newton.Wurzel <- function(y, Startwert, eps = 10^(-7)){
    x <- Startwert                      # Mit Startwert beginnen
    repeat{
        x.neu <- x - (x^2 - y) / (2 * x)# Iterationsvorschrift
        if(abs(x - x.neu) < eps) break  # Abbruchkriterium
        x <- x.neu
    }
    return(x.neu)
}
Newton.Wurzel(4, 1)



#############################################
### 4.2 Verzögerte Auswertung - Lazy Evaluation

faul <- function(x, rechnen = TRUE) {
    if(rechnen) x <- x + 1
    print(a)
}
faul((a <- 3), rechnen = FALSE)         # (1)
faul(a <- 3)                            # (2)

aufruf <- function(x)
    return(list(Aufruf = substitute(x), Wert = x))
aufruf(1 + 2)



#############################################
### 4.3 Umgebungen und deren Regeln - Environments und Scoping Rules

search()

einfach <- function(x){
    med <- median(x)
    return(list(x = x, median = med))
}
einfach(1:3)

x <- 5
scope <- function(do.it = TRUE){
    if(do.it) x <- 3
    innen <- function()
        print(x)
    innen()
}
scope()
scope(FALSE)

l.scope <- function(){
    nur.hier <- "Ich bin nur hier definiert!"
    innen <- function() print(nur.hier)
    return(innen)
}

Ausgabe <- l.scope()
ls()
Ausgabe
testen <- function() print(nur.hier)
testen()
Ausgabe()
ls(environment(Ausgabe))

beispiel <- function(x)
    sin(2 * pi * x)
beispiel(1:5)                           # erwartetes Ergebnis
sin <- sum                              
pi <- 0.5                               
beispiel(1:5)                           # Summe der Zahlen 1:5

zuweisen <- function(){
    a <- 1                              # lokale Zuweisung
    b <<- 2                             # Zuweisung in die .GlobalEnv
    assign("d", 3, pos = ".GlobalEnv")
    return(c(a, b, d))                  # a lokal; b und d aus dem Workspace
}
zuweisen()
print(a)                                # a ist nicht im Workspace
print(b)
print(d)

for(i in 1:3)
    assign(paste("Buchstabe", i, sep = ""), LETTERS[i])
ls()                                    # Objekte im Workspace
Buchstabe2
for(i in 3:1)
    print(get(paste("Buchstabe", i, sep = "")))



#############################################
### 4.4 Umgang mit Fehlern

## 4.4.1 Finden und Beseitigen von Fehlern - Debugging

foo2 <- function(x, s)
    x[[s]] + 5
foo1 <- function(x){ 
    y <- x + 1
    foo2(y, s = -5)
}

foo1(1:5)
traceback()

debug(foo2)
foo1(1:5)
# Im Browser:   ls()                    # Welche Objekte gibt es?
#               x                       # Die Definition von x ansehen
#               where                   # An welcher Stelle sind wir noch gerade?
#               n
undebug(foo2)                           # B

options(error = recover)
foo1(1:5)
# In Selection: 2
# Im Browser:   Q

options(error = dump.frames)
foo1(1:5)
debugger()
# In Selection: 2
# Im Broswer:   ls()
#               Q


## 4.4.2 Fehlerbehandlung

foo2 <- function(x, s)
    x[[s]] + 5
foo1 <- function(x, s = 1){
    wert <- try(foo2(x = x, s = s))
    cat("Ich werde immer noch ausgeführt...\n")
    if(class(wert) == "try-error")
        wert <- "Ein Fehler ist aufgetreten"
    return(wert)
}

foo1(1:5, s = -5)                       # Fehlerfall
foo1(1:5)                               # Ohne Fehler



#############################################
### 4.5 Rekursion

fib.rek <- function(n){
    if(n == 1) return(0)
    else if(n == 2) return(1)
    else return(fib.rek(n - 1) + fib.rek(n - 2))
}
fib.rek(10)

fib.it <- function(n){
    FZ <- numeric(n)                    # x: Platz für alle Fib.-Z.
    FZ[1:2] <- 0:1                      # FZ[1] und FZ[2] laut Def.
    for(i in seq(along = FZ)[-(1:2)])   # FZ[1] und FZ[2] auslassen
        FZ[i] <- FZ[i-1] + FZ[i-2]      # laut Definition
    return(FZ[n])
}
fib.it(10)



#############################################
### 4.6 Umgang mit Sprachobjekten

round(12.3456, digits = 2)

(mycall <- call("round", 12.3456, digits = 2))
mode(mycall)
eval(mycall)

do.call("round", list(12.3456, digits = 2))

quote(round(12.3456, digits = 2))

(zeichen <- "round(12.3456, digits = 2)")
mode(zeichen)
(ausdruck <- parse(text = zeichen))
mode(ausdruck)
eval(ausdruck)

(myexpr <- expression(x <- 12.3456, round(x, digits = 2)))
mode(myexpr)
eval(myexpr)
(call1 <- myexpr[[1]])    # erstes Element (Aufruf) der Liste
mode(call1)
print(eval(call1))

deparse(call("round", 12.3456, digits = 2))

foo <- function(x)
    return(list(Aufruf = deparse(substitute(x)), Ergebnis = x))
foo(sin(pi / 2))

lambda <- pi / 2          # sei aus einer Berechnung
(mycall <- substitute(sin(lambda), list(lambda = lambda)))
mode(mycall)
eval(mycall)



#############################################
### 4.7 Vergleich von Objekten

x <- c(1, 2, 3)
y <- c("1", "2", "3")
x == y
all(x == y)
identical(x, y)
identical(x, as.numeric(y))

(X <- matrix(c(3, 4, 8, 7, 1, 8, 2, 9, 9), 3))

X %*% solve(X)

identical(diag(3), X %*% solve(X))
diag(3) == X %*% solve(X)

all.equal(diag(3), X %*% solve(X))
all.equal(3, 3.1)

isTRUE(all.equal(x, y), TRUE)
