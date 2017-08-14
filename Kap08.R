###   Kapitel 8:  Grafik   ###

#############################################
### 8.1 Konventionelle Grafik

## 8.1.2 High-level Grafik

data(iris)                              # Lade Beispieldatensatz
attach(iris)                            # Daten in den Suchpfad einhängen
plot(Petal.Length, Petal.Width, pch = as.numeric(Species))
hist(Petal.Length)
detach(iris)

x <- c(0, 10)
y <- c(0, 20)
(z <- outer(x, y))
image(x, y, z, col = c("transparent", "black"))

# install.packages("scatterplot3d") # Paket bei Bedarf installieren
library("scatterplot3d")                # ... und laden
data(trees)                             # Beispieldaten laden
s3d <- scatterplot3d(trees, type = "h", angle = 55,
                     scale.y = 0.7, pch = 16, main = "trees")
my.lm <- with(trees, lm(Volume ~ Girth + Height))
s3d$plane3d(my.lm, lty.box = "solid")   # Regressionsebene zeichnen


## 8.1.3 Transparenz

set.seed(321)
x <- c(rnorm(2000), (a <- rnorm(200, sd = 0.5)))
y <- c(rnorm(2000), a)
pdf("Transparenz.pdf", version = "1.4")
plot(x, y, col = rgb(0, 0, 0, alpha = 0.1), pch = 16)
dev.off()

 
 
## 8.1.5 Mathematische Beschriftung

plot(1:10, sub = expression(y[i] == beta[0] + beta[1] * x[i] + e))

wert <- 0.5
substitute(sigma == s, list(s = wert))



## 8.1.3 - 8.1.5 Beispiel:

par(mar = c(4,4,2,0) + .5)
set.seed(123)
x <- rnorm(100)                         # 100 N(0,1)-verteilte Zufallszahlen
par(las = 1)
# Beschriftetes und manuell skaliertes Histogramm:
hist(x, main = "Dichte 100 N(0,1)-verteilter Zufallszahlen",
     freq = FALSE, col = "grey", ylab = "Dichte",
     xlim = c(-5, 5), ylim = c(0, 0.6))
# Hinzufügen der theor. Dichtefunktion - dick und gestrichelt
curve(dnorm, from = -5, to = 5, add = TRUE, lwd = 3, lty = 2)
legend(-4.5, 0.55, legend = c("emp. Dichte", "theor. Dichte"),
       col = c("grey", "black"), lwd = 5)
text(-5, 0.3, adj = 0, cex = 1.3,
    expression(f(x) == frac(1, sigma * sqrt(2*pi)) ~~
        e^{frac(-(x - mu)^2, 2 * sigma^2)}))
text(5, 0.3, adj = 1, cex = 1.3,
    expression("mit " * {mu == 0} * ", " * {sigma == 1}))    
    
        

#############################################
### 8.2 Trellis Grafiken mit lattice


## 8.2.2 Das Paket grid - mehr als nur Grundlage für lattice
library(grid)                     # Laden von "grid"
vp <- viewport(x = 0.5, y = 0.6, angle = 15,
               w = unit(4, "cm"), h = unit(2, "cm"))
grid.show.viewport(vp)            # Viewport vp visualisieren
pushViewport(vp)                  # Viewport auf den Stack legen
grid.text("Beispiel!", 0.6, 0.4, rot = -90)
popViewport()                     # einen Viewport zurück
grid.rect(gp = gpar(lwd = 3))     # äußerer Rahmen
    
    
## 8.2.3 Ausgabe von Trellis Grafiken - trellis.device()

library("lattice")

mein.trellis <- function(){
    xyplot(1 ~ 1)                       # hier wurde etwas vergessen!
    return(1)                           
}                                       
mein.trellis()                          # hier erscheint *keine* Grafik!

mein.trellis <- function(){
    print(xyplot(1 ~ 1))
    return(1)
}
mein.trellis()                          # jetzt erscheint eine Grafik!

trellisObjekt <- xyplot(1 ~ 1)
str(trellisObjekt)                      # Struktur des Trellis Objektes
# Hier könnte das Trellis Objekt manipuliert werden.
print(trellisObjekt)                    # Die Grafik wird jetzt gezeichnet


## 8.2.4 Formelinterface

data(iris)                              # Laden des Datensatzes
library("lattice")                      # Laden des Pakets lattice
# trellis.device("postscript", file = "lattice-beispiel.eps",
#                width = 8, height = 4, horizontal = FALSE)
xyplot(Petal.Length ~ Sepal.Length | Species, data = iris)
# dev.off()    

SW <- equal.count(iris$Sepal.Width, number = 2, overlap = 0.33)
xyplot(Petal.Length ~ Sepal.Length | Species * SW, data = iris)


## 8.2.5 Konfiguration und Erweiterbarkeit

mein.panel <- function(x, y){
    panel.xyplot(x, y)
    panel.lmline(x, y, lwd = 3)}

xyplot(Petal.Length ~ Sepal.Length | Species, data = iris,
       panel = mein.panel)
