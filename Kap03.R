###   Kapitel 3:  Ein- und Ausgabe von Daten   ###

#############################################
### 3.1 ASCII-Dateien

data(iris)
write.table(iris, file = "iris.txt")
x <- read.table("iris.txt")
write.table(iris, file = "iris2.txt", sep = "\t", dec = ",")
x <- read.table("iris2.txt", sep = "\t", dec = ",")
file.remove("iris.txt", "iris2.txt")            # Zuvor angelegte Dateien wieder l�schen



#############################################
### 3.5 Zugriff auf Datenbanken
         
## Hier m�ssen Voraussetzugen erf�llt sein (s. Buch) ...
library("RODBC")                                # das Paket laden
channel <- odbcConnect("Mining")                # Verbindung �ffnen
data(iris)                                      # Laden der iris Daten
# Datensatz "iris" in Tabelle "iristab" schreiben:
sqlSave(channel, iris, "iristab")
sqlTables(channel)                              # vorh. Tabelle(n) anzeigen
sqlQuery(channel, "select * from iristab")
sqlQuery(channel,
    "select * from iristab where Species = 'virginica' and PetalLength > 6")
close("channel")



#############################################
### 3.6 Zugriff auf Excel-Daten

## Hier m�ssen Voraussetzugen erf�llt sein (s. Buch) ...
library("RODBC")                                # das Paket laden
channel <- odbcConnectExcel("C:/irisdat.xls")   # Verbindung �ffnen
sqlTables(channel)                              # Name der Tabellen
sqlQuery(channel, "select * from \"iris$\"")    # Datensatz lesen
close(channel)
