###   Kapitel 6:  Objektorientiertes Programmieren   ###

#############################################
### 6.1 OOP mit S3-Methoden und -Klassen

data(anscombe)
class(anscombe$x1)
print(anscombe$x1)                      # analog zur Eingabe von 'anscombe$x1'
ansreg1 <- lm(y1 ~ x1, data = anscombe)
class(ansreg1)
print(ansreg1)                          # analog zur Eingabe von 'ansreg1'

methods(print)

plot(anscombe$x1)
plot(ansreg1)
summary(anscombe$x1)
summary(ansreg1)

Q <- 1                                  # beliebiges Objekt Q
class(Q) <- "quit"                      # Q hat jetzt die Klasse "quit"
print.quit <- function(x)               # Eine Methode für print() zur Klasse
    q("no")                             #   "quit"
# Achtung, durch Eingabe von  
#    Q                         
# wird R jetzt geschlossen ...



#############################################
### 6.2 OOP mit S4-Methoden und -Klassen     

## 6.2.1 Beispiel: Eine Klasse Wave und Methoden   

setClass("Wave",
    representation = representation(left = "numeric",
        right = "numeric", stereo = "logical",
        samp.rate = "numeric", bit = "numeric"),
    prototype = prototype(stereo = TRUE, samp.rate = 44100,
        bit = 16))

(waveobj <- new("Wave"))                # Erzeugen eines Wave Objekts

setValidity("Wave", 
function(object){
    if(!is(object@left, "numeric")) return("channels of Wave objects must be numeric")
    if(!(is(object@stereo, "logical") && (length(object@stereo) < 2)))
        return("slot 'stereo' of a Wave object must be a logical of length 1")
    if(object@stereo){
        if(!is(object@right, "numeric"))
            return("channels of Wave objects must be numeric")
        if(length(object@left) != length(object@right))
            return("both channels of Wave objects must have the same length")
    }
    else if(length(object@right))
        return("'right' channel of a wave object is not supposed to contain data if slot stereo==FALSE")
    if(!(is(object@samp.rate, "numeric") &&
        (length(object@samp.rate) < 2) && (object@samp.rate > 0)))
            return("slot 'samp.rate' of a Wave object must be a positive numeric of length 1")
    if(!(is(object@bit, "numeric") &&
        (length(object@bit) < 2) && (object@bit %in% c(8, 16))))
            return("slot 'bit' of a Wave object must be a positive numeric (either 8 or 16) of length 1")
    return(TRUE)
})

waveobj@stereo <- "Ja"                  # ungültig, leicht editierte Ausgabe

waveobj@samp.rate <- -1000
validObject(waveobj)
                                                         
setClass("Song",
    representation = representation(text = "character"),
    contains = "Wave")
new("Song")

setMethod("show", signature(object = "Wave"),
    function(object){
        l <- length(object@left)
        cat("\nWave Object")
        cat("\n\tNumber of Samples:     ", l)
        cat("\n\tDuration (seconds):    ",
            round(l / object@samp.rate, 2))
        cat("\n\tSamplingrate (Hertz):  ", object@samp.rate)
        cat("\n\tChannels (Mono/Stereo):",
            if(object@stereo) "Stereo" else "Mono")
        cat("\n\tBit (8/16):            ", object@bit, "\n\n")
    }
)

show(waveobj)
# analog: "print(waveobj)" oder einfach "waveobj"
