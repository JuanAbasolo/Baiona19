## Azalpena ---------------------------
##
## Script name: Bourciez MDS maparatua
##
## Purpose of script:
##
## Author: Juan
##
## Date Created: 2019-05-17
##
## Email: juan.abasolo@ehu.eus
##
## Oharrak ---------------------------
##
## Notes:
##   
##
## Beharrezko paketeak ---------------------------
require(reshape2)
require(diaMeasures)
require(plot3D)
require(scales)
require(scatterplot3d)
require(rgdal)
require(sp)
library(RColorBrewer)

## Datuak sartu ---------------------------

## Datuak sartu
dtk.brcz <- read.table('data/raw/bourciez-taulazabalduan-b.csv', sep = ',', header = T)
names(dtk.brcz) <- read.table('data/raw/bourciez-taulazabalduan-b.csv', sep = ',', stringsAsFactors = F)[1,]
row.names(dtk.brcz) <- dtk.brcz[,1] 
dtk.brcz <- dtk.brcz[,-1]
# NA gehiegi daukaz Hendaiak
dtk.brcz <- dtk.brcz[-which(row.names(dtk.brcz)=="Hendaia"),]

## Mapa Sartu
mapi <- readOGR('data/maps/EH_Udalerriak-Barri/udalerriak_EH_berriaB.shp')

## Distantzia matrizea sortu---------------------------

## Datuen prestaketa
dtk.tnp <- cbind(herria = row.names(dtk.brcz), dtk.brcz)
names.bcrz <- names(dtk.tnp)

dtk.bcrz.luze <- melt(dtk.tnp, id.vars = 'herria', variable.name = 'erantzunak')

## Distantzia
d.brcz <- as.dist(diaMeasures::diaMeasure(data = dtk.bcrz.luze, 
                                  formula = herria~erantzunak, 
                                  value.var = 'value',
                                  measure = 'iri',
                                  binaryIndex = 'dice'))
## Erakutsi
knitr::kable(as.matrix(d.brcz)[1:10,1:10], digits = 2)

## MDS
mds.bcrz <- cmdscale(d.brcz, k = 3)

## Koloreak
col.mds <- rgb(rescale(1-mds.bcrz[,], to = c(0, 1)))

## Irudiak ---------------------------
## 2D
plot(mds.bcrz[,1:2], col = col.mds, pch = 20, cex = 2.5)
text(mds.bcrz[,1:2], labels = row.names(mds.bcrz), col = 'gray40', cex = 0.7)

## 3D
scatter3D(mds.bcrz[,1], mds.bcrz[,2], mds.bcrz[,3], 
          type = 'h', 
          pch = 20, 
          cex = 1.5, 
          bty = "f",
          col = 1,
          theta = 35, 
          phi = 20
)
text3D(mds.bcrz[,1], mds.bcrz[,2], mds.bcrz[,3],
       labels = row.names(mds.bcrz),
       col = col.mds,
       cex = 0.7,
       add = T)
title(main = 'Bourciezen datuen esklatze multidimentsionala (MDS)')

## Mapak ---------------------------
##         
## Maparatuta:
df <- data.frame(mds.bcrz)
df$col <- col.mds

mapi@data$col.mds <- 0


for(i in row.names(df)){
        # print(i)
        mapi@data[mapi@data$IZ_EUSKAL==i, "col.mds"] <- df[i, 'col']
}

plot(mapi, col = mapi@data$col.mds,
     main = 'Bourciezen datuen esklatze multidimentsionala (MDS) maparatuta')
legend("bottomleft",
       legend = c('1. dim', '2. dim', '3. dim'),
       title = 'MDS dimentsioak',
       col = c(2:4),
       pch = 20)

## Paleta ---------------------------
##       Paletik 17 maila bihar leukez gitxienez, hamen gitxio erabilikotez.
display.brewer.all()

cols <- brewer.pal(12, 'Paired')


## Aldagai bat maparatu ---------------------------

# ##         aimar
# mapi@data$aimer <- NA
# 
# for(i in row.names(dtk.brcz)){
#         # print(i)
#         mapi@data[mapi@data$IZ_EUSKAL==i, 'aimer'] <- dtk.brcz[i, "aimer"]
# }
# 
# 
# plot(mapi, col=cols[as.numeric(mapi@data$aimer)],
#      main = 'Frantsesezko aimer emateko moduak Bourciezen datuetan')
# legend("bottomleft",
#               legend = levels(mapi@data$aimer),
#               title = 'Euskarazko formak',
#               col = cols[1:length(levels(mapi@data$aimer))],
#               pch = 20)
# 
# ## Vin
# 
# mapi@data$vin <- NA
# 
# for(i in row.names(dtk.brcz)){
#         # print(i)
#         mapi@data[mapi@data$IZ_EUSKAL==i, 'vin'] <- dtk.brcz[i, "vin"]
# }
# 
# 
# 
# plot(mapi, col=cols[as.numeric(mapi@data$vin)],
#      main = 'Frantsesezko vin emateko moduak Bourciezen datuetan')
# mapi@data$vin <- factor(mapi@data$vin, levels = levels(dtk.brcz$vin))
# legend("bottomleft",
#        legend = levels(mapi@data$vin),
#        title = 'Euskarazko formak',
#        col = cols[1:length(levels(mapi@data$vin))],
#        pch = 20)
# 
# ## baiser
# 
# mapi@data$baiser <- NA
# 
# for(i in row.names(dtk.brcz)){
#         # print(i)
#         mapi@data[mapi@data$IZ_EUSKAL==i, 'baiser'] <- dtk.brcz[i, "baiser"]
# }
# 
# # 
# 
# plot(mapi, col=cols[as.numeric(mapi@data$baiser)],
#      main = 'Frantsesezko baiser emateko moduak Bourciezen datuetan')
# mapi@data$baiser <- factor(mapi@data$baiser, levels = levels(dtk.brcz$baiser))
# legend("bottomleft",
#        legend = levels(mapi@data$baiser),
#        title = 'Euskarazko formak',
#        col = cols[1:length(levels(mapi@data$baiser))],
#        pch = 20)
# 
# ## buit
# mapi@data$bruit <- NA
# 
# for(i in row.names(dtk.brcz)){
#         # print(i)
#         mapi@data[mapi@data$IZ_EUSKAL==i, 'bruit'] <- dtk.brcz[i, "bruit"]
# }
# 
# # 
# 
# plot(mapi, col=cols[as.numeric(mapi@data$bruit)],
#      main = 'Frantsesezko bruit emateko moduak Bourciezen datuetan')
# mapi@data$bruit <- factor(mapi@data$bruit, levels = levels(dtk.brcz$bruit))
# legend("bottomleft",
#        legend = levels(mapi@data$bruit),
#        title = 'Euskarazko formak',
#        col = cols[1:length(levels(mapi@data$bruit))],
#        pch = 20)
# 
# ## Voler
# 
# mapi@data$voler <- NA
# 
# for(i in row.names(dtk.brcz)){
#         # print(i)
#         mapi@data[mapi@data$IZ_EUSKAL==i, 'voler'] <- dtk.brcz[i, "voler"]
# }
# 
# # 
# 
# plot(mapi, col=cols[as.numeric(mapi@data$voler)],
#      main = 'Frantsesezko voler emateko moduak Bourciezen datuetan')
# mapi@data$voler <- factor(mapi@data$voler, levels = levels(dtk.brcz$voler))
# legend("bottomleft",
#        legend = levels(mapi@data$voler),
#        title = 'Euskarazko formak',
#        col = cols[1:length(levels(mapi@data$voler))],
#        pch = 20)
# 

## Voler

mapi@data$voler <- NA

for(i in row.names(dtk.brcz)){
        # print(i)
        mapi@data[mapi@data$IZ_EUSKAL==i, 'voler'] <- dtk.brcz[i, "voler"]
}

# 

plot(mapi, col=cols[as.numeric(mapi@data$voler)],
     main = 'Frantsesezko voler emateko moduak Bourciezen datuetan')
mapi@data$voler <- factor(mapi@data$voler, levels = levels(dtk.brcz$voler))
legend("bottomleft",
       legend = levels(mapi@data$voler),
       title = 'Euskarazko formak',
       col = cols[1:length(levels(mapi@data$voler))],
       pch = 20)



## AURREKOA FUNTZINORA PASAU    ---------------------------
##

## Mailak kontau, 12 baino gehixago ala gitxiago
itema <- '\"Inor\" izenordain zehaztugabea'
mailak <- levels(dtk.brcz[, itema])

laburtu <- function(mailak){
        mailak <- c(mailak[1:11], 'beste batzuk')
        print(mailak)
}

mapatxu <- function(itema){
        
        mailak <- levels(dtk.brcz[, itema])
        mailak <- if(length(mailak)>12) laburtu(mailak) else mailak
        mailak <- if(length(mailak)<3)c(mailak, 1, 2) else mailak
        cols <- brewer.pal(length(mailak), 'Paired')
        mapi@data[, itema] <- NA
        
        for(i in row.names(dtk.brcz)){
                # print(i)
                mapi@data[mapi@data$IZ_EUSKAL==i, itema] <- dtk.brcz[i, itema]
        }
        
        plot(mapi, col=ifelse(is.na(as.numeric(mapi@data[, itema])), 1, cols[as.numeric(mapi@data[, itema])]),
             main = paste(itema,  'emateko moduak Bourciezen datuetan'))
        mapi@data[, itema] <- factor(mapi@data[, itema], levels = levels(dtk.brcz[, itema]))
        legend("bottomleft",
               legend = levels(mapi@data[, itema][1:length(mailak)]),
               title = 'Euskarazko formak',
               col = cols[1:length(levels(mapi@data[, itema]))],
               pch = 20)
}


## Maparatu
mapatxu(names(dtk.brcz)[9])


for(i in names(dtk.brcz[,1:240]))mapatxu(i)
        
## Mapa finxiaguak ---------------------------
##         Mapotako ahalegina: 6 aldakirik ugarienak + bestelakoak pintau

## Kolorik: Dark2

itema.1 <- '\"Inor\" izenordain zehaztugabea'
itema.1 <- '\"Anitz\" zenbatzaile zehaztugabea'


mailak <- levels(dtk.brcz[, itema.1])

## mailak 8 aldaki baino gehiago badauko
if(length(mailak)>8){
        ## konta aldakien maiztasuna
        erakustekuak <- sort(table(dtk.brcz[, itema.1]), decreasing = TRUE)
        erakustekuak <- erakustekuak[erakustekuak!=min(erakustekuak)]
        if(length(erakustekuak)>8){
                erakustekuak <- sort(table(dtk.brcz[, itema.1]), decreasing = TRUE)
                erakustekuak <- erakustekuak[erakustekuak!=min(erakustekuak)]
        }
}
mailak <- attributes(erakustekuak)$dimnames


## eta aukeratu 7 ugarienak

## besteai 'beste batzuk' etiketa gehitu

laburtu <- function(mailak){
        mailak <- c(mailak[1:11], 'beste batzuk')
        print(mailak)
}

addLevel <- function(x, newlevel=NULL) {
        if(is.factor(x)) {
                if (is.na(match(newlevel, levels(x))))
                        return(factor(x, levels=c(levels(x), newlevel)))
        }
        return(x)
}

mapatxu.v1.0 <- function(itema.1){
        aldagaijja <- dtk.brcz[, itema.1]
        mailak <- levels(aldagaijja)
        if(length(mailak)>8){
                ## konta aldakien maiztasuna
                erakustekuak <- sort(table(aldagaijja), decreasing = TRUE)
                erakustekuak <- erakustekuak[erakustekuak!=min(erakustekuak)]
                if(length(erakustekuak)>8){
                        erakustekuak <- sort(table(aldagaijja), decreasing = TRUE)
                        erakustekuak <- erakustekuak[erakustekuak!=min(erakustekuak)]
                }
        }
        
        mailak <- attributes(erakustekuak)$dimnames[[1]]

        cols <- brewer.pal(length(mailak), 'Dark2')
        mapi@data[, itema.1] <- NA
        levels(aldagaijja) <- c(levels(aldagaijja), 'bestelakoak')
        aldagaijja[which(!is.element(aldagaijja, mailak)&!is.na(aldagaijja))] <- 'bestelakoak'
        # aldagaijja <- as.character(aldagaijja)
        for(i in row.names(dtk.brcz)){
                # print(i)
                # print(aldagaijja[dtk.brcz[i, itema.1]])
                mapi@data[mapi@data$IZ_EUSKAL==i, itema.1] <- aldagaijja[row.names(dtk.brcz)==i]
        }
        
        mapi@data[, itema.1] <- as.factor(mapi@data[,itema.1])
        plot(mapi, col=ifelse(is.na(as.numeric(mapi@data[, itema.1])), 1, cols[as.numeric(mapi@data[, itema.1])]),
             main = paste(itema.1,  'emateko moduak Bourciezen datuetan'))
        mapi@data[, itema.1] <- factor(mapi@data[, itema.1], levels = mailak)
        legend("bottomleft",
               legend = levels(mapi@data[, itema.1][1:length(mailak)]),
               title = 'Formak',
               col = cols[1:length(levels(mapi@data[, itema.1]))],
               pch = 20)
}

mapatxu.v1.0('\"Anitz\" zenbatzaile zehaztugabea')

for(j in names(dtk.brcz)[50:60]) mapatxu.v1.0(j)
        


