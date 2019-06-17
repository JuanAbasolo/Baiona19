# ---------------------------
##
## Script name: datak.bourciez.mapa.R
##
## Purpose of script:
##
## Author: Juan Abasolo
##
## Date Created: 2019-04-30
##
## Email: juan.abasolo@ehu.eus
##
## ---------------------------
##
## Oharrak: Hurrengo analisietarako 'dtk.brcz' eta 'mapi' ahalik eta egokien eukitzeko
##   
##
##
##
## ---------------------------

## BOURCIEZ sartu

# !!!!!!!! Arazua Jatsu biek emoten dabe. Originalean zuzendu behar da

dtk.brcz <- read.table('data/raw/bourciez-taulazabalduan-b.csv', sep = ',', header = T)
# Egokitu zutabeen izenak
names(dtk.brcz) <- read.table('data/raw/bourciez-taulazabalduan-b.csv', sep = ',', stringsAsFactors = F)[1,]

# Herrien euskarazko izenak lerroen izen
row.names(dtk.brcz) <- dtk.brcz[,1] 
# Ezabatu euskarazko izenen zutabea (lerroetan dagoz-eta)
dtk.brcz <- dtk.brcz[,-1]

# NA gehiegi daukaz Hendaiak
dtk.brcz <- dtk.brcz[-which(row.names(dtk.brcz)=="Hendaia"),]

## ---------------------------

## Mapa sartu

## load up the packages we will need:  (uncomment as required)

require(rgdal)
# require(sp)
# require(maptools)
# require(ggplot2)
mapi <- readOGR('data/maps/EH_Udalerriak-Barri/udalerriak_EH_berriaB.shp')
# mapi <- readOGR(dsn = 'data/maps/EH_Udalerriak-Barri', layer = 'udalerriak_EH_berriaB')
plot(mapi, lwd =0.2)

# Tue Apr 30 17:04:10 2019 ------------------------------

## Aprobak zer pintaten dan-ta ikusteko
mapi@data$col <- NA
for(i in row.names(dtk.brcz)){
        mapi@data[mapi@data$IZ_EUSKAL==i, "col"] <- 'black'
}
        
plot(mapi, col = mapi@data$col)

## Aztertu izenen taldeak
row.names(dtk.brcz)[!is.element(row.names(dtk.brcz), sort(intersect(mapi@data$IZ_EUSKAL, row.names(dtk.brcz))))]

