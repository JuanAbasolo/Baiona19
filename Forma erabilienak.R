## ---------------------------
##
## Script name: Forma erabilienak
##
## Purpose of script: Xarlesek komentau eustan ia identifikau leikezan formarik erabilienak.
##                      horixe erakutsi bihar dau honek
## Author: Juan
##
## Date Created: 2019-06-06
##
## Email: juan.abasolo@ehu.eus
##
## ---------------------------
##
## Notes: oharrak
##   
##
## ---------------------------

## ---------------------------

## load up the packages we will need:  (uncomment as required)

# require(tidyverse)
# require(data.table)
# source('')       # loads up all the packages we need

## Datuak sartu ---------------------------
rm(list = ls())
dtk.brcz <- read.table('data/raw/bourciez-taulazabalduan-b.csv', sep = ',', header = T)
names(dtk.brcz) <- read.table('data/raw/bourciez-taulazabalduan-b.csv', sep = ',', stringsAsFactors = F)[1,]
row.names(dtk.brcz) <- dtk.brcz[,1] 
dtk.brcz <- dtk.brcz[,-1]
# NA gehiegi daukaz Hendaiak
dtk.brcz <- dtk.brcz[-which(row.names(dtk.brcz)=="Hendaia"),]

## Beste ---------------------------
##   

## Aldagai bakotxak zemat maila daukozan ikusteko
for (i in names(dtk.brcz)[1:244]){
        print(cat(i, '\t', length(levels(dtk.brcz[,i]))), '\t')
}

dtk.brcz$vin

## ---------------------------

taula.01 <- structure(list(lehenengoa = character(245), 
                           bigarrena = character(1)), 
                      hirugarrena = character(1), 
                      laugarrena = character(1),
                      class = "data.frame", 
                      row.names = names(dtk.brcz[,1:245]))
# data.frame(row.names = names(dtk.brcz[,1:245]), col.names = 1:12)

for(i in names(dtk.brcz[,1:245])){
        # print(i)
        x <-  sort(table(dtk.brcz[,i]), decreasing = TRUE)[1:4]
        if(is.table(x)){
                taula.01[i,1] <- paste(attributes(x)$dimnames[[1]][1], x[1], sep = ':')
                taula.01[i,2] <- paste(attributes(x)$dimnames[[1]][2], x[2], sep = ':')
                taula.01[i,3] <- paste(attributes(x)$dimnames[[1]][3], x[3], sep = ':')
                taula.01[i,4] <- paste(attributes(x)$dimnames[[1]][4], x[4], sep = ':')
        } else {
                taula.01[i,] <- c(paste(attributes(x)$names[1], x[1], sep = ':'), rep(NA,3))
        }
}

## Taula bera inprimatu ---------------------------
##         

knitr::kable(taula.01[,1:2], col.names = c('erabiliena', 'bigarren erabiliena'))
