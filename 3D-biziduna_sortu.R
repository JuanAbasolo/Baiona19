## ---------------------------
##
## Script name: 3D-biziduna_sortu.R
##
## Purpose of script: 3D irudia Rtik atara eta objektu esportagarria sortu
##
## Author: Juan
##
## Date Created: 2019-05-09
##
## Email: juan.abasolo@ehu.eus
##
## ---------------------------
##
## Notes: 3D mobimentoduna sortzeko
##   Aurretik 3D irudia sortuta euki bihar da
##
## ---------------------------

## ---------------------------

## load up the packages we will need:  (uncomment as required)

## 18.04-n behar dau, ez badago instalauta:
## sudo apt-get install libmagick++-dev
## install.packages("magick")

require(magick)
require(rgl)
# require(data.table)
# source('')       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

# source("functions/summarise_data.R") 

## --------------------------- Internetetik moldatutako eredua
# z=iris
# #colors vector
# x=rep(0,150)
# 
# for (i in 1:150) {
#         if(z$Species[i] == "setosa") x[i]="royalblue1"
#         if(z$Species[i] == "versicolor") x[i]="darkcyan"
#         if(z$Species[i] == "virginica") x[i]="oldlace"
# }
# z$Color=x
# 
# plot3d( z[,1], z[,2], z[,3], col = z$Color, type = "s", radius = .02 )

# # We can indicate the axis and the rotation velocity

## Honek desegin leix bidijua sortzeko egindako aldaketak (axis'en kontrakuk eta bestik bardin)

play3d( spin3d( axis = c(-1, 2, -2), 
                rpm = 5),
        duration = 6 )


#Save like gift
movie3d( spin3d( axis = c(1, -2, 2), 
                 rpm = 5),
         fps = 25, # factor of 100 ixan bidau
         duration = 6, 
         dir = getwd(),
         type = "gif",
         clean = TRUE ) # Honek sortuniko tarteko irudi guztijak gordetzen ditu

## ---------------------------
##         mp4 formatua erabilteko:
##  Linux kontsola erabili bihar da
#' Kontsola erabilita eta ffmpeg instalauta lortzen dot `sudo apt install ffmpeg`. Hurrengo komandoak antzerako 
#' zeozer sortzen dau, aurreko komanduan `clean = FALSE` aukera itxi ezkero, harek sortutako
#' irudiak erabilita
#' `ffmpeg -framerate 15 -pattern_type glob -i '*.png' -c:v libx264 out.mp4`

