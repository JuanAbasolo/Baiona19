## ---------------------------
##
## Script name: 3Dko.analisiak
##
## Purpose of script:
##
## Author: Juan
##
## Date Created: 2019-04-30
##
## Email: juan.abasolo@ehu.eus
##
## ---------------------------
##
## Notes: 
##   
##
##
##
## ---------------------------

## Source
source('datuak.bourciez.mapa.R')

## --------------------------- MCA

bourciez.mca <- FactoMineR::MCA(dtk.brcz[,1:245], ncp = 3, graph = T)


# 3D Irudia
rgl::rgl.spheres(bourciez.mca$ind$coord, 
                 col = rgb(scales::rescale(bourciez.mca$ind$coord, to = c(0, 1))), 
                 r = 0.030)


# Irudia mapan
col <- scales::rescale(bourciez.mca$ind$coord[,], to = c(0,1))
rn <- row.names(col)
col1 <- rgb(col)

mapi@data$mca.col <- 0
for(i in row.names(col)){
         print(i)
         mapi@data[mapi@data$IZ_EUSKAL==i, "mca.col"] <- col1[which(row.names(col)==i)]
}
plot(mapi, col = mapi@data$mca.col)#, border = NA)

## ---------------------------
##         Kolore bakotxa aldagai baten. Ahalegina.
# 
plot(bourciez.mca$ind$coord[,c(1,2)], 
     col = rgb(0, 0, col[, 3]), 
     pch = 20, cex = 5,
     main = 'Hirugarren dimentsioa koloreaz irudikatzen da')

# Mapari hiru aldagai, dimentsioko bat
mapi@data$r <- 0
mapi@data$g <- 0
mapi@data$b <- 0
for(i in row.names(col)){
        print(i)
        mapi@data[mapi@data$IZ_EUSKAL==i, "r"] <- col[i,1]
        mapi@data[mapi@data$IZ_EUSKAL==i, "g"] <- col[i,2]
        mapi@data[mapi@data$IZ_EUSKAL==i, "b"] <- col[i,3]
}

par(mfrow = c(2, 2))
plot(mapi, col = rgb(mapi@data$r, 0, 0),
     main = paste('Bariantzaren %', round(bourciez.mca$eig[1,2]),2))
plot(mapi, col = rgb(0, mapi@data$g, 0),
     main = paste('Bariantzaren %', round(bourciez.mca$eig[2,2]),2))
plot(mapi, col = rgb(0, 0, mapi@data$b),
     main = paste('Bariantzaren %', round(bourciez.mca$eig[3,2]),2))
plot(mapi, col = rgb(mapi@data$r, 
                     mapi@data$g, 
                     mapi@data$b),
     main = paste('Bariantzaren %', round(bourciez.mca$eig[3,3]),2))
par(mfrow = c(1, 1))

plot(mapi, col = rgb(mapi@data$r,
                     mapi@data$g,
                     0),
     main = 'Lehenengo bi dimentsioak',
     sub = paste('Azaltzen du bariantzaren', round(bourciez.mca$eig[2,3],2)))





# Ez dira datuak eskalatu bihar ploten aurretik
#  holan dimentsinoko alderdi negatiboa be ikusten da.
#  Bestelan positiboa baino ez da ikusten.
#
# 3D irudiak X, Y eta Zko 0 ardatzak euki bihar ditu pintauta, ez atzeko plano horreek
#
plot(bourciez.mca$ind$coord[,c(1,2)], col = col1, pch = 20, cex = 5)
plot(bourciez.mca$ind$coord[,c(1,3)], col = col1, pch = 20, cex = 5)

## ---------------------------- MDS


## ---------------------------
# Interactiboa izenekin

plot3D::text3D(bourciez.mca$ind$coord[,1],
               bourciez.mca$ind$coord[,2],
               bourciez.mca$ind$coord[,3],
               labels = row.names(bourciez.mca$ind$coord),
               col = rgb(scales::rescale(bourciez.mca$ind$coord, to = c(0,1))))
# Make the rgl version
# library("plot3Drgl")
plot3Drgl::plotrgl()


## ---------------------------
# Ardatzak pintau nahirik
# Plot3D

x <- bourciez.mca$ind$coord[,1]
y <- bourciez.mca$ind$coord[,2]
z <- bourciez.mca$ind$coord[,3]

# Open a new RGL device, aurreko testudunagaz segitzeko, markata itxi
# rgl::open3d()
# rgl::rgl.open()
# rgl::rgl.bg(color = "white") # Setup the background color
rgl::rgl.spheres(data.frame(x, y, z), r = 0.02, 
                 color = rgb(scales::rescale(bourciez.mca$ind$coord, to = c(0,1)))) 

# Add x, y, and z Axes
rgl::rgl.lines(c(min(x), max(x)), c(0, 0), c(0, 0), color = "red")
rgl::rgl.lines(c(0, 0), c(min(y), max(y)), c(0, 0), color = "green")
rgl::rgl.lines(c(0, 0), c(0, 0), c(min(z), max(z)), color = "blue")

## ---------------------------
##         GIFa -edo sortzeko
##   Hurrengo antolaereak irudikatzen dau
##   mapako banaketa (aldeak-alde)
##   Beste script baten dagoz sortzekuak
# play3d( spin3d( axis = c(1, -2, 2),
#                 rpm = 5), 
#         duration = 6 )

