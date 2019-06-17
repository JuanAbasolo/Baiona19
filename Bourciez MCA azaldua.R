## ---------------------------
##
## Script name: Bourciez MCA analisi azaldua
##
## Purpose of script: MCA analisia egitea eta horren osagaiak nabarmentzea
##
## Author: Juan
##
## Date Created: 2019-05-16     
##
## Email: juan.abasolo@ehu.eus
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(tidyverse)
require(data.table)
require(factoextra)
require(FactoMineR)
require(plot3D)
require(scales)
require(stringr)
require(scatterplot3d)
## Mapetarako
require(rgdal)
require(sp)

# source('')       # loads up all the packages we need

## ---------------------------
## Datuak sartu
dtk.brcz <- read.table('data/raw/bourciez-taulazabalduan-b.csv', sep = ',', header = T)
names(dtk.brcz) <- read.table('data/raw/bourciez-taulazabalduan-b.csv', sep = ',', stringsAsFactors = F)[1,]
row.names(dtk.brcz) <- dtk.brcz[,1] 
dtk.brcz <- dtk.brcz[,-1]
# NA gehiegi daukaz Hendaiak
dtk.brcz <- dtk.brcz[-which(row.names(dtk.brcz)=="Hendaia"),]


names(dtk.brcz)
## ---------------------------
## Datuen hasiera eta amaiera erakutsi
knitr::kable(rbind(head(dtk.brcz), tail(dtk.brcz)), 
             caption = 'Datuen hasiera eta amaiera')

## ---------------------------
##         Datuak antolatu

## ---------------------------
##         Datuen analisia
mca.bourciez <- MCA(dtk.brcz[,1:245], ncp = 4, graph = T)
## ---------------------------
##         Dimentsioek zenbat azaltzen duten
fviz_screeplot(mca.bourciez, addlabels = TRUE, ylim = c(0, 7))

## Base erabilita:
barplot(mca.bourciez$eig[,2], 
        ylim = c(0,7),
        col = c(rep('red',3), rep('darkblue', nrow(mca.bourciez$eig)-3)), 
        las = 2,
        cex.names = 0.7,
        main = 'Info guztia',
        sub = 'Baina tuneatik esan nahi jok bihar handijjagua eukiti, jakina!')

## ---------------------------
##         Dimentsioak zerk osatzen dituen (herriak/lemak)

# Asko tardaten dau. Honek aldagai guztiak irudikatzen ditu plano baten.
# fviz_mca_var(mca.bourciez, repel = TRUE)
# 50 garrantzitsuenak
fviz_mca_var(mca.bourciez, select.var = list(contrib = 50))
## Izenka aldagaiak aukeratu:
fviz_mca_var(mca.bourciez, 
             select.var= list(name = c("ardu", "arno", "ano")))

## Biak batera
## Hurrengoak gehiegi tardatzen du
# fviz_mca_biplot(mca.bourciez, repel = TRUE) + theme_minimal()
## Kontribuzioa (50 banakoak -herriak- eta 50 aldagai)   
fviz_mca_biplot(mca.bourciez,
                select.ind = list(contrib = 50),
                select.var = list(contrib = 50))

## Datuek errepresentaten dutenaren kalitatea:
## Koloriek erakusten dabe zein neurritan dagozan ondo ala txarto islatuta dimentsino horreetan
## hain daukagu aldagai asko, eze, uste dot ez dala holakorik erakutsi behar.
## KONTUZ! Nire ordenagailuan 4 min 40 seg tardaten dau
fviz_mca_var(mca.bourciez, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

## Dimentsioak zerk osatzen duen, herriak eta aldagaiak
## Lehenengo dimentsioa

# Cos2 of individuals
fviz_cos2(mca.bourciez, choice = "ind", axes = 1:3, top = 40)

# Contribution of individuals to the dimensions
fviz_contrib(mca.bourciez, choice = "ind", axes = 1, top = 50)
fviz_contrib(mca.bourciez, choice = "var", axes = 1, top = 50)
fviz_contrib(mca.bourciez, choice = "ind", axes = 2, top = 50)
fviz_contrib(mca.bourciez, choice = "var", axes = 2, top = 50)
fviz_contrib(mca.bourciez, choice = "ind", axes = 3, top = 40)
fviz_contrib(mca.bourciez, choice = "var", axes = 3, top = 50)
fviz_contrib(mca.bourciez, choice = "ind", axes = 4, top = 40)
fviz_contrib(mca.bourciez, choice = "var", axes = 4, top = 50)

## ---------------------------
##         Dimentsio biko irudiak
tauli <- mca.bourciez$ind$coord
col <- rgb(rescale(mca.bourciez$ind$coord[,1:3], to = c(0,1)))
tauli <- cbind(tauli, col)
plot(tauli[,1:2], 
     col = tauli[,'col'], 
     pch = 20, 
     cex = 3,
     xlab = paste( 'Aldakuntzen %',
                   round(mca.bourciez$eig[1,2], 2), 'azaltzen du'),
     ylab = paste( 'Aldakuntzen %',
                   round(mca.bourciez$eig[2,2], 2), 'azaltzen du'),
     main = 'Bourciez-en datuen araberako bariazioa',
     sub = 'Bariazioaren azalpenaren lehenengo bi dimentsioak'
)
text(tauli[,1:2], labels = row.names(tauli), col = 'gray40', cex = 0.7)

plot(tauli[,2:3], 
     col = tauli[,'col'], 
     pch = 20, 
     cex = 3,
     xlab = paste( 'Aldakuntzen %',
                   round(mca.bourciez$eig[2,2], 2), 'azaltzen du'),
     ylab = paste( 'Aldakuntzen %',
                   round(mca.bourciez$eig[3,2], 2), 'azaltzen du'),
     main = 'Arratiako hizkerak belaunaldi bitan eta euskara estandarra',
     sub = 'Bariazioaren azalpenaren bigarren eta hirugarren dimentsioak'
)
text(tauli[,2:3], labels = row.names(tauli), col = 'gray40', cex = 0.6)

## ---------------------------
##         3D irudia
tauli <- mca.bourciez$ind$coord
x <- tauli[,1]
y <- tauli[,2]
z <- tauli[,3]

sct3d <- scatterplot3d(x,
                       y,
                       z,
                       color = col,
                       pch = 19,
                       type = 'h', # c('p', 'h', 'l')
                       main = "Arratiako hizkerak eta batua 3D errepresantazioan",
                       xlab = '1. dimentsioa',
                       ylab = '2. dimentsioa',
                       zlab = '3. dimentsioa')
s3d.coords <-sct3d$xyz.convert(x,y,z)
text(s3d.coords$x,
     s3d.coords$y,
     labels = row.names(tauli),
     col = 'gray40',
     cex = 0.7,
     pos = 4)

# Ematen du Arrokiaga, Donapalu, Azkarate, Banka, Larresoro eta besteren batek zerbait dutela komunean,
# 3. dimentsio horretan
na.in.ind <- rowSums(is.na(dtk.brcz))%>%sort( decreasing = TRUE)
knitr::kable(na.in.ind[1:8], col.names = 'Informazio\nhutsuneak', 
             caption = 'Herrietako informazio hutsuneak')

knitr::kable(sort(colSums(is.na(dtk.brcz)), decreasing = TRUE)[1:40],
             caption = "Aldagaietako informazio hutsune handiak")

# Beste bertsino bat
# col <- matrix(ncol = 3, data = c(as.numeric(x), as.numeric(y), as.numeric(z)))
scatter3D(as.numeric(x), as.numeric(y), as.numeric(z), #clab = c("Sepal", "Width (cm)"),
          type = 'p', 
          pch = 20, 
          cex = 1.5, 
          bty = "f",
          col = 1,
          theta = -165, 
          phi = 225
          )

text3D(mca.bourciez$ind$coord[,1],
       mca.bourciez$ind$coord[,2],
       mca.bourciez$ind$coord[,3],
       labels = row.names(mca.bourciez$ind$coord),
       col = col,
       cex = 0.7,
       add = T)

## ---------------------------
##         BESTE 3D batzuk
# plot3Drgl::plotrgl()
# x <- arratia.eas.mca$ind$coord[,1]
# y <- arratia.eas.mca$ind$coord[,2]
# z <- arratia.eas.mca$ind$coord[,3]
# 
# # Open a new RGL device, aurreko testudunagaz segitzeko, markata itxi
# # rgl::open3d()
# # rgl::rgl.open()
# # rgl::rgl.bg(color = "white") # Setup the background color
# rgl::rgl.spheres(data.frame(x, y, z), r = 0.02, 
#                  color = rgb(scales::rescale(arratia.eas.mca$ind$coord, to = c(0,1)))) 
# 
# # Add x, y, and z Axes
# rgl::rgl.lines(c(min(x), max(x)), c(0, 0), c(0, 0), color = "red")
# rgl::rgl.lines(c(0, 0), c(min(y), max(y)), c(0, 0), color = "green")
# rgl::rgl.lines(c(0, 0), c(0, 0), c(min(z), max(z)), color = "blue")
# 

## ---------------------------
##         Puntu diagramak eta osagaien ekarpenak

## Ekarpenen taula garbitua

t.var.ekarpenak <- mca.bourciez$var$contrib
# kentzeko <- str_which(attributes(t.var.ekarpenak)$dimnames[[1]], '_0')
# t.var.ekarpenak <- t.var.ekarpenak[-kentzeko,]
# t.var.ekarpenak
# attributes(t.var.ekarpenak)$dimnames[[1]] <- gsub('_1', '', attributes(t.var.ekarpenak)$dimnames[[1]])

## Irudiak

dotchart(tauli[,1:4], main = 'Lau dimentsioen banaketa', color = col, cex = 0.8)

# Dimentsioka
par(mfrow = c(1, 2))
dotchart(tauli[order(tauli[,1]),1], color = col[order(tauli[, 1])], pch = 20,
         main = paste( 'Dim 1 %', 
                       round(mca.bourciez$eig[1,2], 2)))
# knitr::kable(t.var.ekarpenak[order(t.var.ekarpenak[,1], 
#                                    decreasing = TRUE), 1][1:120])
dotchart(sort(t.var.ekarpenak[order(t.var.ekarpenak[,1], 
                                    decreasing = TRUE), 1][1:150]), cex = 0.7,
         main = 'aldagaien ekarpenak')

#' Gehiegi dira ondo irakurri ahal izateko era honetara aurkeztuta. Hemen taulan:
#'
knitr::kable(t.var.ekarpenak[order(t.var.ekarpenak[,1], 
                                   decreasing = TRUE), 1][1:120])

par(mfrow = c(1, 2))
dotchart(tauli[order(tauli[,2]),2], color = col[order(tauli[, 2])], pch = 20,
         main = paste( 'DIM2 %', 
                       round(mca.bourciez$eig[2,2], 2)))
# knitr::kable(t.var.ekarpenak[order(t.var.ekarpenak[,2], 
#                                    decreasing = TRUE), 2][1:50])
dotchart(sort(t.var.ekarpenak[order(t.var.ekarpenak[,2], 
                                    decreasing = TRUE), 2][1:50]), cex = 0.7,
         main = 'aldagaien ekarpenak')

par(mfrow = c(1, 2))
dotchart(tauli[order(tauli[,3]),3], color = col[order(tauli[, 3])], pch = 20,
         main = paste( 'DIM3 %', 
                       round(mca.bourciez$eig[3,2], 2)))
# knitr::kable(t.var.ekarpenak[order(t.var.ekarpenak[, 3], 
#                                    decreasing = TRUE), 3][1:50])
dotchart(sort(t.var.ekarpenak[order(t.var.ekarpenak[, 3], 
                                    decreasing = TRUE), 3][1:50]), cex = 0.7,
         main = 'aldagaien ekarpenak')

par(mfrow = c(1, 2))
dotchart(tauli[order(tauli[,4]),4], color = col[order(tauli[, 4])], pch = 20,
         main = paste( 'DIM4 %', 
                       round(mca.bourciez$eig[4,2], 2)))
# knitr::kable(t.var.ekarpenak[order(t.var.ekarpenak[, 4], 
#                                    decreasing = TRUE), 4][1:50])
dotchart(sort(t.var.ekarpenak[order(t.var.ekarpenak[, 4], 
                                    decreasing = TRUE), 4][1:50]), cex = 0.7,
         main = 'aldagaien ekarpenak')
par(mfrow = c(1, 1))

## ---------------------------
##         Mapak

mapi <- readOGR('data/maps/EH_Udalerriak-Barri/udalerriak_EH_berriaB.shp')
plot(mapi)

## 3rak batera

col <- scales::rescale(mca.bourciez$ind$coord[,], to = c(0,1))
rn <- row.names(col)
col1 <- rgb(col)

mapi@data$mca.col <- 0
for(i in row.names(col)){
        # print(i)
        mapi@data[mapi@data$IZ_EUSKAL==i, "mca.col"] <- col1[which(row.names(col)==i)]
}
plot(mapi, col = mapi@data$mca.col)#, border = NA)

##

plot(mca.bourciez$ind$coord[,c(1,2)], 
     col = rgb(0, 0, col[, 3]), 
     pch = 20, cex = 5,
     main = 'Hirugarren dimentsioa koloreaz irudikatzen da')

##

mapi@data$r <- 0
mapi@data$g <- 0
mapi@data$b <- 0
for(i in row.names(col)){
        # print(i)
        mapi@data[mapi@data$IZ_EUSKAL==i, "r"] <- col[i,1]
        mapi@data[mapi@data$IZ_EUSKAL==i, "g"] <- col[i,2]
        mapi@data[mapi@data$IZ_EUSKAL==i, "b"] <- col[i,3]
}

par(mfrow = c(1, 1))
plot(mapi, col = rgb(mapi@data$r, 0, 0),
     main = paste('Bariantzaren %', round(mca.bourciez$eig[1,2]),2))
plot(mapi, col = rgb(0, mapi@data$g, 0),
     main = paste('Bariantzaren %', round(mca.bourciez$eig[2,2]),2))
plot(mapi, col = rgb(0, 0, mapi@data$b),
     main = paste('Bariantzaren %', round(mca.bourciez$eig[3,2]),2))
plot(mapi, col = rgb(mapi@data$r, 
                     mapi@data$g, 
                     mapi@data$b),
     main = paste('Bariantzaren %', round(mca.bourciez$eig[3,3]),2))
par(mfrow = c(1, 1))

plot(mapi, col = rgb(mapi@data$r,
                     mapi@data$g,
                     0),
     main = 'Lehenengo bi dimentsioak',
     sub = paste('Azaltzen du bariantzaren', round(mca.bourciez$eig[2,3],2)))





