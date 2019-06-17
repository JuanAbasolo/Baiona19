## ---------------------------
##
## Script name: Ploty 3D
##
## Purpose of script: MCA on line argitaratzeko zerbitzua aprobatzea (PLOTY)
##
## Author: Juan
##
## Date Created: 2019-05-02
##
## Email: juan.abasolo@ehu.eus
##
## ---------------------------
##
## Notes: Honek internetera konexioa behar dau.
##   
##
## ---------------------------

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(plotly)
# require(data.table)
# source('')       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

# source("datak.bourciez.mapa.R") 

## --------------------------- Ploty erabiltekoak
# Sys.setenv("plotly_username"="JuanAbasolo")
# Sys.setenv("plotly_api_key"="iB99j3Tk1GfN7lOJdaWM")
# Set up API credentials: https://plot.ly/r/getting-started

bourciez.mca <- FactoMineR::MCA(dtk.brcz[,1:245], ncp = 3, graph = T)
kontr.mca <- factoextra::get_eigenvalue(bourciez.mca)[,2]


mtcars$am[which(mtcars$am == 0)] <- 'Automatic'
mtcars$am[which(mtcars$am == 1)] <- 'Manual'
mtcars$am <- as.factor(mtcars$am)

bourciez.mca$ind$coord[,1]

x1 <- data.frame(bourciez.mca$ind$coord)

p <- plot_ly(x1, 
             x=~Dim.1, 
             y=~Dim.2, 
             z=~Dim.3,
             color = rgb(scales::rescale(bourciez.mca$ind$coord, to = c(0,1)))) %>%
        add_markers() %>%
        layout(scene = list(xaxis = list(title = round(kontr.mca[1], 2)),
                            yaxis = list(title = round(kontr.mca[2], 2)),
                            zaxis = list(title = round(kontr.mca[3], 2)))
               )

# Create a shareable link to your chart
chart_link <- api_create(p, filename="scatter3d-basic")
chart_link
