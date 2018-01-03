library(deldir)
library(ggplot2)
library(ggthemes)
library(foreign)
library(sp)
library(dplyr)
library(data.table)
library(plotly)


## Data Sources
bureaux.poste.url <- "https://datanova.laposte.fr/explore/dataset/laposte_poincont2/download/?format=csv&timezone=Asia/Shanghai&use_labels_for_header=true"

## Read data from 
## https://datanova.laposte.fr
bureaux.poste.dl <- fread(bureaux.poste.url)

## Keep only poste office with geoloc and in France mainland

## -- remove NA
bureaux.poste <- bureaux.poste.dl[!is.na(Longitude) & !is.na(Latitude)]
## -- Keep france mainland only
bureaux.poste <- bureaux.poste[ Longitude > -20 &
                                        Longitude < 20 &
                                        Latitude > 30 &
                                        Latitude < 60]

## Creates the Voronoi (segments)
## Delaunay triangulation and Dirichlet tessellation
## This function computes the Delaunay triangulation (and hence the Dirichlet or Voronoi tesselation) of a planar point set according to the second (iterative) algorithm of Lee and Schacter — see REFERENCES. 
## The triangulation is made to be with respect to the whole plane by suspending it from so-called ideal points (-Inf,-Inf), (Inf,-Inf) (Inf,Inf), and (-Inf,Inf). 
## The triangulation is also enclosed in a finite rectangular window. A set of dummy points may be added, in various ways, to the set of data points being triangulated.


#department.list <- c("19","87","23","63","15","12")
department.list <- c("19","66")
#department.list <- c("75","92","93","94","77","78","91","95")
#department.list <- c("75","92","93","94","77","78","91","95",
#                     "19","87","23","63","15","12")
department.list <- as.character(30:50)


bureaux.poste <- bureaux.poste[
        grepl(pattern = paste0("^(",
                               paste(department.list,collapse = "|"),
                               ")[0-9]{3}"
        ),
        x = bureaux.poste$Code_postal
        )
        ]

voronoi.bureaux <- deldir(
        bureaux.poste[,Longitude],
        bureaux.poste[,Latitude]
)



#Now we can make a plot
g <- ggplot(data=bureaux.poste,
            aes(x=Longitude,y=Latitude))
## Plot Voronoi
g <- g + geom_segment(
        aes(x = x1, y = y1, xend = x2, yend = y2),
        size = 0.5,
        data = voronoi.bureaux$dirsgs,
        linetype = 1,
        color= "#9999BB")
## Add the points
g <- g + geom_point(
        fill=rgb(30,180,140,255,maxColorValue=255),
        pch=21,
        size = 1,
        color="#666666")
g <- g + coord_fixed(ratio = 1)
g


## France GAA
france.level0.url <- "http://biogeo.ucdavis.edu/data/gadm2.8/rds/FRA_adm0.rds"
france.level1.url <- "http://biogeo.ucdavis.edu/data/gadm2.8/rds/FRA_adm1.rds"
france.level1.file <- "FRA_adm1.rds"
france.level2.url <- "http://biogeo.ucdavis.edu/data/gadm2.8/rds/FRA_adm2.rds"
france.level2.file <- "FRA_adm2.rds"
france.level3.url <- "http://biogeo.ucdavis.edu/data/gadm2.8/rds/FRA_adm3.rds"
france.level4.url <- "http://biogeo.ucdavis.edu/data/gadm2.8/rds/FRA_adm4.rds"
france.level5.url <- "http://biogeo.ucdavis.edu/data/gadm2.8/rds/FRA_adm5.rds"

download.file(url = france.level1.url, destfile = france.level1.file)
france.level.1 <- readRDS(france.level1.file)
france.level.1.df <- fortify(france.level.1)
download.file(url = france.level2.url, destfile = france.level2.file)
france.level.2 <- readRDS(france.level2.file)
france.level.2.df <- fortify(france.level.2)



department.list.id <- france.level.2[france.level.2$CCA_2 %in% department.list,]$ID_2

## Now we can make a simple map
g <- ggplot()
g <- g + geom_map(data = france.level.2[france.level.2$CCA_2 %in% department.list,],
                  aes(map_id = id),
                  map = france.level.2.df,
                  fill  = "white",
                  col = "grey")
g <- g + expand_limits(
        x = france.level.2.df[france.level.2.df$id %in% department.list.id,]$long,
        y =  france.level.2.df[france.level.2.df$id %in% department.list.id,]$lat)
g <- g + coord_equal(ratio = 1)
g <- g + labs(x="Longitude", y="Latitude", title="Map of France old regions")

## Add the post office

## Plot Voronoi
g <- g + geom_segment(
        aes(x = x1, y = y1, xend = x2, yend = y2),
        size = 0.3,
        data = voronoi.bureaux$dirsgs,
        linetype = 1,
        color= "#9999BB")


## Add the points
g <- g + geom_point(
        aes(x = bureaux.poste$Longitude,
            y = bureaux.poste$Latitude),
        fill=rgb(30,180,140,255,maxColorValue=255),
        pch=21,
        size = 1,
        color="#666666")
g <- g + theme_bw()
g




## Leaflet Version
## Make a SpatialPointDataFrame from the initial data
voronoi_pts <- SpatialPointsDataFrame(cbind(bureaux.poste$Longitude,
                                            bureaux.poste$Latitude),
                                      bureaux.poste, match.ID = TRUE)

# tile.list extracts the polygon data from the deldir computation
voronoi_desc <- tile.list(deldir(voronoi_pts@coords[,1], voronoi_pts@coords[,2]))

voronoi_polygons <- lapply(1:(length(voronoi_desc)), function(i) {
        
        # tile.list gets us the points for the polygons but we
        # still have to close them, hence the need for the rbind
        tmp <- cbind(voronoi_desc[[i]]$x, voronoi_desc[[i]]$y)
        tmp <- rbind(tmp, tmp[1,])
        
        # now we can make the Polygon(s)
        Polygons(list(Polygon(tmp)), ID=i)
        
})

# hopefully the caller passed in good metadata!
voronoi_dat <- voronoi_pts@data

# this way the IDs _should_ match up w/the data & voronoi polys
rownames(voronoi_dat) <- sapply(slot(SpatialPolygons(voronoi_polygons),
                                     'polygons'),
                                slot, 'ID')

voronoi <- SpatialPolygonsDataFrame(SpatialPolygons(voronoi_polygons),
                                    data=voronoi_dat)

voronoi_df <- fortify(voronoi)















library(leaflet)
leaflet(width=900, height=650) %>%
        # base map
        addProviderTiles("Hydda.Base") %>%
        addPolygons(data=france.level.2[france.level.2$CCA_2 %in% department.list,],
                    stroke=TRUE, color="white", weight=1, opacity=1,
                    fill=TRUE, fillColor="#cccccc", smoothFactor=0.5) %>%
        
        # post offices layer
        addCircles(data=bureaux.poste,
                  lng=~Longitude,
                  lat=~Latitude,
                  radius=500, # size is in m for addCircles O_o
                  color="white", weight=1, opacity=1,
                  fillColor="steelblue", fillOpacity=1
        ) %>%
        
        # voronoi (click) layer
        addPolygons(data=voronoi,
                    stroke=TRUE,
                    color="#FFa5a5",
                    weight=2,
                    fill=TRUE,
                    fillOpacity = 0.0,
                    smoothFactor=0.5)
