## =============================================================================
##
##	main script for laposteR Project
##	0. Load libraries & functions & data sources
##	1. Getting data
## =============================================================================

## =============================================================================
## 	0. Load libraries & functions & data sources
## =============================================================================
source('./R/core-R/load-functions.R')
loadLibraries()
loadFunctions()


## =============================================================================
## 	1. Getting data
## =============================================================================
france.zip.codes <- fread(france.zip.codes.url)
mailbox <- fread(mailbox.url)
post.offices <- fread(post.offices.url)

## =============================================================================
## 	2. Getting data
## =============================================================================

## Cleaning long / lat
mailbox[ , ':=' (
	Lat = as.numeric(gsub(pattern = "^(.*),.*$",replacement = "\\1",x = latlong)),
	Long = as.numeric(gsub(pattern = "^.*, (.*)$",replacement = "\\1",x = latlong)) 
)]
post.offices[ , ':=' (
	Lat = as.numeric(gsub(pattern = "^(.*),.*$",replacement = "\\1",x = latlong)),
	Long = as.numeric(gsub(pattern = "^.*, (.*)$",replacement = "\\1",x = latlong)) 
)]

## Subsetting Pyrenees Orientales
mailboxes.pyrenees.orientales <- mailbox[
	grepl(pattern = "^(66)[0-9]{3}$",x = CO_POSTAL)
	]
post.offices.pyrenees.orientales <- post.offices[
	grepl(pattern = "^(66)[0-9]{3}$",x =  code_postal)]


## Getting France partitions
download.file(url = france.level1.url, destfile = france.level1.file)
france.level.2 <- readRDS(france.level1.file)
france.level.1.df <- fortify(france.level.1)
download.file(url = france.level2.url, destfile = france.level2.file)
france.level.2 <- readRDS(france.level2.file)
france.level.2.df <- fortify(france.level.2)
download.file(url = france.level3.url, destfile = france.level3.file)
france.level.3 <- readRDS(france.level3.file)
france.level.3.df <- fortify(france.level.3)
download.file(url = france.level4.url, destfile = france.level4.file)
france.level.4 <- readRDS(france.level4.file)
france.level.4.df <- fortify(france.level.4)
download.file(url = france.level5.url, destfile = france.level5.file)
france.level.5 <- readRDS(france.level5.file)
france.level.5.df <- fortify(france.level.5)



## =============================================================================
## 	3. Exploratory analysis
## =============================================================================

g <- ggplot(data = mailboxes.pyrenees.orientales[CO_POSTAL =="66820"])
g <- g + geom_point(aes(x = Long, y = Lat))
g <- g + geom_point(data = post.offices.pyrenees.orientales[code_postal == "66820"], aes(x = Long, y = Lat), color = "red")
g <- g + coord_fixed()
g



## Make the icons for post office and mailbox
icon.post.office <- makeAwesomeIcon(icon= 'home', library = 'ion',
				    markerColor = 'orange',
				    iconColor = 'black')


icon.mailbox <- makeAwesomeIcon(icon= 'email', library = 'ion',
				markerColor = 'lightblue',
				iconColor = 'black')

m <- leaflet(width=1400, height=1024) %>%
	# base map
	addProviderTiles("Hydda.Base") %>%
	
	## Partitions
	addPolygons(data=france.level.5[france.level.5$NAME_2 %in% c("Pyrénées-Orientales"),],
		    stroke=TRUE, color="white", weight=1, opacity=1,
		    fill=TRUE, fillColor="#cccccc", smoothFactor=0.5) %>%

	addPolygons(data=france.level.4[france.level.4$NAME_2 %in% c("Pyrénées-Orientales"),],
		    stroke=TRUE, color="grey", weight=1, opacity=1,
		    fill=TRUE, fillColor="#cccccc", smoothFactor=0.5) %>%
	
	addPolygons(data=france.level.3[france.level.3$NAME_2 %in% c("Pyrénées-Orientales"),],
		    stroke=TRUE, color="purple", weight=1, opacity=1,
		    fill=TRUE, fillColor="#cccccc", smoothFactor=0.5) %>%

	# post offices layer
	addAwesomeMarkers(data = post.offices.pyrenees.orientales,
			  lng=~Long,
			  lat=~Lat,
			  popup = ~as.character(paste(
			  	paste(code_postal,localite),
			  	caracteristique_du_site,
			  	sep="<br>")),
			  label = ~as.character(libelle_du_site),
			  icon = icon.post.office
	)  %>%
	
	# pmailbox layer	
	addAwesomeMarkers(data = mailboxes.pyrenees.orientales,
			  lng=~Long,
			  lat=~Lat,
			  popup = ~as.character(paste(
			  	paste(CO_POSTAL,LB_COM),
			  	paste("Code INSEE:",CO_INSEE_COM),
			  	sep="<br>")),
			  icon = icon.mailbox,
			  label = ~as.character(LB_COM)
	)	

saveWidget(m, file = "posteR.html")


