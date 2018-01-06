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



## =============================================================================
## 	3. EExploratory analysis
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

leaflet(width=900, height=650) %>%
	# base map
	addProviderTiles("Hydda.Base") %>%
	
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
			  	CO_INSEE_COM,
			  	sep="<br>")),
			  icon = icon.mailbox,
			  label = ~as.character(LB_COM)
	)	



