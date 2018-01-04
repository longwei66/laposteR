library(data.table)
library(ggplot2)
library(plotly)

boites.aux.lettres <- fread("../../Data/BOP/laposte_boiterue.csv")
boites.pyrenees.orientales <- boites.aux.lettres[
  grepl(pattern = "^(66)[0-9]{3}$",x = CO_POSTAL)
]
boites.pyrenees.orientales[ , ':=' (
  Lat = as.numeric(gsub(pattern = "^(.*),.*$",replacement = "\\1",x = Latlong)),
  Long = as.numeric(gsub(pattern = "^.*, (.*)$",replacement = "\\1",x = Latlong)) 
  
)]



bureaux.de.poste <- fread("../../Data/BOP/laposte_poincont2.csv")
bureaux.pyrenees.orientales <- bureaux.de.poste[
  grepl(pattern = "^(66)[0-9]{3}$",x = Code_postal)
  ]


g <- ggplot(data = boites.pyrenees.orientales[CO_POSTAL =="66820"])
g <- g + geom_point(aes(x = Long, y = Lat))
g <- g + geom_point(data = bureaux.pyrenees.orientales[Code_postal == "66820"], aes(x = Longitude, y = Latitude), color = "red")
g <- g + coord_fixed()
ggplotly(g)