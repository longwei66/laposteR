library(leaflet)
library(shiny)
library(shinydashboard)


# header board
header <- dashboardHeader(
	title = 'Cartes Rurales et Postales',
	titleWidth = 300)

# Side bar boardy
sidebar <- dashboardSidebar(
	sidebarMenu(
		id = 'menu_tabs'
		, menuItem('Bureaux de Poste',
			   tabName = 'bureaux-de-poste',
			   icon = icon("home"))
		, menuItem('Boites aux lettres',
			   tabName = 'boites-aux-lettres',
			   icon = icon("envelope-open-o"))
		, menuItem('Tournées',
			   tabName = 'tournees',
			   icon = icon("truck")),
		menuItem('Parametres',
			   tabName = 'parameters',
			   icon = icon("cogs")),
		tags$hr(),
		tags$p("Données : datanova.laposte.fr"),
		tags$p("Infographie : BOP"),
		tags$div("GNU General Public License v3.0"), 
		img(src='BOP-LOGO.png',
		    width = '100',
		    align = 'center')
	)
)

# Body board
body <- dashboardBody(
	tabItems(
		
		tabItem(
			tabName = 'bureaux-de-poste'
			),
		tabItem(
			tabName = 'boites-aux-lettres',
			fluidRow(
				
				tabBox(
					title = "Pyrenées Orientales",
					# The id lets us use input$tabset1 on the server to find the current tab
					id = "tabset1",
					width = 12,
					tabPanel("Carte", leafletOutput("mymap")),
					tabPanel("Données", "Tab content 2")
				)
			),
			
			verbatimTextOutput('summary')
		)
	),
	tags$head(tags$style(HTML('
	/* logo */
        .skin-blue .main-header .logo {
                              background-color: #f4b943;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #ffc928;
                              }

	/* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #666666;
                              } 
      .skin-blue .main-sidebar {
        background-color: #666666;
      }
      .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
        background-color: #444444;
      }
    ')))
)

# Shiny UI
ui <- dashboardPage(
	header,
	sidebar,
	body
)

server <- function(input, output, session) {
	points <- eventReactive(input$recalc, {
		cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
	}, ignoreNULL = FALSE)
	
	load(file = 'm.Rda')
	
	output$mymap <- renderLeaflet({
		# leaflet() %>%
		# 	addProviderTiles(providers$Stamen.TonerLite,
		# 			 options = providerTileOptions(noWrap = TRUE)
		# 	) %>%
		# 	addMarkers(data = points())
		m
	})
	
}


shinyApp(ui, server)