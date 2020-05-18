# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Preface
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(DT)
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggiraph)
library(rvest)
library(shiny)
library(shinythemes)
library(stringi)
library(usmap)
library(curl)
source("R/functions.R")
source("R/functions-shiny.R")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Download/update data
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Data_Update()

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Process datasets and extract them into workspace 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
list2env(Data_Process(), envir = .GlobalEnv)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Define UI 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ui <- fluidPage(
	
	# make title bigger
	list(tags$style(HTML("
      .navbar-default .navbar-brand {font-size: 24px;} 
  "))),
	
	# specify web name and theme
	navbarPage("COVID-19 and Mobility", theme = shinytheme("journal"),
						 
						 #---------------------------------------------------------------------------
						 # COVID-19 data
						 #---------------------------------------------------------------------------
						 tabPanel("COVID-19 Cases", fluid=TRUE,
						 				 sidebarLayout(
						 				 	sidebarPanel(
						 				 		radioButtons(inputId = "case_state_or_county",
						 				 								 label = "Select a geographic level:",
						 				 								 choices = c("State", "County"),
						 				 								 inline = TRUE),
						 				 		radioButtons(inputId = "case_case_or_death",
						 				 								 label = "Show cases or deaths:",
						 				 								 choices = c("Cases", "Deaths"),
						 				 								 inline = TRUE),
						 				 		radioButtons(inputId = "case_level_or_change",
						 				 								 label = "Show new cases/deaths in levels or changes:",
						 				 								 choices = c("New cases/deaths", "Weekly changes in new cases/deaths")),
						 				 		selectInput( inputId = "case_period",
						 				 								 label = "Choose a week:",
						 				 								 choices = GenerateWeekInterval(unique(case$date))),
						 				 		checkboxInput(inputId = "case_per_capita_or_not",
						 				 									label = "Expressed per 1M people?"),
						 				 		div(style = "text-align:left", 
						 				 				p("Data are as of ", span(max(case$date), style = "color:blue"), " from The New York Times's",
						 				 					a("COVID-19 data repository.", href="https://github.com/nytimes/covid-19-data"))
						 				 		)
						 				 	),
						 				 	mainPanel(girafeOutput("case_fig1"))
						 				 )
						 ),
						 
						 #---------------------------------------------------------------------------
						 # Mobility data: Google
						 #---------------------------------------------------------------------------
						 tabPanel("Google Mobility", fluid=TRUE,
						 				 sidebarLayout(
						 				 	sidebarPanel(
						 				 		p("Google's mobility measures the change in visits to places compared to the baseline, 
						where baseline is the median value, for the corresponding day of the week, during Jan 3–Feb 6, 2020."),
						 				 		radioButtons(inputId = "moby_goog_state_or_county",
						 				 								 label = "Select a geographic level:",
						 				 								 choices = c("State", "County"),
						 				 								 inline = TRUE),
						 				 		radioButtons(inputId = "moby_goog_type",
						 				 								 label = "Select a place type:",
						 				 								 choices = c("Grocery and Pharmacy","Parks","Residential",
						 				 								 						"Retail and Recreation","Transit","Workplaces")),
						 				 		radioButtons(inputId = "moby_goog_level_or_change",
						 				 								 label = "Show mobility in levels or changes:",
						 				 								 choices = c("Levels", "Weekly changes")),
						 				 		selectInput( inputId = "moby_goog_date",
						 				 								 label = "Choose a date:",
						 				 								 choices = seq(max(moby.goog$date), by=-7, length.out = 7)),
						 				 		div(style = "text-align:left",
						 				 				p("Data are as of ", span(max(moby.goog$date), style = "color:blue"), " from Google's",
						 				 					a("Community Mobility Reports.", href="https://www.google.com/covid19/mobility/"))
						 				 		)
						 				 	),
						 				 	mainPanel(girafeOutput("moby_goog_fig1"))
						 				 )
						 ),
						 
						 #---------------------------------------------------------------------------
						 # Mobility data: Descartes Labs (DL)
						 #---------------------------------------------------------------------------
						 tabPanel("DL Mobility", fluid=TRUE,
						 				 sidebarLayout(
						 				 	sidebarPanel(
						 				 		p("Descartes Labs' (DL) mobility measures the distance a typical member of a given population moves in a day. 
						See technical details", a("here.", href = "https://www.descarteslabs.com/wp-content/uploads/2020/03/mobility-v097.pdf")),
						 				 		radioButtons(inputId = "moby_dl_state_or_county",
						 				 								 label = "Select a geographic level:",
						 				 								 choices = c("State", "County"),
						 				 								 inline = TRUE),
						 				 		radioButtons(inputId = "moby_dl_level_or_change",
						 				 								 label = "Select a measure:",
						 				 								 choices = c("m50", "m50_index"),
						 				 								 inline = TRUE),
						 				 		helpText("m50 is the median max-distance mobility"),
						 				 		helpText("m50_index is the percent of normal m50, with normal m50 defined during 2020-02-17 to 2020-03-07"),
						 				 		dateInput(inputId = "moby_dl_date",
						 				 							label = "Select a date:",
						 				 							value = max(moby.dl$date),
						 				 							min = min(moby.dl$date),
						 				 							max = max(moby.dl$date)),
						 				 		div(style = "text-align:left",
						 				 				p("Data are as of ", span(max(moby.dl$date), style = "color:blue"), " from Descartes Labs'",
						 				 					a("mobility statistics.", href="https://github.com/descarteslabs/DL-COVID-19"))
						 				 		)
						 				 	),
						 				 	mainPanel(girafeOutput("moby_dl_fig1"))
						 				 )
						 ),
						 
						 #---------------------------------------------------------------------------
						 # Mobility data: Apple
						 #---------------------------------------------------------------------------
						 tabPanel("Apple Mobility", fluid=TRUE,
						 				 sidebarLayout(
						 				 	sidebarPanel(
						 				 		p("Apple's mobility measures the daily change in requests for directions by transportation type for all available countries/regions, sub-regions, and cities."),
						 				 		selectInput(inputId = "moby_appl_type",
						 				 								label = "Select a transportation type:",
						 				 								choices = c("Driving", "Transit", "Walking")),
						 				 		div(style = "text-align:left",
						 				 				p("Data are as of ", span(max(moby.appl$date), style = "color:blue"), " from Apple's",
						 				 					a("Mobility Trends Reports.", href="https://www.apple.com/covid19/mobility"))
						 				 		)
						 				 	),
						 				 	mainPanel(plotOutput("moby_appl_fig1"))
						 				 )
						 ),
						 
						 #---------------------------------------------------------------------------
						 # Mobility data: MS2
						 #---------------------------------------------------------------------------
						 tabPanel("MS2 Mobility", fluid=TRUE,
						 				 sidebarLayout(
						 				 	sidebarPanel(
						 				 		p("MS2's mobility measures the daily change in traffic volumes as compared to the same day of week in the same month of last year. 
						The metric is based on 24/7/365 data from traffic sensors and smart traffic signals installed by many road agencies."),
						 				 		selectInput(inputId = "moby_ms2_state",
						 				 								label = "Select a state:",
						 				 								choices = unique(moby.ms2$state)[-1]),
						 				 		div(style = "text-align:left",
						 				 				p("Data are as of ", span(max(moby.ms2$date), style = "color:blue"), " from ",
						 				 					a("MS2's Traffic Dashborad.", href="https://www.ms2soft.com/traffic-dashboard/"))
						 				 		)
						 				 	),
						 				 	mainPanel(plotOutput("moby_ms2_fig1"))
						 				 )
						 )
	)
)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Define server 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
server <- function(input, output){
	
	#-----------------------------------------------------------------------------
	# COVID-19 data
	#-----------------------------------------------------------------------------
	output$case_fig1 <- renderGirafe({
		v1 <- input$case_state_or_county == "State"
		v2 <- tolower(input$case_case_or_death)
		v3 <- ifelse(input$case_level_or_change == "New cases/deaths", "_new", "_new_change")
		v4 <- as.Date(substr(input$case_period, 15, 24))
		v5 <- ifelse(input$case_per_capita_or_not, "_a", "")
		ggiraph(code = print(plot_case(coord, case, v1, v2, v3, v4, v5)), width_svg = 9, height_svg = 6)
	})
	
	#-----------------------------------------------------------------------------
	# Mobility data: Google
	#-----------------------------------------------------------------------------
	output$moby_goog_fig1 <- renderGirafe({
		v1 <- input$moby_goog_state_or_county == "State"
		v2 <- input$moby_goog_type
		v3 <- ifelse(input$moby_goog_level_or_change == "Levels", "", "_change")
		v4 <- as.Date(input$moby_goog_date)
		ggiraph(code = print(plot_moby_goog(coord, moby.goog, v1, v2, v3, v4)), width_svg = 9, height_svg = 6)
	})
	
	#-----------------------------------------------------------------------------
	# Mobility data: DL
	#-----------------------------------------------------------------------------
	output$moby_dl_fig1 <- renderGirafe({
		v1 <- input$moby_dl_state_or_county == "State"
		v2 <- input$moby_dl_level_or_change
		v3 <- input$moby_dl_date
		ggiraph(code = print(plot_moby_dl(coord, moby.dl, v1, v2, v3)), width_svg = 9, height_svg = 6)
	})
	
	#-----------------------------------------------------------------------------
	# Mobility data: Apple
	#-----------------------------------------------------------------------------
	output$moby_appl_fig1 <- renderPlot({
		v1 <- input$moby_appl_type
		plot_moby_appl(moby.appl, v1)
	}, width = 720, height = 480)
	
	#-----------------------------------------------------------------------------
	# mobility data: MS2
	#-----------------------------------------------------------------------------
	output$moby_ms2_fig1 <- renderPlot({
		v1 <- input$moby_ms2_state
		plot_moby_ms2(moby.ms2, v1)
	}, width = 720, height = 480)
	
}

shinyApp(ui, server)