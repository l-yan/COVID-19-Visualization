# ------------------------------------------------------------------------------
# load packages
# ------------------------------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggthemes)
library(shiny)
library(shinythemes)
library(usmap)
source("R/functions.R")
source("R/functions-shiny.R")

#-------------------------------------------------------------------------------
# download/update data ---------------------------------------------------------
#-------------------------------------------------------------------------------
Data_Download()

#-------------------------------------------------------------------------------
# process data -----------------------------------------------------------------
#-------------------------------------------------------------------------------
list2env(Data_Process(), envir = .GlobalEnv)

#-------------------------------------------------------------------------------
# define UI --------------------------------------------------------------------
#-------------------------------------------------------------------------------
ui <- fluidPage(
	navbarPage("Mobility and COVID-19", theme=shinytheme("journal"),
		
		#---------------------------------------------------------------------------
		# plot COVID-19 cases 
		tabPanel("COVID-19 Cases", fluid=TRUE,
			sidebarLayout(
				sidebarPanel(
					titlePanel(""),
					
					fluidRow(
						radioButtons(inputId = "show_case_level",
												 label = "Display cases at a state or county level:",
												 choices = c("state", "county"),
												 selected = "state")
						),
					br(),
					fluidRow(
						radioButtons(inputId = "show_case_diff",
												 label = "Display the level or change in cases:",
												 choices = c("total confirmed cases", 
												 						 "percentage change since yesterday", 
												 						 "percentage change since 1 week ago",
												 						 "percentage change since 2 weeks ago",
												 						 "percentage change since 1 month ago"),
												 selected = "total confirmed cases")
					)
				),
				mainPanel(plotOutput("case"),
									br(),
									br(),
									br(),
									br(),
									br(),
									br(),
									p("Data are from The New York Times's",
									a("COVID-19 data repository", href="https://github.com/nytimes/covid-19-data")
									))
			)
		),
		
		#---------------------------------------------------------------------------
		# plot google mobility
		tabPanel("Google Mobility", fluid=TRUE,
			sidebarLayout(
				sidebarPanel(
					titlePanel(""),
						fluidRow(
							radioButtons(inputId = "show_moby_level",
													 label = "Display change in mobility by:",
													 choices = c("state", "county"),
													 selected = "state")
						),
						br(),
						fluidRow(
							radioButtons(inputId = "show_moby_date",
													 label = "Display change in mobility on:",
													 choices = c("March 1, 2020", 
													 						"March 8, 2020", 
													 						"March 15, 2020", 
													 						"March 22, 2020",
													 						"March 29, 2020", 
													 						"April 5, 2020", 
													 						"April 11, 2020"),
													 selected = "April 11, 2020")
						),
						br(),
						fluidRow(
							radioButtons(inputId = "show_moby_type",
													 label = "Display change in mobility in:",
													 choices = c("Grocery and Pharmacy","Parks","Residential", 
						 									 				 "Retail and Recreation","Transit","Workplaces"),
													 selected = "Grocery and Pharmacy")
						)
				),
				mainPanel(plotOutput("moby"),
									br(),
									br(),
									br(),
									br(),
									br(),
									br(),
									p("Data are from Google's",
										a("Community Mobility Reports", href="https://www.google.com/covid19/mobility/")
									))
			)
		),
		
		#---------------------------------------------------------------------------
		# plot traffic volume
		tabPanel("Traffic Volume", fluid=TRUE,
						 sidebarLayout(
						 	sidebarPanel(
						 		titlePanel(""),
						 		fluidRow(
						 			dateInput("traf_date",
						 								label = "Display change in traffic volume on:",
						 								value = Sys.Date()-3,
						 								min = "2020-03-01",
						 								max = Sys.Date()-3
						 			)
						 		)
						 	),
						 	mainPanel(plotOutput("traf"),
						 						br(),
						 						br(),
						 						br(),
						 						br(),
						 						br(),
						 						br(),br(),br(),
						 						plotOutput("traf_case"),
						 						br(),
						 						br(),
						 						br(),
						 						br(),
						 						br(),
						 						br(),
						 						p("Data are from",
						 							a("MS2's Traffic Dashborad", href="https://www.ms2soft.com/traffic-dashboard/")
						 						))
						 )
		)
	)
)

#-------------------------------------------------------------------------------
# define server ----------------------------------------------------------------
#-------------------------------------------------------------------------------
server <- function(input, output){
	
	#-----------------------------------------------------------------------------
	# plot COVID-19 cases 
	output$case <- renderPlot({
		var <- switch(input$show_case_diff,
									"total confirmed cases" = "cases",
									"percentage change since yesterday" = "ln_cases_d1",
									"percentage change since 1 week ago" = "ln_cases_d7",
									"percentage change since 2 weeks ago" = "ln_cases_d14",
									"percentage change since 1 month ago" = "ln_cases_d30"
									)
		if (input$show_case_level=="state"){
			data <- merge(coord.state, case.state[date==max(date)], by="state", all.x=TRUE)
		}else{
			data <- merge(coord.cnty, case.cnty[date==max(date)], by="fips", all.x=TRUE)
		}
		plot_case(var, data, coord.state)
	}, height = 500, width = 800)
	
	#-----------------------------------------------------------------------------
	# plot google mobility 
	output$moby <- renderPlot({
		var <- switch(input$show_moby_type,
									"Grocery and Pharmacy" = "grocery",
									"Parks" = "parks",
									"Residential" = "residential",
									"Retail and Recreation" = "retail",
									"Transit" = "transit",
									"Workplaces" = "workplaces")
		day <- switch(input$show_moby_date,
									"March 1, 2020" = "2020-03-01",
									"March 8, 2020" = "2020-03-08",
									"March 15, 2020" = "2020-03-15",
									"March 22, 2020" = "2020-03-22",
									"March 29, 2020" = "2020-03-29",
									"April 5, 2020" = "2020-04-05",
									"April 11, 2020" = "2020-04-11")
		if (input$show_moby_level=="state"){
			data <- merge(coord.state, moby.state[date==day], by="state", all.x=TRUE)
		}else{
			data <- merge(coord.cnty, moby.cnty[date==day], by="id", all.x=TRUE)
		}
		plot_moby(var, data, coord.state)
	}, height = 500, width = 800)
	
	#-----------------------------------------------------------------------------
	# plot traffic volume 
	output$traf <- renderPlot({
		data <- merge(coord.state, traf.state[date==input$traf_date], by="state", all.x=TRUE)
		plot_traf(data, coord.state)
	}, height = 500, width = 800)
	
	#-----------------------------------------------------------------------------
	# plot case growth versus change in traffic volume
	case.sum <- case.state[state %in% unique(coord.state$state), sum(cases), keyby=date]
	case.sum[, case_growth := c(NA, 100*diff(log(V1)))]
	case.sum[, date := as.Date(date)]
	data <- merge(traf.state[state=="National"], case.sum, by = "date", all.x = TRUE)
	output$traf_case <- renderPlot({
		plot_traf_case(data)
	}, height = 500, width = 800)
}

#-------------------------------------------------------------------------------
# run app ----------------------------------------------------------------------
#-------------------------------------------------------------------------------
shinyApp(ui, server)
