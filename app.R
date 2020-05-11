# ------------------------------------------------------------------------------
# load packages
# ------------------------------------------------------------------------------
pkgs <- list("data.table", "ggplot2", "ggthemes", "ggiraph", "shiny", "shinythemes", "usmap", "curl")
lapply(pkgs, require, character.only = TRUE)
my_theme <- theme(plot.title = element_text(size = 18, hjust = 0.5, family = "Times"), 
									legend.position = "right", 
									legend.title = element_text(size = 14, family = "Times"),
									legend.text = element_text(size = 14,vjust = 0.5, family = "Times"))
source("R/functions.R")
source("R/functions-shiny.R")

#-------------------------------------------------------------------------------
# download/update data ---------------------------------------------------------
#-------------------------------------------------------------------------------
Data_Update()

#-------------------------------------------------------------------------------
# process data -----------------------------------------------------------------
#-------------------------------------------------------------------------------
list2env(Data_Process(), envir = .GlobalEnv)

#-------------------------------------------------------------------------------
# define UI --------------------------------------------------------------------
#-------------------------------------------------------------------------------
ui <- fluidPage(
	navbarPage("COVID-19 and Mobility", theme = shinytheme("journal"),
		
		#---------------------------------------------------------------------------
		# plot COVID-19 cases and deaths
		tabPanel("COVID-19 Cases", fluid=TRUE,
			sidebarLayout(
				sidebarPanel(width = 3,
						radioButtons(inputId = "show_case_level",
												 label = "Select state/county:",
												 choices = c("state", "county"),
												 selected = "state"),
						radioButtons(inputId = "show_case_content",
												 label = "Select cases/deaths:",
												 choices = c("cases", "deaths"),
												 selected = "cases"),
						radioButtons(inputId = "show_case_change",
												 label = "Display changes in cases/deaths over the past:",
												 choices = c("1 week", "2 weeks", "3 weeks","4 weeks"),
												 selected = "1 week"),
					div(style = "text-align:left", 
							p("Data are as of ", span(max(case.state$date), style = "color:blue"), " from The New York Times's",
								a("COVID-19 data repository.", href="https://github.com/nytimes/covid-19-data"))
					)
				),
				mainPanel(width = 9, girafeOutput("case"))
			)
		),
		
		#---------------------------------------------------------------------------
		# plot Google mobility
		tabPanel("Google Mobility", fluid=TRUE,
			sidebarLayout(
				sidebarPanel(width = 3,
					radioButtons(inputId = "show_moby_level",
											 label = "Select state/county:",
											 choices = c("state", "county"),
											 selected = "state"),
					radioButtons(inputId = "show_moby_type",
											 label = "Select a category:",
											 choices = c("Grocery and Pharmacy","Parks","Residential", 
											 						"Retail and Recreation","Transit","Workplaces"),
											 selected = "Grocery and Pharmacy"),
					dateInput(inputId = "show_moby_date",
										label = "Display change in mobility on:",
										value = max(moby.state$date),
										min = min(moby.state$date),
										max = max(moby.state$date)),
					div(style = "text-align:left", 
							p("Data are as of ", span(max(moby.state$date), style = "color:blue"), " from Google's",
								a("Community Mobility Reports.", href="https://www.google.com/covid19/mobility/"))
					)
				),
				mainPanel(width = 9, girafeOutput("moby_goog"))
			)
		),
		
		#---------------------------------------------------------------------------
		# plot DL mobility
		tabPanel("DL Mobility", fluid=TRUE,
						 sidebarLayout(
						 	sidebarPanel(width = 3,
						 							 radioButtons(inputId = "moby_dl_level",
						 							 						 label = "Select state/county:",
						 							 						 choices = c("state", "county"),
						 							 						 selected = "state"),
						 							 radioButtons(inputId = "moby_dl_type",
						 							 						 label = "Select a measure:",
						 							 						 choices = c("median max-distance mobility", 
						 							 						 						"change in median max-distance mobility"),
						 							 						 selected = "median max-distance mobility"),
						 							 helpText("Change in median is the percent of normal m50, with normal m50 defined during 2020-02-17 to 2020-03-07."),
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
						 	mainPanel(width = 9, girafeOutput("moby_dl"))
						 )
		),
		
		#---------------------------------------------------------------------------
		# plot Apple mobility
		tabPanel("Apple Mobility", fluid=TRUE,
						 sidebarLayout(
						 	sidebarPanel(width = 3,
						 							 selectInput(inputId = "moby_appl_type", 
						 							 						label = "Select a transportation type:", 
						 							 						choices = unique(moby.appl$transportation_type),
						 							 						selected = "driving"),
						 							 div(style = "text-align:left", 
						 							 		p("Data are as of ", span(max(moby.appl$date), style = "color:blue"), " from Apple's",
						 							 			a("Mobility Trends Reports.", href="https://www.apple.com/covid19/mobility"))
						 							 )
						 	),
						 	mainPanel(width = 9, plotOutput("moby_appl"))
						 )
		),
		
		#---------------------------------------------------------------------------
		# plot traffic volume
		tabPanel("Traffic Volume", fluid=TRUE,
						 sidebarLayout(
						 	sidebarPanel(width = 3,
						 		selectInput(inputId = "traf_state", 
						 								label = "Select a state:", 
						 								choices = unique(traf.state$state),
						 								selected = "National"),
					 			div(style = "text-align:left", 
					 					p("Data are as of ", span(max(traf.state$date), style = "color:blue"), " from ",
					 						a("MS2's Traffic Dashborad.", href="https://www.ms2soft.com/traffic-dashboard/"))
					 			)
						 	),
						 	mainPanel(width = 9, plotOutput("traf"))
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
	output$case <- renderGirafe({
		var <- paste(input$show_case_content, "_w", substr(input$show_case_change, 1, 1), sep = "")
		if (input$show_case_level=="state"){
			data <- merge(coord.state, case.state[date==max(date)], by="state", all.x=TRUE)
		}else{
			data <- merge(coord.cnty, case.cnty[date==max(date)], by="fips", all.x=TRUE)
		}
		ggiraph(code = print(plot_case(var, data, coord.state)), width_svg = 9, height_svg = 6)
	})
	
	#-----------------------------------------------------------------------------
	# plot Google mobility 
	output$moby_goog <- renderGirafe({
		if (input$show_moby_level=="state"){
			data <- merge(coord.state, moby.state[date==input$show_moby_date], by="state", all.x=TRUE)
		}else{
			data <- merge(coord.cnty, moby.cnty[date==input$show_moby_date], by="id", all.x=TRUE)
		}
		ggiraph(code = print(plot_moby(input$show_moby_type, data, coord.state)), width_svg = 9, height_svg = 6)
	})
	
	#-----------------------------------------------------------------------------
	# plot DL mobility 
	output$moby_dl <- renderGirafe({
		if (input$moby_dl_level=="state"){
			data <- merge(coord.state, moby.dl[county=="" & date==input$moby_dl_date], by="state", all.x=TRUE)
			data[, county := NULL]
		}else{
			data <- merge(coord.cnty, moby.dl[county!="" & date==input$moby_dl_date], by="fips", all.x=TRUE)
		}
		ggiraph(code = print(plot_moby_dl(input$moby_dl_type, data, coord.state)), width_svg = 9, height_svg = 6)
	})
	
	#-----------------------------------------------------------------------------
	# plot Apple mobility 
	output$moby_appl <- renderPlot({
		case.sum <- case.state[state %in% unique(coord.state$state), sum(cases), keyby=date]
		case.sum[, case_growth := c(NA, 100*diff(log(V1)))]
		case.sum[, date := as.Date(date)]
		data <- merge(moby.appl[transportation_type==input$moby_appl_type], case.sum, by = "date", all.x = TRUE)
		plot_moby_appl(data)
	}, width = 720, height = 480)
	
	#-----------------------------------------------------------------------------
	# plot traffic volume 
	output$traf <- renderPlot({
		if (input$traf_state == "National"){
			case.sum <- case.state[state %in% unique(coord.state$state), sum(cases), keyby=date]
		}else{
			case.sum <- case.state[state == input$traf_state, sum(cases), keyby=date]
		}
		case.sum[, case_growth := c(NA, 100*diff(log(V1)))]
		case.sum[, date := as.Date(date)]
		data <- merge(traf.state[state==input$traf_state], case.sum, by = "date", all.x = TRUE)
		plot_traf(data)
	}, width = 720, height = 480)

}

#-------------------------------------------------------------------------------
# run app ----------------------------------------------------------------------
#-------------------------------------------------------------------------------
shinyApp(ui, server)
