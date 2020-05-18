# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This file contains helper functions 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#-----------------------------------------------------------------------------
# Download/update data and save them in folder 'data'
#-----------------------------------------------------------------------------
Data_Update <- function(){
	
	# COVID-19 data from New York Times
	if (as.Date(file.info("data/case.csv")$mtime) < Sys.Date()){
		dt.state <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", showProgress = FALSE) 
		dt.cnty <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", showProgress = FALSE) 
		dt.state[, county := "State"] # set county="State"
		dt <- rbind(dt.state[,c(1,6,2:5)], dt.cnty)
		write.csv(dt, file = "data/case.csv")
		print("COVID-19 data have been successfully updated!")
	}else{
		print("COVID-19 data are up to date!")
	}
	
	# Mobility data from Google
	if (as.Date(file.info("data/moby_goog.csv")$mtime) < Sys.Date()){
		write.csv(x = fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", showProgress = FALSE), 
							file = "data/moby_goog.csv")
		print("Mobility data from Google have been successfully updated!")
	}else{
		print("Mobility data from Google are up to date!")
	}
	
	# Mobility data from Descartes Labs
	if (as.Date(file.info("data/moby_dl.csv")$mtime) < Sys.Date()){
		write.csv(x = fread("https://raw.githubusercontent.com/descarteslabs/DL-COVID-19/master/DL-us-mobility-daterow.csv", showProgress = FALSE), 
							file = "data/moby_dl.csv")
		print("Mobility data from Descartes Labs have been successfully updated!")
	}else{
		print("Mobility data from Descartes Labs are up to date!")
	}
	
	# Mobility data from Apple
	if (as.Date(file.info("data/moby_appl.csv")$mtime) < Sys.Date()){
		print("Mobility data from Apple are out of date, please update them from https://www.apple.com/covid19/mobility !")
	}else{
		print("Mobility data from Apple are up to date!")
	}
	
	# Mobility data from MS2
	if (as.Date(file.info("data/moby_ms2.csv")$mtime) < Sys.Date()){
		# step 1: scrape the last 30-day data from the web
		dat <- html_node(x = read_html("https://www.ms2soft.com/traffic-dashboard/"), 
										 css = "main#content")
		dat <- strsplit(html_text(dat), "\\[\\{|\\}\\]")[[1]][2]
		dat <- stri_replace_all_fixed(dat, pattern = c("\"", "{", "State:", "CountDate:", "PercentChange:", "PercentChangeTruck:", "}"), 
													 replacement = c(""), vectorize_all = FALSE)
		dat <- unlist(strsplit(dat, split = ","))
		index <- 1:length(dat) %% 4
		moby.traf <- data.table(state_abbr = dat[index==1], 
														date = as.Date(dat[index==2]), 
														traf = as.numeric(dat[index==3]))
		# step 2: merge the last 30-day data with older data
		moby.traf.old <- fread("data/moby_ms2.csv")[, -c("V1")]
		moby.traf.old[, date := as.Date(date)]
		moby.traf <- rbind(moby.traf, moby.traf.old[date < min(moby.traf$date)])
		write.csv(moby.traf, file = "data/moby_ms2.csv")
		print("Mobility data from MS2 have been successfully updated!")
	}else{
		print("Mobility data from MS2 are up to date!")
	}
	
}

#-----------------------------------------------------------------------------
# Process data
#-----------------------------------------------------------------------------
Data_Process <- function(){
	
	# ----------------------------------------------------------------------------
	# COVID-19 data
	# ----------------------------------------------------------------------------
	# read data
	case <- fread("data/case.csv")[, -c("V1")]
	# convert daily to weekly
	case[, date := as.Date(date)]
	case <- case[wday(case$date) == 1]
	# remove cases when the patient's county of residence is unknown or pending determination
	case <- case[county != "Unknown"] 
	# set fips=36061 b/c cases are aggregated over New York(36061), Kings(36047), Queens(36081), Bronx(36005), and Richmond(36085)
	case[county == "New York City", fips := 36061] 
	# set fips=29037 b/c cases are aggregated over Case(29037), Clay(29047), Jackson(29095), and Platte(29165)
	case[county == "Kansas City", fips := 29037] 
	# add population data
	pop <- rbind(data.table(statepop)[, list(fips, pop_2015)], data.table(countypop)[, list(fips, pop_2015)])
	pop[fips=="36061", pop_2015 := pop[fips %in% c("36061", "36047", "36081", "36005", "36085"), sum(pop_2015)]]
	pop[fips=="29037", pop_2015 := pop[fips %in% c("29037", "29047", "29095", "29165"), sum(pop_2015)]]
	pop[, fips := as.numeric(fips)]
	pop[, pop_2015 := pop_2015 / 1000000]
	case <- merge(case, pop, by = "fips", all.x = TRUE)
	# define variables
	setorder(case, fips, date)
	case[, cases_new := c(NA, diff(cases)), by = fips] # weekly new cases
	case[, deaths_new := c(NA, diff(deaths)), by = fips] # weekly new deaths
	case[, cases_new_percent := c(NA, 100 * diff(log(cases))), by = fips] # weekly new cases (%)
	case[, deaths_new_percent := c(NA, 100 * diff(log(deaths))), by = fips] # weekly new deaths (%)
	case[, cases_new_change := c(NA, diff(cases_new)), by = fips] # weekly change in new cases
	case[, deaths_new_change := c(NA, diff(deaths_new)), by = fips] # weekly change in new deaths
	case[, cases_a := cases / (pop_2015)] # total cases per 100K people
	case[, deaths_a := deaths / (pop_2015)] # total deaths per 100K people
	case[, cases_new_a := cases_new / (pop_2015)] # new cases per 100K
	case[, deaths_new_a := deaths_new / (pop_2015)] # new deaths per 100K
	case[, cases_new_change_a := cases_new_change / (pop_2015)] # weekly change in new cases per 100K
	case[, deaths_new_change_a := deaths_new_change / (pop_2015)] # weekly change in new deaths per 100K
	
	# ----------------------------------------------------------------------------
	# Mobility data: Google
	# ----------------------------------------------------------------------------
	# read data and rename
	moby.goog <- fread("data/moby_goog.csv")[, -c("V1")]
	setnames(moby.goog, c("country_code", "country_name", "state", "county", "date",
									 "retail", "grocery", "parks", "transit", "workplaces", "residential"))
	# select data for US states and counties
	moby.goog <- moby.goog[country_code == "US" & state != ""]
	# adjust county names
	moby.goog[county == "", county := "State"]
	moby.goog[county == "Doña Ana County", county := "Dona Ana County"]
	moby.goog[county == "Shannon County" & state == "South Dakota", county := "Oglala Lakota County"]
	# convert daily to weekly
	moby.goog[, date := as.Date(date)]
	moby.goog <- moby.goog[wday(moby.goog$date) == 1]
	# define key
	moby.goog[, id := tolower(paste(state, county, sep = ","))]
	# define variables
	setorder(moby.goog, id, date)
	moby.goog[, (6:11) := lapply(.SD, as.numeric), .SDcols=(6:11)]
	for (col in names(moby.goog)[6:11]){
		moby.goog[, paste(col, "_change", sep = "") := c(NA, diff(get(col))), by = id]
	}
	
	# ----------------------------------------------------------------------------
	# Mobility data: Apple
	# ----------------------------------------------------------------------------
	moby.appl <- fread("data/moby_appl.csv")
	moby.appl <- melt(moby.appl, measure.vars = 5:ncol(moby.appl), variable.name = "date", value.name = "traf")
	moby.appl <- moby.appl[region == "United States"]
	moby.appl[, date := as.Date(date)]
	setorder(moby.appl, transportation_type, date)
	
	# ----------------------------------------------------------------------------
	# Mobility data: Descartes Labs
	# ----------------------------------------------------------------------------
	moby.dl <- fread("data/moby_dl.csv")[, c(2, 5:10)]
	setnames(moby.dl, old = c("admin1", "admin2"), new = c("state", "county"))
	moby.dl[county == "", county := "State"]
	moby.dl[, date := as.Date(date)]
	setorder(moby.dl, fips, date)
	
	# ----------------------------------------------------------------------------
	# Mobility data: MS2
	# ----------------------------------------------------------------------------
	moby.ms2 <- fread("data/moby_ms2.csv")[, -c("V1")]
	tmp <- unique(data.table(us_map("state"))[,list(abbr, full, fips)], by = "abbr")
	moby.ms2$state <- tmp$full[match(moby.ms2$state_abbr, tmp$abbr)]
	moby.ms2$fips <- as.numeric(tmp$fips[match(moby.ms2$state_abbr, tmp$abbr)])
	moby.ms2[is.na(state), state := "National"]
	moby.ms2[, date := as.Date(date)]
	setorder(moby.ms2, fips, date)
	
	# ----------------------------------------------------------------------------
	# US geo info
	# ----------------------------------------------------------------------------
	coord.state <- data.table(us_map("state"))
	setnames(coord.state, "full", "id")
	coord.state[, id := paste(tolower(id), ",state", sep = "")]
	coord.cnty <- data.table(us_map("county"))
	coord.cnty[, id := tolower(paste(full, gsub(" city", "", county), sep = ","))]
	coord <- rbind(coord.state[, list(x, y, group, fips, id)], coord.cnty[, list(x, y, group, fips, id)])
	coord[, fips := as.numeric(fips)]
	
	# ----------------------------------------------------------------------------
	# Export all data as a list
	# ----------------------------------------------------------------------------
	alldata <- list(case = case, 
									moby.goog = moby.goog, 
									moby.appl = moby.appl, 
									moby.dl = moby.dl, 
									moby.ms2 = moby.ms2, 
									coord = coord)
	return(alldata)
}

#-----------------------------------------------------------------------------
# Generate legend labels 
#-----------------------------------------------------------------------------
RenameLabels <- function(q, digits, percent = FALSE){
	q <- formatC(q, digits = digits, format = "f")
	if (percent == TRUE) q <- paste(q, "%", sep = "")
	n <- length(q)
	labels <- array(NA, length(q) + 1)
	labels[1] <- paste("\u2264 ", q[1], sep = "")
	labels[n+1] <- paste("> ", q[n], sep = "")
	for (i in 2:(n)){
		labels[i] <- paste("(", q[i-1], ", ", q[i], "]", sep = "")
	}
	return(labels)
}

#-----------------------------------------------------------------------------
# Generate figure title for case map
#-----------------------------------------------------------------------------
GenerateCaseTitle <- function(v1, v2, v3, v4, v5){
	title <- paste(ifelse(v3 == "_new", "New ", "Weekly Change in New "), 
								 ifelse(v2 == "cases", "Cases ", "Deaths "), 
								 ifelse(v5 == "_a", "per 1M ", ""),
								 ifelse(v1 == TRUE, "by State ", "by County "), sep = "")
	if (v3 == "_new"){
		title <- paste(paste(title, "in the Week ", sep = ""), paste(v4-6, "~", v4, sep = ""), sep = "\n")
	}else{
		title <- paste(title, paste("from ", v4-13, "~", v4-7, " to ", v4-6, "~", v4, sep = ""), sep = "\n")
	}
	return(title)
}

#-----------------------------------------------------------------------------
# Generate figure title for moby.goog map
#-----------------------------------------------------------------------------
GenerateGoogTitle <- function(v1, v2, v3, v4){
	title <- paste(ifelse(v3 == "_change", "Weekly Change in Google's Mobility in ", "Google's Mobility in "), 
								 v2, 
								 ifelse(v1 == TRUE, " by State ", " by County "), sep = "")
	if (v3 == "_change"){
		title <- paste(title, paste("from ", v4-7, " to ", v4, sep = ""), sep = "\n")
	}else{
		title <- paste(title, "on ", v4, sep = "")
	}
	return(title)
}

#-----------------------------------------------------------------------------
# Generate figure title for moby.dl map
#-----------------------------------------------------------------------------
GenerateDLTitle <- function(v1, v2, v3){
	title <- paste(ifelse(v2 == "m50", "Descartes Labs' Mobility ", "Descartes Labs' Mobility Index "), 
								 ifelse(v1 == TRUE, "by State on ", "by County on "), 
								 v3, sep = "")
	return(title)
}

#-----------------------------------------------------------------------------
# Find approximate sample quantiles corresponding to the given probabilities
#-----------------------------------------------------------------------------
AppQuantile <- function(x, probs){
	x <- quantile(x, probs, na.rm = TRUE)
	y <- round(x/10^floor(log10(abs(x))))*10^floor(log10(abs(x)))
	y[is.na(y)] <- 0
	return(y)
}

#-----------------------------------------------------------------------------
# Generate week intervals from a date sequence
#-----------------------------------------------------------------------------
GenerateWeekInterval <- function(date_seq){
	date_seq <- sort(date_seq, decreasing = TRUE)
	n <- length(date_seq)
	week_inv <- paste(date_seq[2:n] + 1, " to ", date_seq[1:(n-1)], sep = "")
	return(week_inv)
}

#-----------------------------------------------------------------------------
# Define ggplot theme
#-----------------------------------------------------------------------------
my_theme <- function(){
	return(theme(plot.title = element_text(size = 18, hjust = 0.5, family = "Times"), 
				legend.position = "right", 
				legend.title = element_text(size = 14, family = "Times"),
				legend.text = element_text(size = 14, vjust = 0.5, family = "Times"))
				)
}
	

