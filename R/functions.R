# ------------------------------------------------------------------------------
# This file contains functions used to help data processing
# ------------------------------------------------------------------------------

# download online data and save them in folder 'data'
Data_Download <- function(){
	require(data.table)
	# US COVID-19 case data from nytimes (https://github.com/nytimes/covid-19-data)
	# nytimes updates this data every day
	dat <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
	write.csv(dat, file = "data/case_state.csv")
	dat <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
	write.csv(dat, file = "data/case_county.csv")
	
	# Google mobility report (updated every Sunday)
	dat <- fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
	write.csv(dat, file = "data/moby.csv")
	
}

# process data
Data_Process <- function(){
	
	# COVID-19 cases by state
	case.state <- fread("data/case_state.csv")[, !c('V1', 'fips'), with=FALSE]
	setorder(case.state, state, -date)
	case.state[, ln_cases := pmax(0, log(cases))] # log(0) = 0
	case.state[, ln_cases_d1 := 100*(ln_cases-shift(ln_cases, 1, 0, "lead")), by=state]
	case.state[, ln_cases_d3 := 100*(ln_cases-shift(ln_cases, 3, 0, "lead")), by=state]
	case.state[, ln_cases_d7 := 100*(ln_cases-shift(ln_cases, 7, 0, "lead")), by=state]
	case.state[, ln_cases_d14 :=100*(ln_cases-shift(ln_cases,14, 0, "lead")), by=state]
	case.state[, ln_cases_d30 :=100*(ln_cases-shift(ln_cases,30, 0, "lead")), by=state]
	case.state <- case.state[date==max(date)]
	
	# COVID-19 cases by county
	case.cnty <- fread("data/case_county.csv")[, !c('V1', 'deaths'), with=FALSE]
	case.cnty <- case.cnty[!is.na(fips)] # some cases have no fips
	setorder(case.cnty, fips, -date)
	case.cnty[, ln_cases := log(cases)]
	case.cnty[, ln_cases_d1 := 100*(ln_cases-shift(ln_cases, 1, 0, "lead")), by=fips]
	case.cnty[, ln_cases_d3 := 100*(ln_cases-shift(ln_cases, 3, 0, "lead")), by=fips]
	case.cnty[, ln_cases_d7 := 100*(ln_cases-shift(ln_cases, 7, 0, "lead")), by=fips]
	case.cnty[, ln_cases_d14 :=100*(ln_cases-shift(ln_cases,14, 0, "lead")), by=fips]
	case.cnty[, ln_cases_d30 :=100*(ln_cases-shift(ln_cases,30, 0, "lead")), by=fips]
	case.cnty <- case.cnty[date==max(date)]
	
	# Google mobility
	moby <- fread("data/moby.csv")
	moby[, date := as.Date(date)]
	setnames(moby, c("V1", "country_code", "country_name", "state", "county", "date",
									 "retail", "grocery", "parks", "transit", "workplaces", "residential"))
	
	# Google mobility by state
	moby.state <- moby[country_code=="US" & state!="" & county=="", c(4,6:12)]
	
	# Google mobility by county
	moby.cnty <- moby[country_code=="US" & county!="", 4:12]
	moby.cnty[county=="Doña Ana County", county := "Dona Ana County"]
	moby.cnty[county=="Shannon County" & state=="South Dakota", county := "Oglala Lakota County"]
	moby.cnty[, id := tolower(paste(state, county, sep = ","))]

	# US state coordinates
	coord.state <- data.table(us_map("state"))
	setnames(coord.state, "full", "state")
	coord.state <- coord.state[, list(x, y, group, state)]
	
	# US county coordinates
	coord.cnty <- data.table(us_map("county"))
	coord.cnty[, fips := as.numeric(fips)]
	coord.cnty[, id := tolower(paste(full, gsub(" city", "", county), sep = ","))]
	coord.cnty <- coord.cnty[, list(x, y, group, fips, id)]
	
	# Traffic volume by state
	dat <- unlist(c(fread("data/traffic_new.txt", header = F, quote="")))
	dat <- gsub("\"", "", dat)
	dat <- gsub("\\{State:", "", dat)
	dat <- gsub("CountDate:", "", dat)
	dat <- gsub("PercentChange:", "", dat)
	dat <- gsub("PercentChangeTruck:", "", dat)
	dat <- gsub("}", "", dat)
	index <- 1:length(dat) %% 4
	traf.state.new <- data.table(state = dat[index==1], date = dat[index==2], traf = dat[index==3])
	traf.state.new[, date := as.Date(date)]
	traf.state.new[, traf := as.numeric(traf)]
	traf.state.old <- fread("data/traffic_old.csv")
	traf.state.old[, date := as.Date(date, "%m/%d/%y")]
	traf.state.old <- melt(traf.state.old, id.vars = "date", variable.name = "state", value.name = "traf") 
	traf.state <- rbind(traf.state.new, traf.state.old[date < min(traf.state.new$date), c(2, 1, 3)])
	tmp <- unique(data.table(us_map("state"))[,list(abbr,full)], by="abbr")
	traf.state$state <- tmp$full[match(traf.state$state, tmp$abbr)]
	traf.state[is.na(state), state := "National"]

	# exports
	out <- list(case.state = case.state, 
							case.cnty = case.cnty, 
							moby.state = moby.state, 
							moby.cnty = moby.cnty, 
							coord.state = coord.state, 
							coord.cnty = coord.cnty,
							traf.state = traf.state)
	return(out)
}

# generate legend labels 
RenameLabels <- function(q, digits){
	q <- formatC(round(q, digits))
	n <- length(q)
	labels <- array(NA, length(q) + 1)
	labels[1] <- paste("\u2264 ", q[1], sep = "")
	labels[n+1] <- paste("> ", q[n], sep = "")
	for (i in 2:(n)){
		labels[i] <- paste("(", q[i-1], ", ", q[i], "]", sep = "")
	}
	return(labels)
}

# log zero
log1 <- function(x){
	y <- log(x)
	y[!is.finite(y)] <- 0
	return(y)
}

