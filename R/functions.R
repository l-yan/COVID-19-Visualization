# ------------------------------------------------------------------------------
# This file contains helper functions 
# ------------------------------------------------------------------------------

# download online data and save them in folder 'data'
Data_Update <- function(){
	
	# US COVID-19 data from New York Times
	# by state
	if (as.Date(file.info("data/case_state.csv")$ctime) < Sys.Date()){
		write.csv(x = fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", showProgress = FALSE), 
							file = "data/case_state.csv")
		print("COVID-19 cases by state have been successfully updated!")
	}else{
		print("COVID-19 cases by state are up to date!")
	}
	# by county
	if (as.Date(file.info("data/case_county.csv")$mtime) < Sys.Date()){
		write.csv(x = fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", showProgress = FALSE), 
							file = "data/case_county.csv")
		print("COVID-19 cases by county have been successfully updated!")
	}else{
		print("COVID-19 cases by county are up to date!")
	}
	
	# Mobility reports from Google
	if (as.Date(file.info("data/moby_goog.csv")$mtime) < Sys.Date()){
		write.csv(x = fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", showProgress = FALSE), 
							file = "data/moby_goog.csv")
		print("Mobility reports from Google have been successfully updated!")
	}else{
		print("Mobility reports from Google are up to date!")
	}
	
	# Mobility reports from Descartes Labs
	if (as.Date(file.info("data/moby_dl.csv")$mtime) < Sys.Date()){
		write.csv(x = fread("https://raw.githubusercontent.com/descarteslabs/DL-COVID-19/master/DL-us-mobility-daterow.csv", showProgress = FALSE), 
							file = "data/moby_dl.csv")
		print("Mobility reports from Descartes Labs have been successfully updated!")
	}else{
		print("Mobility reports from Descartes Labs are up to date!")
	}
	
}

# process data
Data_Process <- function(){
	
	# COVID-19 cases by state
	case.state <- fread("data/case_state.csv")[, !c('V1', 'fips'), with=FALSE]
	setorder(case.state, state, -date)
	for (i in 1:4){
		case.state[, paste("cases_w", i, sep = "") := cases - shift(cases, i*7, NA, "lead"), by = state]
		case.state[, paste("deaths_w", i, sep = "") := deaths - shift(deaths, i*7, NA, "lead"), by = state]
	}
	
	# COVID-19 cases by county
	case.cnty <- fread("data/case_county.csv")[, !c('V1'), with=FALSE]
	case.cnty <- case.cnty[!is.na(fips)] # some cases have no fips
	setorder(case.cnty, fips, -date)
	for (i in 1:4){
		case.cnty[, paste("cases_w", i, sep = "") := cases - shift(cases, i*7, NA, "lead"), by = fips]
		case.cnty[, paste("deaths_w", i, sep = "") := deaths - shift(deaths, i*7, NA, "lead"), by = fips]
	}
	
	# Google mobility
	moby <- fread("data/moby_goog.csv")
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
	
	# Descartes Labs mobility
	moby.dl <- fread("data/moby_dl.csv")
	moby.dl[, date := as.Date(date)]
	moby.dl <- moby.dl[, c(1, 4:9)]
	setnames(moby.dl, old = c("admin1", "admin2"), new = c("state", "county"))
	
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

	# Apple mobility
	moby.appl <- fread("data/moby_appl.csv")
	moby.appl <- melt(moby.appl, measure.vars = 5:ncol(moby.appl), variable.name = "date", value.name = "traf")
	moby.appl[, date := as.Date(date)]
	moby.appl <- moby.appl[region == "United States"]
	
	# exports
	out <- list(case.state = case.state, 
							case.cnty = case.cnty, 
							moby.state = moby.state, 
							moby.cnty = moby.cnty, 
							coord.state = coord.state, 
							coord.cnty = coord.cnty,
							traf.state = traf.state,
							moby.appl = moby.appl,
							moby.dl = moby.dl)
	return(out)
}

# generate legend labels 
RenameLabels <- function(q, digits, percent=FALSE){
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

# remove day of week effects
RemoveDayOfWeek <- function(x, date){
	D <- matrix(0, length(date), 7)
	for (i in 1:7){
		D[wday(date)==i, i] <- 1
	}
	return(residuals(lm(x ~ D - 1)))
}

# Approximate quantile
AppQuantile <- function(x, q){
	x <- quantile(x, q, na.rm = TRUE)
	y <- round(x/10^floor(log10(abs(x))))*10^floor(log10(abs(x)))
	y[is.na(y)] <- 0
	return(y)
}


