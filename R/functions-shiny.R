# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This file contains shiny output functions
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#-------------------------------------------------------------------------------
# Plot COVID-19 data
#-----------------------------------------------------------------------------
plot_case <- function(coord, case, v1, v2, v3, v4, v5){
	var <- paste(v2, v3, v5, sep = "")
	data <- merge(coord, case[county %in% "State" == v1 & date == v4], by = "fips", all.x = TRUE)[!is.na(county)]
	q <- AppQuantile(data[, unique(get(var))], seq(0.2, 0.8, 0.2))
	ggplot(data, aes(x, y, group = group)) +
		{if (v1 == FALSE) geom_polygon(aes(fill = findInterval(get(var), q)))} +
		{if (v1 == TRUE) geom_polygon_interactive(aes(fill = findInterval(get(var), q), tooltip = sprintf(c("%s<br>%1.0f"), state, get(var))))} +
		geom_polygon(data=coord[fips < 1001], fill = NA, color = "black") +
		scale_fill_distiller(palette="RdYlBu", direction=-1, name="", na.value="white", labels=RenameLabels(q,0)) +
		ggtitle(GenerateCaseTitle(v1, v2, v3, v4, v5)) +
		ggthemes::theme_map() + my_theme()
}

#-----------------------------------------------------------------------------
# Plot Google mobility
#-----------------------------------------------------------------------------
plot_moby_goog <- function(coord, moby.goog, v1, v2, v3, v4){
	var <- paste(switch(v2, "Grocery and Pharmacy" = "grocery",
													"Parks" = "parks",
													"Residential" = "residential",
													"Retail and Recreation" = "retail",
													"Transit" = "transit",
													"Workplaces" = "workplaces"), v3, sep = "")
	data <- merge(coord, moby.goog[county %in% "State" == v1 & date == v4], by = "id", all.x = TRUE)[!is.na(county)]
	q <- quantile(data[, unique(get(var))], seq(0.2, 0.8, 0.2), na.rm = TRUE)
	ggplot(data, aes(x, y, group = group)) +
		{if (v1 == FALSE) geom_polygon(aes(fill = findInterval(get(var), q)))} +
		{if (v1 == TRUE) geom_polygon_interactive(aes(fill = findInterval(get(var), q), tooltip = sprintf(c("%s<br>%1.1f%%"), state, get(var))))} +
		geom_polygon(data=coord[fips < 1001], fill = NA, color = "black") +
		scale_fill_distiller(palette="RdYlBu", direction=-1, name="", na.value="white", labels=RenameLabels(q, 0, TRUE)) +
		ggtitle(GenerateGoogTitle(v1, v2, v3, v4)) +
		ggthemes::theme_map() + my_theme()
}

#-----------------------------------------------------------------------------
# Plot DL mobility
#-----------------------------------------------------------------------------
plot_moby_dl <- function(coord, moby.dl, v1, v2, v3){
	var <- v2
	formats <- ifelse(v2 == "m50", c("%s<br>%1.1f"), c("%s<br>%1.0f%%"))
	data <- merge(coord, moby.dl[county %in% "State" == v1 & date == v3], by = "fips", all.x = TRUE)[!is.na(county)]
	q <- quantile(data[, unique(get(var))], seq(0.2, 0.8, 0.2), na.rm = TRUE)
	ggplot(data, aes(x, y, group = group)) +
		{if (v1 == FALSE) geom_polygon(aes(fill = findInterval(get(var), q)))} +
		{if (v1 == TRUE) geom_polygon_interactive(aes(fill = findInterval(get(var), q), tooltip = sprintf(formats, state, get(var))))} +
		geom_polygon(data=coord[fips < 1001], fill = NA, color = "black") +
		{if (v2 == "m50") scale_fill_distiller(palette="RdYlBu", direction=1, name="", na.value="white", labels=RenameLabels(q, 0))} +
		{if (v2 != "m50") scale_fill_distiller(palette="RdYlBu", direction=1, name="", na.value="white", labels=RenameLabels(q, 0, TRUE))} +
		ggtitle(GenerateDLTitle(v1, v2, v3)) +
		ggthemes::theme_map() + my_theme()
}

#-----------------------------------------------------------------------------
# Plot Apple mobility
#-----------------------------------------------------------------------------
plot_moby_appl <- function(moby.appl, v1){
	data <- moby.appl[transportation_type == tolower(v1)]
	ggplot(data, aes(x = date, y = traf)) +
		geom_line(col = "blue", size = 1) +
		geom_hline(yintercept = 100) +
		scale_x_date(breaks = "1 week") +
		scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 50)) +
		labs(y = "", x = "", color = "") + 
		theme_classic() +		
		ggtitle(paste("Apple's Mobility in ", v1, " (2020-01-13=100)", sep = "")) + 
		my_theme() + 
		theme(axis.text.x=element_text(size=14,family="Times",angle=45,hjust=1),
					axis.text.y=element_text(size=14,family="Times",hjust=1),
					axis.title =element_text(size=14,family="Times"))
}

#-----------------------------------------------------------------------------
# Plot MS2 mobility
#-----------------------------------------------------------------------------
plot_moby_ms2 <- function(moby.ms2, v1){
	data <- moby.ms2[state == v1]
	ggplot(data, aes(x = date, y = traf+100)) +
		geom_line(col = "red", size = 1) +
		geom_line(data=moby.ms2[state == "National"], col = "blue", size = 1) +
		geom_hline(yintercept = 100) +
		scale_x_date(breaks = "1 week") +
		scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 25)) +
		labs(y = "", x = "", color = "") + 
		theme_classic() +		
		ggtitle(paste(paste("MS2's Mobility in ", v1, " (red) Compared to National Average (blue)", sep = ""),
									"(the same day of week in the same month of last year = 100)", sep = "\n")) +
		my_theme() + 
		theme(axis.text.x=element_text(size=14,family="Times",angle=45,hjust=1),
					axis.text.y=element_text(size=14,family="Times",hjust=1),
					axis.title =element_text(size=14,family="Times"))
}
