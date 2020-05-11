# ------------------------------------------------------------------------------
# This file contains functions used to produce shiny outputs
# ------------------------------------------------------------------------------

# plot COVID-19 cases
plot_case <- function(var, data, coord.state){
	q <- AppQuantile(data[, unique(get(var))], seq(0.2, 0.8, 0.2))
	ggplot(data, aes(x, y, group = group)) +
		{if ("county" %in% names(data) == T) geom_polygon(aes(fill = findInterval(get(var), q)))} +
		{if ("county" %in% names(data) == F) geom_polygon_interactive(aes(fill = findInterval(get(var), q), tooltip = sprintf(c("%s<br>%1.0f"), state, get(var))))} +
		geom_polygon(data=coord.state, fill = NA, color = "black") +
		scale_fill_distiller(palette="Reds", direction=1, name="", na.value="white", labels=RenameLabels(q,0)) +
		ggtitle(paste("Changes in COVID-19 ", unlist(strsplit(var, split = "_"))[1], " since ", sub('.*(?=.$)', '', var, perl=T), " week(s) ago", sep = "")) +
		ggthemes::theme_map() + my_theme
}

# plot google mobility
plot_moby <- function(moby_type, data, coord.state){
	var <- switch(moby_type,
								"Grocery and Pharmacy" = "grocery",
								"Parks" = "parks",
								"Residential" = "residential",
								"Retail and Recreation" = "retail",
								"Transit" = "transit",
								"Workplaces" = "workplaces")
	q <- quantile(data[, unique(get(var))], seq(0.2, 0.8, 0.2), na.rm = TRUE)
	direction <- sign(min(data[, get(var)], na.rm=T))
	pals <- c("Blues", "RdYlBu", "Reds")
	pals.i <- sign(sum(sign(c(min(data[, get(var)], na.rm=T), max(data[, get(var)], na.rm=T))))) + 2
	ggplot(data, aes(x, y, group = group)) +
		{if ("county" %in% names(data) == T) geom_polygon(aes(fill = findInterval(get(var), q)))} +
		{if ("county" %in% names(data) == F) geom_polygon_interactive(aes(fill = findInterval(get(var), q), tooltip = sprintf(c("%s<br>%1.1f%%"), state, get(var))))} +
		geom_polygon(data=coord.state, fill = NA, color = "black") +
		scale_fill_distiller(palette=pals[pals.i], direction=direction, name="", na.value="white", labels=RenameLabels(q, 0, TRUE)) +
		ggtitle(paste("Changes in mobility in ", moby_type, " on ", unique(data$date), sep = "")) +
		ggthemes::theme_map() + my_theme
}

# plot DL mobility
plot_moby_dl <- function(moby_dl_type, data, coord.state){
	var <- switch(moby_dl_type,
								"median max-distance mobility" = "m50",
								"change in median max-distance mobility" = "m50_index")
	q <- quantile(data[, unique(get(var))], seq(0.2, 0.8, 0.2), na.rm = TRUE)
	ggplot(data, aes(x, y, group = group)) +
		{if ("county" %in% names(data) == T) geom_polygon(aes(fill = findInterval(get(var), q)))} +
		{if ("county" %in% names(data) == F) geom_polygon_interactive(aes(fill = findInterval(get(var), q), tooltip = sprintf(c("%s<br>%1.1f"), state, get(var))))} +
		geom_polygon(data=coord.state, fill = NA, color = "black") +
		scale_fill_distiller(palette="Reds", direction=1, name="", na.value="white", labels=RenameLabels(q, 0)) +
		ggtitle(paste("Max-distance mobility on ", unique(data$date), sep = "")) +
		ggthemes::theme_map() + my_theme
}

# plot traffic volume
plot_traf <- function(data){
	ggplot(data, aes(x = date)) +
		geom_line(aes(y = case_growth, color = "Case growth (%)"), size=1) +
		geom_line(aes(y = traf * 1 + 50, color = "Traffic decline (%)"), size=1) +
		scale_y_continuous(
			sec.axis = sec_axis( ~ . / 1 - 50, name = "Traffic decline (%)")
		) +
		scale_x_date(breaks = "1 week") +
		labs(y = "Case growth (%)", x = "", color = "") + 
		scale_color_manual(values = c("blue", "red")) +
		theme_classic() +		
		ggtitle(paste("Changes in traffic volume and growth of cases: ", unique(data$state), sep = "")) +
		theme(plot.title = element_text(size = 18, hjust = 0.5, family = "Times"), 
					legend.position = c(0.8, 0.8), 
					legend.title=element_text(size=14,family="Times"),
					legend.text=element_text(size=14,vjust=0.5,family="Times"),
					axis.text.x=element_text(size=14,family="Times",angle=45,hjust=1),
					axis.text.y=element_text(size=14,family="Times",hjust=1),
					axis.title=element_text(size=14,family="Times")) 
}

# plot Apple mobility
plot_moby_appl <- function(data){
	ggplot(data, aes(x = date)) +
		geom_line(aes(y = case_growth, color = "Case growth (%)"), size=1) +
		geom_line(aes(y = traf * 1 - 50, color = "Traffic decline (%)"), size=1) +
		scale_y_continuous(
			sec.axis = sec_axis( ~ . / 1 + 50, name = "Traffic decline (%)")
		) +
		scale_x_date(breaks = "1 week") +
		labs(y = "Case growth (%)", x = "", color = "") + 
		scale_color_manual(values = c("blue", "red")) +
		theme_classic() +		
		ggtitle(paste("Changes in traffic volume and growth of cases: ", unique(data$transportation_type), sep = "")) +
		theme(plot.title = element_text(size = 18, hjust = 0.5, family = "Times"), 
					legend.position = c(0.8, 0.8), 
					legend.title=element_text(size=14,family="Times"),
					legend.text=element_text(size=14,vjust=0.5,family="Times"),
					axis.text.x=element_text(size=14,family="Times",angle=45,hjust=1),
					axis.text.y=element_text(size=14,family="Times",hjust=1),
					axis.title=element_text(size=14,family="Times")) 
}

