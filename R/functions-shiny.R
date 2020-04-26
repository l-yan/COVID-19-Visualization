# ------------------------------------------------------------------------------
# This file contains functions used to produce shiny outputs
# ------------------------------------------------------------------------------

# plot COVID-19 cases
plot_case <- function(var, data, coord.state){
	q <- quantile(data[, get(var)], seq(0.2, 0.8, 0.2), na.rm = TRUE)
	name <- "Percent"
	if (var=="cases"){name<-""}
	ggplot(data, aes(x, y, group = group)) +
		geom_polygon(aes(fill = findInterval(get(var), q))) +
		geom_polygon(data=coord.state, fill = NA, color = "black") +
		scale_fill_distiller(palette="Reds", direction=1, name=name, na.value="white", labels=RenameLabels(q,0)) +
		ggthemes::theme_map() + 
		theme(legend.position="right", legend.title=element_text(size=16,family="Times New Roman"),
					legend.text=element_text(size=12,vjust=0.5,family="Times New Roman"))
}


# plot google mobility
plot_moby <- function(var, data, coord.state){
	q <- quantile(data[, get(var)], seq(0.2, 0.8, 0.2), na.rm = TRUE)
	direction <- -1
	pal <- "RdYlBu"
	if (max(q)<0){
		direction <- -1
		pal <- "Blues"
	}
	if (min(q)>0){
		direction <- 1
		pal <- "Reds"
	}
	ggplot(data, aes(x, y, group = group)) +
		geom_polygon(aes(fill = findInterval(get(var), q))) +
		geom_polygon(data=coord.state, fill = NA, color = "black") +
		scale_fill_distiller(palette=pal, direction=direction, name="Percent", na.value="white", labels=RenameLabels(q,0)) +
		ggthemes::theme_map() + 
		theme(legend.position="right", legend.title=element_text(size=16,family="Times New Roman"),
					legend.text=element_text(size=12,vjust=0.5,family="Times New Roman"))
}

# plot traffic volume
plot_traf <- function(data, coord.state){
	q <- seq(-50, 10, 10)
	data[, fills := findInterval(traf, q)]
	ggplot(data, aes(x, y, group = group)) +
		geom_polygon(aes(fill = fills)) +
		geom_polygon(data=coord.state, fill = NA, color = "black") +
		scale_fill_distiller(palette="Blues", direction=-1, name="%", na.value="white", labels=RenameLabels(q,0)[sort(unique(data$fills))+1]) +
		ggthemes::theme_map() + 
		theme(legend.position="right", legend.title=element_text(size=16,family="Times New Roman"),
					legend.text=element_text(size=12,vjust=0.5,family="Times New Roman"))
}

# Analysis
plot_traf_case <- function(data){
	ggplot(data, aes(x = date)) +
		geom_line(aes(y = case_growth, color = "Case growth")) +
		geom_line(aes(y = traf * 1 + 50, color = "traffic decline")) +
		scale_y_continuous(
			breaks = seq(0, 50, 10),
			sec.axis = sec_axis( ~ . / 1 - 50, name = "Traffic decline (%)")
		) +
		scale_x_date(breaks = "1 week") +
		labs(
			y = "Case growth (%)",
			x = "",
			color = ""
		) + theme_classic() +
		theme(legend.position = c(0.8, 0.8), 
					legend.title=element_text(size=16,family="Times New Roman"),
					legend.text=element_text(size=14,vjust=0.5,family="Times New Roman"),
					axis.text.x=element_text(size=14,family="Times New Roman",angle=45,hjust=1)) + 
		scale_color_manual(values = c("blue", "red"))
}
