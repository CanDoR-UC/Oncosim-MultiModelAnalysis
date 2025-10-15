dateRangeToShow <- 2020:2051


colours <- list()

colours["Breast"] <- "#E1549D"
colours["Lung"] <- "#404040"
colours["Colorectal"] <- "#022D89"
colours["Combined"] <- "#B28CD6"


ascendingStageColours <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026") #brewer.pal(5, name = "YlOrRd")

colours["Stage 0"] <- ascendingStageColours[1]
colours["Stage I"] <- ascendingStageColours[2]
colours["Stage II"] <- ascendingStageColours[3]
colours["Stage III"] <- ascendingStageColours[4]
colours["Stage IV"] <- ascendingStageColours[5]

colours["Limited (Small Cell Lung)"] <- "#818BBB"
colours["Extensive (Small Cell Lung)"] <- "#741C6D"


visualizeByTime <- function(dataset, variable, title, yAxisTitle) {
	dataset$ColumnToShow <- dataset[[variable]]
	dataset <- dataset %>% filter(Shiftable_year %in% dateRangeToShow)
	return(
		ggplot(dataset, aes(x = Shiftable_year, y = ColumnToShow, colour = formattedName)) +
			facet_grid(vars(cancertype), scales = "free") +
			geom_line(size = 2) +
			scale_x_continuous("Simulated Year") +
			scale_y_continuous(yAxisTitle, labels = scales::comma) +
			scale_colour_discrete("Scenario") +
			expand_limits(y = 0) +
			ggtitle(title) +
			theme_minimal() +
			theme(text = element_text(family = "Avenir", size = 20))
	)
}

visualizeParticipation <- function(dataset) {
	leftPlot <- ggplot(dataset %>% filter(grepl("_1_", sce) | grepl("BaseCase", sce)), aes(x = Shiftable_year, y = percentScreenedWithin1Years, colour = formattedName)) +
		facet_grid(vars(cancertype), scales = "free") +
		geom_line(size = 2) +
		scale_x_continuous("Simulated Year") +
		scale_y_continuous("Percent Screened Within 1 Year", labels = scales::percent) +
		scale_colour_discrete("Scenario") +
		expand_limits(y = c(0, 1)) +
		ggtitle("One Year Participation") +
		geom_hline(yintercept = 0.9, linetype = "dashed", colour = "blue") +
		geom_vline(xintercept = 2025, linetype = "dashed", colour = "red") +
		theme_minimal() +
		theme(text = element_text(family = "Avenir", size = 20))

	rightPlot <- ggplot(dataset %>% filter(grepl("_3_", sce) | grepl("BaseCase", sce)), aes(x = Shiftable_year, y = percentScreenedWithin3Years, colour = formattedName)) +
		facet_grid(vars(cancertype), scales = "free") +
		geom_line(size = 2) +
		scale_x_continuous("Simulated Year") +
		scale_y_continuous("Percent Screened Within 3 Year", labels = scales::percent) +
		scale_colour_discrete("Scenario") +
		expand_limits(y = c(0, 1)) +
		ggtitle("Three Year Participation") +
		geom_hline(yintercept = 0.3, linetype = "dashed", colour = "blue") +
		geom_vline(xintercept = 2025, linetype = "dashed", colour = "red") +
		theme_minimal() +
		theme(text = element_text(family = "Avenir", size = 20))

	ggarrange(leftPlot, rightPlot, nrow = 1)
}

visualizeCalibration <- function(dataset, variable, title, yAxisTitle) {
	dataset$ColumnToShow <- dataset[[variable]]
	dataset <- dataset %>% filter(Shiftable_year %in% dateRangeToShow)

	firstPlot = ggplot(dataset, aes(x = Shiftable_year, y = ColumnToShow, colour = cancertype)) +
		facet_grid(vars(formattedName), scales = "free") +
		geom_line(size = 2) +
		scale_x_continuous("Simulated Year") +
		scale_y_continuous(yAxisTitle, labels = scales::comma) +
		scale_colour_discrete("Scenario") +
		expand_limits(y = 0) +
		ggtitle(title) +
		theme_minimal() +
		theme(text = element_text(family = "Avenir", size = 20))

	secondPlot = ggplot(dataset, aes(x = Shiftable_year, y = ColumnToShow, colour = cancertype)) +
		facet_grid(vars(formattedName), scales = "free") +
		geom_smooth(size = 2) +
		scale_x_continuous("Simulated Year") +
		scale_y_continuous(yAxisTitle, labels = scales::comma) +
		scale_colour_discrete("Scenario") +
		expand_limits(y = 0) +
		ggtitle(title) +
		theme_minimal() +
		theme(text = element_text(family = "Avenir", size = 20))

	plot = ggarrange(firstPlot, secondPlot, nrow = 1)

	return(
		plot
	)
}


visualizeTotals <- function(dataset, variable, title, yAxisTitle) {
	dataset$ColumnToShow <- dataset[[variable]]
	dataset$Type <- ifelse(dataset$cancertype == "Combined", "Combined", "Cancertype Specific")
	return(
		ggplot(dataset, aes(x = formattedName, y = ColumnToShow, fill = cancertype)) +
			facet_grid(vars(Type), scales = "free") +
			geom_col(position = "dodge") +
			scale_x_discrete("Scenario") +
			scale_y_continuous(yAxisTitle, labels = scales::comma) +
			scale_fill_manual("Cancer Type", values = colours) +
			expand_limits(y = 0) +
			ggtitle(title) +
			theme_minimal() +
			theme(text = element_text(family = "Avenir", size = 20))
	)
}

visualizeStages <- function(dataset) {
	return(
		ggplot(dataset, aes(x = formattedName, y = expr_value, fill = expr_name)) +
			facet_grid(vars(cancertype), scales = "free") +
			geom_col(position = "stack") +
			scale_x_discrete("Scenario") +
			scale_fill_manual("Stage", values = colours) +
			scale_y_continuous("Distribution", labels = scales::percent) +
			expand_limits(y = 0) +
			ggtitle("Stage Distribution") +
			theme_minimal() +
			theme(text = element_text(family = "Avenir", size = 20))
	)
}
