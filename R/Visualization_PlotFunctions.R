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
	plotList = list()

	dataset$Label = paste(dataset$minAge, dataset$testType, sep = "\n")

	oneYearData = dataset %>% filter(grepl("_1_", sce) | grepl("BaseCase", sce))
	leftPlot <- ggplot(oneYearData, aes(x = Shiftable_year, y = percentScreenedWithin1Years, colour = participation)) +
		facet_grid(vars(Label), scales = "free") +
		geom_line(size = 2) +
		scale_x_continuous("Simulated Year") +
		scale_y_continuous("Percent Screened Within 1 Year", labels = scales::percent, breaks = c(0.3, 0.5, 0.7, 0.9)) +
		scale_colour_discrete("Scenario") +
		expand_limits(y = c(0, 1)) +
		ggtitle("One Year Participation") +
		geom_hline(yintercept = 0.9, linetype = "dashed", colour = "blue") +
		geom_hline(yintercept = 0.7, linetype = "dashed", colour = "blue") +
		geom_hline(yintercept = 0.5, linetype = "dashed", colour = "blue") +
		geom_hline(yintercept = 0.3, linetype = "dashed", colour = "blue") +
		geom_vline(xintercept = 2025, linetype = "dashed", colour = "red") +
		theme_minimal() +
		theme(text = element_text(family = "Avenir", size = 20))

	threeYearData = dataset %>% filter(grepl("_3_", sce) | grepl("BaseCase", sce))
	rightPlot <- ggplot(dataset %>% filter(grepl("_3_", sce) | grepl("BaseCase", sce)), aes(x = Shiftable_year, y = percentScreenedWithin3Years, colour = participation)) +
		facet_grid(vars(Label), scales = "free") +
		geom_line(size = 2) +
		scale_x_continuous("Simulated Year") +
		scale_y_continuous("Percent Screened Within 3 Year", labels = scales::percent, breaks = c(0.3, 0.5, 0.7, 0.9)) +
		scale_colour_discrete("Scenario") +
		expand_limits(y = c(0, 1)) +
		ggtitle("Three Year Participation") +
		geom_hline(yintercept = 0.9, linetype = "dashed", colour = "blue") +
		geom_hline(yintercept = 0.7, linetype = "dashed", colour = "blue") +
		geom_hline(yintercept = 0.5, linetype = "dashed", colour = "blue") +
		geom_hline(yintercept = 0.3, linetype = "dashed", colour = "blue") +
		geom_vline(xintercept = 2025, linetype = "dashed", colour = "red") +
		theme_minimal() +
		theme(text = element_text(family = "Avenir", size = 20))

	ggarrange(leftPlot, rightPlot, nrow = 1)
}


visualizeTotals <- function(dataset, variable, title, yAxisTitle) {
	dataset$ColumnToShow <- dataset[[variable]]
	dataset$Type <- ifelse(dataset$cancertype == "Combined", "Combined", "Cancer Types")
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

visualizeFrontier <- function(frontierData, type) {
	frontierData <- frontierData %>%
		mutate(Type = "Cost-efficient")

	variableToUse = "Health_adjusted_person_years"

	if (variableToUse == "HAPY_PerPerson_WithDeaths") {
		yLabel = "Health Adjusted Person Years Per Person (+ Deaths)"
	}
	if (variableToUse == "HAPY_PerPerson") {
		yLabel = "Health Adjusted Person Years Per Person"
	}
	if (variableToUse == "Health_adjusted_person_years") {
		yLabel = "Health Adjusted Person Years"
	}

	frontierData$yVar = frontierData[[variableToUse]]

	frontierData <- frontierData[order(frontierData$Total_Cost), ]

	frontierDominated <- data.frame()
	frontierCount <- 1
	continueDominateChecking <- TRUE

	while (continueDominateChecking & frontierCount < 30 & nrow(frontierData) > 2) {
		frontierData$delta_cost <- 0
		frontierData$delta_HAPY <- 0
		for (r in 2:nrow(frontierData)) {
			frontierData$delta_HAPY[r] <- frontierData$yVar[r] -
				frontierData$yVar[r - 1]
			frontierData$delta_cost[r] <- frontierData$Total_Cost[r] -
				frontierData$Total_Cost[r - 1]
		}

		frontierDominated <- bind_rows(
			frontierDominated,
			frontierData %>%
				filter((delta_HAPY < 0 & delta_cost > 0)) %>%
				mutate(Type = paste("Rejected due to dominance (Round ", frontierCount, ")", sep = ""))
		)
		currentLength <- nrow(frontierData)
		frontierData <- frontierData %>%
			filter(!sce %in% frontierDominated$sce)

		if (currentLength == nrow(frontierData)) {
			continueDominateChecking <- FALSE
		} else {
			frontierCount <- frontierCount + 1
		}
	}

	frontierPlotData <- bind_rows(frontierData, frontierDominated)
	million <- 1000000

	frontierPlotData$Labels <- ifelse(frontierPlotData$Type == "Cost-efficient", frontierPlotData$sce, "")
	ggplot(frontierPlotData, aes(y = yVar, x = Total_Cost / million, colour = Type, shape = testType)) +
		geom_point(size = 3) +
		geom_line(data = frontierData %>% filter(Type == "Cost-efficient"), aes(group = Type)) +
		geom_text(aes(
			label = Labels,
			y = 1.000005 * yVar
		)) +
		scale_y_continuous(
			yLabel,
			labels = scales::comma
		) +
		scale_x_continuous(
			"Total Screening and Treatment Cost (Millions $CAD)",
			labels = scales::dollar
		) +
		scale_shape_discrete("Test Type") +
		scale_colour_discrete("") +
		theme_minimal(13, "Nunito")
}

visualizePoints <- function(totalData, x_var, y_var) {
	totalData$x = totalData[[x_var]]
	totalData$y = totalData[[y_var]]

	ggplot(totalData, aes(y = y, x = x, colour = testType)) +
		geom_point(size = 3) +
		geom_label(aes(
			label = sce,
			y = y
		)) +
		scale_y_continuous(
			y_var,
			labels = scales::comma
		) +
		scale_x_continuous(
			x_var,
			labels = scales::comma
		) +
		scale_shape_discrete("Test Type") +
		scale_colour_discrete("") +
		theme_minimal(13, "Nunito")
}
