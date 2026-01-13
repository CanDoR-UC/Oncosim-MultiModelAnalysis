dateRangeToShow <- 2020:2051


colours <- list()

colours["Breast"] <- "#E1549D"
colours["Lung"] <- "#818181"
colours["Colorectal"] <- "#0046de"
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

	twoYearData = dataset %>% filter(grepl("_2_", sce) | grepl("BaseCase", sce))
	leftPlot <- ggplot(twoYearData, aes(x = Shiftable_year, y = percentScreenedWithin2Years, colour = as.character(participation))) +
		facet_grid(vars(Label), scales = "free") +
		geom_line(size = 2) +
		scale_x_continuous("Simulated Year") +
		scale_y_continuous("Percent Screened Within 2 Year", labels = scales::percent, breaks = c(0.3, 0.5, 0.7, 0.9)) +
		scale_colour_discrete("Scenario") +
		expand_limits(y = c(0, 1)) +
		ggtitle("Two Year Participation") +
		geom_hline(yintercept = 0.9, linetype = "dashed", colour = "blue") +
		geom_hline(yintercept = 0.7, linetype = "dashed", colour = "blue") +
		geom_hline(yintercept = 0.5, linetype = "dashed", colour = "blue") +
		geom_hline(yintercept = 0.3, linetype = "dashed", colour = "blue") +
		geom_vline(xintercept = 2025, linetype = "dashed", colour = "red") +
		theme_minimal() +
		theme(text = element_text(family = "Avenir", size = 20))

	threeYearData = dataset %>% filter(grepl("_3_", sce) | grepl("BaseCase", sce))
	rightPlot <- ggplot(dataset %>% filter(grepl("_3_", sce) | grepl("BaseCase", sce)), aes(x = Shiftable_year, y = percentScreenedWithin3Years, colour = as.character(participation))) +
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


formatNumber <- function(values) {
	billion = 1000000000
	million = 1000000
	thousand = 1000
	if (median(abs(values)) > billion) {
		return(scales::comma(values / billion, accuracy = 0.1, suffix = "B"))
	} else if (abs(median(values)) > million) {
		return(scales::comma(values / million, accuracy = 0.1, suffix = "M"))
	} else if (abs(median(values)) > thousand) {
		return(scales::comma(values / thousand, accuracy = 0.1, suffix = "K"))
	}
	return(scales::comma(values, accuracy = 0.1))
}


visualizeTotals <- function(dataset, variable, title, yAxisTitle) {
	dataset$ColumnToShow <- dataset[[variable]]
	dataset$Type <- ifelse(dataset$cancertype == "Combined", "Combined", "Cancer Types")

	position = "dodge"
	text_position = position_dodge(width = 0.9)
	text_position_divide = 2
	if (abs(sum(dataset$ColumnToShow[dataset$Type == "Combined"]) - sum(dataset$ColumnToShow[dataset$Type != "Combined"])) < 1) {
		dataset = dataset %>% filter(Type != "Combined")
		position = "stack"
		text_position = position_stack(vjust = 0.5)
		text_position_divide = 1
	}

	dataset$formattedName_NoTest[dataset$testType == "BaseCase"] = "BaseCase"

	return(
		ggplot(dataset, aes(x = formattedName_NoTest, y = ColumnToShow, fill = cancertype)) +
			geom_hline(yintercept = 0, linetype = "dashed") +
			geom_col(position = position) +
			facet_grid(rows = vars(testType), scales = "free_x") +
			scale_x_discrete("Scenario") +
			scale_y_continuous(yAxisTitle, labels = scales::comma) +
			scale_fill_manual("Cancer Type", values = colours) +
			geom_text(aes(label = formatNumber(ColumnToShow), y = ColumnToShow / text_position_divide), colour = "black", family = "Avenir", size = 8, position = text_position) +
			expand_limits(y = 0) +
			ggtitle(title) +
			theme_minimal() +
			theme(text = element_text(family = "Avenir", size = 25))
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

	variableToUse = "Person_years"

	if (variableToUse == "HAPY_PerPerson_WithDeaths") {
		yLabel = "Health Adjusted Person Years Per Person (+ Deaths)"
	}
	if (variableToUse == "HAPY_PerPerson") {
		yLabel = "Health Adjusted Person Years Per Person"
	}
	if (variableToUse == "Health_adjusted_person_years") {
		yLabel = "Health Adjusted Person Years"
	}
	if (variableToUse == "Person_years") {
		yLabel = "Person Years"
	}

	frontierData$yVar = frontierData[[variableToUse]]

	frontierData <- frontierData[order(frontierData$Total_Cost), ]

	frontierDominated <- data.frame()
	frontierCount <- 1
	continueDominateChecking <- TRUE

	while (continueDominateChecking & frontierCount < 30 & nrow(frontierData) > 1) {
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

	frontierPlotData$Labels <- ifelse(frontierPlotData$Type == "Cost-efficient", frontierPlotData$formattedName, "")
	frontierPlotData$Size <- ifelse(frontierPlotData$Type == "Cost-efficient", 3, 1)

	ggplot(frontierPlotData, aes(y = yVar, x = Total_Cost / million, colour = Type, shape = testType)) +
		geom_point(size = 3) +
		geom_line(data = frontierData %>% filter(Type == "Cost-efficient"), aes(group = Type), colour = "black", size = 2) +
		geom_label_repel(
			aes(
				label = Labels,
				y = 1.000005 * yVar
			),
			size = 7,
			family = "Nunito",
			colour = "black"
		) +
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

visualizeSpecificCancerType <- function(totalData, typeToShow) {
	currentDataToVisualize <- totalData %>%
		filter(cancertype == typeToShow, testType != "BaseCase") %>%
		pivot_longer(c(Detected_Cases_Diff, Cancer_Deaths_Diff, Total_Cost_Diff, ICER_ReducedDeaths, ICER_DetectedCases, ICER_PersonYears))

	plotsToCombine <- list()

	variableNames = list()
	variableNames["Detected_Cases_Diff"] = "Difference in\nDetected Cases"
	variableNames["ICER_ReducedDeaths"] = "Difference in Cost\nPer Reduction in Cancer Deaths"
	variableNames["Cancer_Deaths_Diff"] = "Difference in\nCancer Deaths"
	variableNames["ICER_DetectedCases"] = "Difference in Cost\nPer Additional Detection"
	variableNames["ICER_PersonYears"] = "Difference in Cost\nPer Additional Person Year"
	variableNames["Total_Cost_Diff"] = "Difference in\nTotal Cost"

	for (variable in unique(currentDataToVisualize$name)) {
		variableName <- variableNames[variable][[1]]
		plotsToCombine[variable] <- list(
			ggplot(currentDataToVisualize %>% filter(name == variable), aes(x = formattedName, y = value, fill = testType)) +
				geom_bar(stat = "identity") +
				theme_minimal(16, "Avenir") +
				geom_text(aes(label = formatNumber(value), y = value / 2), colour = "black", family = "Avenir", size = 5) +
				scale_y_continuous(variableName) +
				scale_x_discrete("Scenario") +
				scale_fill_discrete("Test Type")
		)
	}
	ggarrange(plotlist = plotsToCombine, common.legend = TRUE)
}

visualizeLinearModel <- function(combinedTables, testToShow, outcome, exposure) {
	currentTable <- combinedTables$`PrimaryTable_2025-2051` %>%
		filter(cancertype == "Combined")

	currentTable$Label <- paste(currentTable$testType, "\nFreq: ", currentTable$frequency, " Years", sep = "")
	currentTable$Label[currentTable$testType == "BaseCase"] <- "BaseCase"

	currentTable$ExposureVar <- currentTable[[exposure]]
	currentTable$OutcomeVar <- currentTable[[outcome]]

	outcomeTitle <- str_to_title(gsub("_", " ", outcome))
	exposureTitle <- str_to_title(gsub("_", " ", exposure))

	linearRegressionResults <- data.frame()

	baseCaseValue <- currentTable[currentTable$testType == "BaseCase", ]$OutcomeVar

	for (label in unique(currentTable$Label)) {
		for (age in unique(currentTable$minAge)) {
			if (age != "BaseCase" & label != "BaseCase" & grepl(testToShow, label)) {
				filteredTable <- currentTable %>% filter(minAge == age, Label == label)
				model <- lm(OutcomeVar ~ ExposureVar, data = filteredTable)

				currentResults <- data.frame(Label = label, minAge = age)
				currentResults$intercept <- coef(model)[1]
				currentResults$slope <- coef(model)[2]

				currentResults$BaseCaseIntercept <- (baseCaseValue - currentResults$intercept) / currentResults$slope

				linearRegressionResults <- bind_rows(linearRegressionResults, currentResults)
			}
		}
	}

	ggplot(
		currentTable %>% filter(testType == testToShow),
		aes(x = ExposureVar, y = OutcomeVar, colour = Label)
	) +
		geom_point() +
		geom_smooth(method = "lm", aes(linetype = minAge)) +
		scale_y_continuous(outcomeTitle, label = scales::number) +
		scale_x_continuous(exposureTitle, label = scales::number) +
		geom_hline(yintercept = baseCaseValue) +
		geom_label_repel(data = linearRegressionResults, aes(label = scales::number(BaseCaseIntercept, accuracy = 0.1), x = BaseCaseIntercept), y = baseCaseValue, family = "Avenir") +
		theme_minimal(16, "Avenir")
}
