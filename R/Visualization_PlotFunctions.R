dateRangeToShow <- 2020:2051
scenariosToVisualize <- c("50_3_70_MCC", "50_3_70_MCG", "50_3_70_Non-MC", "45_2_90_MCC", "45_2_90_MCG", "45_2_90_Non-MC", "BaseCase")

colours <- list()

colours["Breast"] <- "#E1549D"
colours["Lung"] <- "#ffe645"
colours["Colorectal"] <- "#0046de"
colours["Combined"] <- "#B28CD6"

colours["Visualized"] <- "#1c9326"
colours["Not Visualized"] <- "#000000"

colours["Interval: 2 Years"] <- "#00394c"
colours["Interval: 3 Years"] <- "#00ace6"


colours["Cost-efficient"] <- "#1c9326"
colours["Rejected due to dominance"] <- "#c52929"
colours["Rejected due to extended dominance"] <- "#d1a237"

ascendingStageColours <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026") #brewer.pal(5, name = "YlOrRd")

colours["Stage 0"] <- ascendingStageColours[1]
colours["Stage I"] <- ascendingStageColours[2]
colours["Stage II"] <- ascendingStageColours[3]
colours["Stage III"] <- ascendingStageColours[4]
colours["Stage IV"] <- ascendingStageColours[5]

colours["Limited (Small Cell Lung)"] <- "#818BBB"
colours["Extensive (Small Cell Lung)"] <- "#741C6D"


colours["MCC"] <- "#c47aec"
colours["MCG"] <- "#d1ab43"
colours["Non-MC"] <- "#bbbbbb"


fontFam <- "Atkinson Hyperlegible"

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
			theme(text = element_text(family = fontFam, size = 20))
	)
}

visualizeParticipation <- function(dataset) {
	plotList <- list()

	dataset$Label <- paste(dataset$minAge, dataset$testType, sep = "\n")

	twoYearData <- dataset %>% filter(grepl("_2_", sce) | grepl("BaseCase", sce))
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
		theme(text = element_text(family = fontFam, size = 20))

	threeYearData <- dataset %>% filter(grepl("_3_", sce) | grepl("BaseCase", sce))
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
		theme(text = element_text(family = fontFam, size = 20))

	ggarrange(leftPlot, rightPlot, nrow = 1)
}


formatNumber <- function(values, facetVar = NULL) {
	#return(scales::comma(values, accuracy = 1))
	toReturn <- c()

	formatItems <- list()
	formatItems["B"] <- 1000000000
	formatItems["M"] <- 1000000
	formatItems["K"] <- 1000
	formatItems[""] <- 0

	for (item in values) {
		for (potentialFormat in names(formatItems)) {
			absValue <- abs(item)
			currentDivide <- ifelse(potentialFormat == "", 0, formatItems[potentialFormat][[1]])
			if (!is.na(item)) {
				if (absValue >= currentDivide) {
					currentVal <- scales::comma(ifelse(currentDivide > 0, item / currentDivide, item), accuracy = 0.1, suffix = potentialFormat)
					if (nchar(currentVal) > 20 | potentialFormat == "") {
						currentVal <- scales::comma(ifelse(currentDivide > 0, item / currentDivide, item), accuracy = 1, suffix = potentialFormat)
					}

					toReturn <- c(toReturn, currentVal)
					break
				}
			} else {
				toReturn <- c(toReturn, NA)
				break
			}
		}
	}
	#if (!is.null(facetVar)) {
	#		toReturn[grepl("Cost", facetVar)] = paste("$", toReturn[grepl("Cost", facetVar)], sep = "")#
	#}

	return(toReturn)
}
formatBarPosition <- function(allValues, facetVar) {
	toReturn <- allValues
	for (name in unique(facetVar)) {
		values <- allValues[facetVar == name]
		maxValue <- max(abs(values))
		cutOffValue <- maxValue / 8
		newValue <- cutOffValue * 1.2

		currentReturn <- c()
		for (value in values) {
			if (abs(value) > cutOffValue) {
				currentReturn <- c(currentReturn, value / 2)
			} else {
				currentReturn <- c(currentReturn, ifelse(value >= 0, newValue, -1 * newValue))
			}
		}
		toReturn[facetVar == name] <- currentReturn
	}
	toReturn
}

visualizeTotals <- function(dataset, variable, title, yAxisTitle) {
	dataset$ColumnToShow <- dataset[[variable]]
	dataset$Type <- ifelse(dataset$cancertype == "Combined", "Combined", "Cancer Types")

	position <- "dodge"
	text_position <- position_dodge(width = 0.9)
	text_position_divide <- 2

	dataset$formattedName_NoTest[dataset$testType == "BaseCase"] <- "BaseCase"

	return(
		ggplot(dataset, aes(x = formattedName_NoTest, y = ColumnToShow, fill = cancertype)) +
			geom_hline(yintercept = 0, linetype = "dashed") +
			geom_col(position = position) +
			facet_grid(rows = vars(testType), scales = "free_x") +
			scale_x_discrete("Scenario") +
			scale_y_continuous(yAxisTitle, labels = scales::comma) +
			scale_fill_manual("Cancer Type", values = colours) +
			geom_text_repel(aes(label = formatNumber(ColumnToShow), y = ColumnToShow / text_position_divide), colour = "black", family = fontFam, size = 8, position = text_position) +
			expand_limits(y = 0) +
			ggtitle(title) +
			theme_minimal() +
			theme(text = element_text(family = fontFam, size = 25))
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
			theme(text = element_text(family = fontFam, size = 20))
	)
}

visualizeFrontier <- function(frontierData, type) {
	frontierData <- frontierData %>%
		mutate(Type = "Cost-efficient", ToVisualize = ifelse(sce %in% scenariosToVisualize, "Visualized", "Not Visualized")) %>%
		filter(cancertype == type)

	variableToUse <- "Person_years"

	if (variableToUse == "HAPY_PerPerson_WithDeaths") {
		yLabel <- "Health Adjusted Person Years Per Person (+ Deaths)"
	}
	if (variableToUse == "HAPY_PerPerson") {
		yLabel <- "Health Adjusted Person Years Per Person"
	}
	if (variableToUse == "Health_adjusted_person_years") {
		yLabel <- "Health Adjusted Person Years"
	}
	if (variableToUse == "Person_years") {
		yLabel <- "Person Years"
	}

	frontierData$yVar <- frontierData[[variableToUse]]

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

		dominanceType <- "Rejected due to extended dominance"
		if (frontierCount == 1) {
			dominanceType <- "Rejected due to dominance"
		}
		frontierDominated <- bind_rows(
			frontierDominated,
			frontierData %>%
				filter((delta_HAPY < 0 & delta_cost > 0)) %>%
				mutate(Type = dominanceType)
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
	billion <- 1000000000

	refValue <- frontierPlotData[frontierPlotData$formattedName == "BaseCase", ]$YVar

	#frontierPlotData$Labels <- ifelse(frontierPlotData$Type == "Cost-efficient", paste(formatNumber(frontierPlotData$Total_Cost), formatNumber(frontierPlotData$yVar), frontierPlotData$formattedName), "")
	#frontierPlotData$Labels <- ifelse(frontierPlotData$Type == "Cost-efficient", paste(frontierPlotData$formattedName, formatNumber(100000 * frontierPlotData$Total_Cost / frontierPlotData$yVar), round(frontierPlotData$Total_Cost / billion), formatNumber(frontierPlotData$Person_years_Diff), sep = "\n"), "")
	frontierPlotData$Labels <- ifelse(frontierPlotData$Type == "Cost-efficient", paste(frontierPlotData$formattedName), "")

	frontierPlotData$Size <- ifelse(frontierPlotData$Type == "Cost-efficient", 3, 1)

	ggplot(frontierPlotData, aes(y = yVar, x = Total_Cost / billion, colour = Type, shape = testType)) +
		geom_point(size = 5) +
		geom_line(data = frontierData %>% filter(Type == "Cost-efficient"), aes(group = Type), colour = "black", size = 2) +
		geom_label_repel(
			aes(
				label = Labels,
				force = 5,
				y = 1.000005 * yVar #,
				#	colour = ToVisualize
			),
			size = 4,
			family = fontFam,
			colour = "black"
		) +
		scale_y_continuous(
			yLabel,
			labels = scales::comma,
		) +
		scale_x_continuous(
			"Total Screening and Treatment Cost (Billions $CAD)",
			labels = scales::dollar
		) +
		scale_shape_discrete("Test Type") +
		scale_colour_manual("", values = colours) +
		theme_minimal(16, fontFam) +
		ggtitle(type) +
		guides(colour = guide_legend(nrow = 2, byrow = TRUE), shape = guide_legend(nrow = 2, byrow = TRUE))
}

visualizePoints <- function(totalData, x_var, y_var) {
	totalData$x <- totalData[[x_var]]
	totalData$y <- totalData[[y_var]]

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
		theme_minimal(13, fontFam)
}

generateMaxToUse <- function(values) {
	maximumVal <- max(values, na.rm = TRUE)
	secondMax <- max(values[values != maximumVal], na.rm = TRUE)

	valueToUseAsMax <- maximumVal

	if (maximumVal > secondMax * 5) {
		valueToUseAsMax <- secondMax
	}

	return(valueToUseAsMax)
}

visualizeSpecificCancerType <- function(totalData, typeToShow) {
	rowCount <- 4
	if (typeToShow != "Combined") {
		currentDataToVisualize <- totalData %>%
			filter(cancertype == typeToShow, testType != "BaseCase") %>%
			#		pivot_longer(c(Detected_Cases_Diff, Cancer_Deaths_Diff, Total_Cost_Diff, ICER_ReducedDeaths, ICER_DetectedCases, ICER_PersonYears))
			pivot_longer(c(Detected_Cases_Diff, Cancer_Deaths_Diff, Total_Cost_Diff, Health_adjusted_person_years_Diff, Person_years_Diff, ICER_DetectedCases, ICER_PersonYears, ICER_HAPY)) %>%
			filter(!is.na(value))
	} else {
		currentDataToVisualize <- totalData %>%
			filter(cancertype == typeToShow, testType != "BaseCase") %>%
			#		pivot_longer(c(Detected_Cases_Diff, Cancer_Deaths_Diff, Total_Cost_Diff, ICER_ReducedDeaths, ICER_DetectedCases, ICER_PersonYears))
			pivot_longer(c(Detected_Cases_Diff, Cancer_Deaths_Diff, Total_Cost_Diff, Person_years_Diff, ICER_DetectedCases, ICER_PersonYears)) %>%
			filter(!is.na(value))
		rowCount <- 3
	}

	plotsToCombine <- list()

	variableNames <- list()
	variableNames["Detected_Cases_Diff"] <- "Difference in Detected Cases"
	variableNames["Cancer_Deaths_Diff"] <- "Difference in Cancer Deaths"
	variableNames["Total_Cost_Diff"] <- "Difference in Total Cost"
	if (typeToShow != "Combined") {
		variableNames["Health_adjusted_person_years_Diff"] <- "Difference in Health\nAdjusted Person Years"
	}
	variableNames["Person_years_Diff"] <- "Difference in Person Years"

	variableNames["ICER_DetectedCases"] <- "Difference in Cost Per\nAdditional Detection"
	variableNames["ICER_ReducedDeaths"] <- "Difference in Cost Per\nReduction in Cancer Deaths"
	variableNames["ICER_PersonYears"] <- "Difference in Cost Per\n100,000 Additional Person Years"
	variableNames["ICER_HAPY"] <- "Difference in Cost Per Additional\n100,000 Health Adjusted Person Years"

	dataToUse <- currentDataToVisualize %>% filter(name %in% names(variableNames))

	for (name in names(variableNames)) {
		dataToUse$name[dataToUse$name == name] <- variableNames[name][[1]]
	}
	dataToUse$name <- factor(dataToUse$name, levels = as.character(variableNames))

	ggplot(dataToUse, aes(x = formattedName_NoTest, y = value, fill = testType, group = testType)) +
		facet_wrap(vars(name), scales = "free_y", ncol = 2) +
		geom_bar(position = "dodge", stat = "identity") +
		theme_minimal(28, fontFam) +
		geom_hline(aes(yintercept = 0), linetype = 2) +
		geom_text(aes(label = formatNumber(value, name), y = formatBarPosition(value, name)), fill = "white", colour = "black", family = fontFam, size = 9, position = position_dodge(width = 0.9)) +
		scale_y_continuous("", label = formatNumber) +
		scale_x_discrete("") +
		scale_fill_manual("Test Type", values = colours) +
		theme(strip.text = element_text(size = 30), legend.position = "right")
}

visualizeLinearModel <- function(combinedTables, testToShow, outcome, exposure) {
	cancerType <- "Combined"

	currentTable <- combinedTables$`PrimaryTable_2025-2051` %>%
		filter(cancertype == cancerType, testType %in% c(testToShow, "BaseCase"))

	currentTable$Label <- paste("Interval: ", currentTable$frequency, " Years", sep = "")
	currentTable$Label[currentTable$testType == "BaseCase"] <- "BaseCase"

	currentTable$ExposureVar <- currentTable[[exposure]]
	currentTable$OutcomeVar <- currentTable[[outcome]]

	outcomeTitle <- str_to_title(gsub("_", " ", outcome))
	exposureTitle <- str_to_title(gsub("_", " ", exposure))

	linearRegressionResults <- data.frame()

	baseCaseValue <- currentTable[currentTable$testType == "BaseCase", ]$OutcomeVar

	for (label in unique(currentTable$Label)) {
		for (age in unique(currentTable$minAge)) {
			if (age != "BaseCase" & label != "BaseCase") {
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

	write.csv(linearRegressionResults, paste("Tables/RegressionResults_", testToShow, "_", cancerType, ".csv", sep = ""))

	ggplot(
		currentTable %>% filter(testType == testToShow),
		aes(x = ExposureVar, y = OutcomeVar, colour = Label)
	) +
		geom_point(size = 2) +
		geom_smooth(method = "lm", aes(linetype = minAge), se = FALSE, linewidth = 2) +
		scale_y_continuous(outcomeTitle, label = scales::comma, expand = c(0, 0)) +
		scale_x_continuous(exposureTitle, label = scales::comma) +
		geom_hline(yintercept = baseCaseValue) +
		geom_label_repel(data = linearRegressionResults, aes(label = scales::percent(BaseCaseIntercept / 100, accuracy = 0.1), x = BaseCaseIntercept), y = baseCaseValue, family = fontFam, size = 6, colour = "black") +
		scale_colour_manual("Interval", values = colours) +
		theme_minimal(20, fontFam) +
		scale_linetype_discrete("Minimum Age") +
		ggtitle(testToShow)
}
