library(tidyverse)
library(ggplot2)


dataDirectory = "Output/Runs"

yearsToFilterWith = 2025:2051

getOncoSimTables <- function(tableName, modelType) {
	if (!grepl(".csv", tableName)) {
		tableName = paste(tableName, ".csv", sep = "")
	}

	allScenarioNames = list.dirs(dataDirectory)
	allScenarioNames = allScenarioNames[allScenarioNames != dataDirectory]

	totalTable = data.frame()
	for (scenario in allScenarioNames) {
		if (grepl(modelType, scenario)) {
			currentTable = read.csv(file.path(scenario, tableName))
			name = strsplit(scenario, paste("OncoSim-", modelType, "_", sep = ""))[[1]][2]
			currentTable$sce = name
			currentTable$cancertype = str_to_title(modelType)
			currentTable$formattedName = name

			if (!grepl("BaseCase", name)) {
				nameSplit <- strsplit(name, "_")[[1]]
				currentTable$minAge <- nameSplit[1]
				currentTable$frequency <- nameSplit[2]
				currentTable$participation <- as.numeric(nameSplit[3])
				currentTable$testType <- nameSplit[4]
				currentTable$formattedName = paste(currentTable$testType, "\n PPT: ", currentTable$participation, "%\n", "MinAge: ", currentTable$minAge, "\nFreq: ", currentTable$frequency, " Years", sep = "")
				currentTable$formattedName_NoTest = paste("PPT: ", currentTable$participation, "%\n", "MinAge: ", currentTable$minAge, "\nFreq: ", currentTable$frequency, " Years", sep = "")
			} else {
				currentTable$minAge <- "BaseCase"
				currentTable$frequency <- "BaseCase"
				currentTable$participation <- 0
				currentTable$testType <- "BaseCase"
			}
			totalTable = bind_rows(totalTable, currentTable)
		}
	}
	return(totalTable)
}

extractGeneralMetrics <- function(importedData) {
	#No idea how person years should be factored in?
	#Maybe by adding together the differences from base case?

	exprRename = list()
	exprRename["Breast_cancer_detected_by_screen"] = "Detected_Cases"
	exprRename["Screen_detected_cancers"] = "Detected_Cases"
	exprRename["Screen_detecte_x_over_detected"] = "Detected_Cases"

	exprRename["Deaths_cause_specific"] = "Cancer_Deaths"
	exprRename["Lung_cancer_deaths"] = "Cancer_Deaths"

	exprRename["Number_of_breast_cancer_screens"] = "Total_Screens"
	exprRename["Total_screens_x_iple_per_visit"] = "Total_Screens"
	exprRename["Total_screens_attended"] = "Total_Screens"

	exprRename["Total_cost_as_x_and_treatment"] = "Total_Cost"
	exprRename["Total_cost_sc_x_ng_treatment"] = "Total_Cost"
	exprRename["Total_cost_va_x_all_treatment"] = "Total_Cost"
	exprRename["Total_cost"] = "Total_Cost"

	exprRename["Cost_of_screening"] = "Screening_Cost"
	exprRename["Cost_of_screen_x_reen_positives"] = "Screening_Cost"
	exprRename["Cost_of_screen_x_tected_cancers"] = "Screening_Cost"

	exprRename["Health_adjusted_person_years"] = "Health_adjusted_person_years"
	exprRename["Person_years"] = "Person_years"

	exprRename["Persons"] = "Persons"

	#Each should be a multiple of 37
	#Importing each table and filtering. Should be 37 * number of scenarios * number of unique expr_name

	breastCasesTable = importedData$Breast_Cancer_Cases_Table %>% filter(expr_name %in% c("Breast_cancer_detected_by_screen", "Deaths_cause_specific"), Hackable_province == "all") %>% select(-Hackable_province)
	breastScreenTable = importedData$Breast_Cancer_Screen_Outcome_Modality_Table %>%
		filter(expr_name %in% c("Number_of_breast_cancer_screens"), Breast_screening_modalities == "all", Age == "all", Hackable_province == "all", Shiftable_year != "all") %>%
		mutate(Shiftable_year = as.numeric(Shiftable_year)) %>%
		select(-c(Hackable_province, Breast_screening_modalities, Age))

	breastICERTable = importedData$Breast_Cancer_ICER_Table %>% filter(expr_name %in% c("Total_cost_as_x_and_treatment", "Cost_of_screening", "Person_years", "Health_adjusted_person_years", "Person_years", "Persons"), Province == "all", Shiftable_year %in% 2015:2051) %>% select(-Province)

	colorectalScreenTable = importedData$Colorectal_Cancer_Screen_Table %>% filter(expr_name %in% c("Screen_detected_cancers", "Total_screens_x_iple_per_visit"), Hackable_province == "all", Shiftable_year != "all") %>% mutate(Shiftable_year = as.numeric(Shiftable_year)) %>% select(-Hackable_province)
	colorectalCasesTable = importedData$Colorectal_Cancer_Cases_Table %>% filter(expr_name %in% c("Deaths_cause_specific"), Province == "all", Sex == "all", Colorectal_cancer_site == "all") %>% mutate(Shiftable_year = as.numeric(Shiftable_year)) %>% select(-c(Colorectal_cancer_site, Province, Sex))
	colorectalICERTable = importedData$Colorectal_Cancer_ICER_Table %>% filter(expr_name %in% c("Total_cost_sc_x_ng_treatment", "Cost_of_screen_x_reen_positives", "Person_years", "Health_adjusted_person_years", "Person_years", "Persons"), Province == "all", Shiftable_year %in% 2015:2051) %>% select(-Province)

	lungCasesTable = importedData$Lung_Cancer_ScreenCases_Year_Table %>% filter(expr_name %in% c("Screen_detecte_x_over_detected", "Lung_cancer_deaths", "Total_screens_attended", "Persons"), Hackable_province == "all", Year != "all") %>% mutate(Shiftable_year = as.numeric(Year)) %>% select(-c(Hackable_province, Year))
	lungICERTable = importedData$Lung_Cancer_ICER_Table %>%
		filter(expr_name %in% c("Total_cost", "Cost_of_screen_x_tected_cancers", "Person_years", "Health_adjusted_person_years", "Person_years"), Province == "all", Shiftable_year %in% 2015:2051, lc_screen_participated_ever == "all", lc_cessation_p_x_rticipated_ever == "all") %>%
		select(-c(Province, lc_screen_participated_ever, lc_cessation_p_x_rticipated_ever))

	cancerMetricsTable = bind_rows(breastCasesTable, breastScreenTable, breastICERTable, colorectalCasesTable, colorectalScreenTable, colorectalICERTable, lungCasesTable, lungICERTable)

	cancerMetricsTable$expr_name = as.character(exprRename[cancerMetricsTable$expr_name])

	cancerMetricsTable = cancerMetricsTable %>% pivot_wider(names_from = expr_name, values_from = expr_value)

	cancerMetricsTable
}

extractParticipation <- function(importedData) {
	ageGroups = c("[50,60)", "[60,70)", "[70,75)")

	breastParticipation = importedData$Breast_Cancer_Timing_Of_Last_Screen %>%
		filter(age_group %in% ageGroups, Shiftable_year != "all") %>%
		group_by(Time_of_last_screen_years, Shiftable_year, sce, minAge, frequency, participation, testType, formattedName, formattedName_NoTest, cancertype) %>%
		summarize(expr_value = sum(expr_value)) %>%
		pivot_wider(names_from = Time_of_last_screen_years, values_from = expr_value) %>%
		mutate(
			percentScreenedWithin3Years = (Last_screen_within_0_1 + Last_screen_within_1_2 + Last_screen_within_2_3) / all,
			percentScreenedWithin2Years = (Last_screen_within_0_1 + Last_screen_within_1_2) / all,
			percentScreenedWithin1Years = (Last_screen_within_0_1) / all,
			Shiftable_year = as.numeric(Shiftable_year)
		)

	colonParticipation = importedData$Colorectal_Cancer_Timing_Of_Last_Screen %>%
		filter(Colorectal_cancer_family_history == "all", age_group %in% ageGroups, expr_name == "Number_of_people", Shiftable_year != "all") %>%
		group_by(Time_of_last_screen_years, Shiftable_year, sce, minAge, frequency, participation, testType, formattedName, formattedName_NoTest, cancertype) %>%
		summarize(expr_value = sum(expr_value)) %>%
		pivot_wider(names_from = Time_of_last_screen_years, values_from = expr_value) %>%
		mutate(
			percentScreenedWithin3Years = (Last_screen_within_0_1 + Last_screen_within_1_2 + Last_screen_within_2_3) / all,
			percentScreenedWithin2Years = (Last_screen_within_0_1 + Last_screen_within_1_2) / all,
			percentScreenedWithin1Years = (Last_screen_within_0_1) / all,
			Shiftable_year = as.numeric(Shiftable_year)
		)

	lungParticipation = importedData$Lung_Cancer_Timing_Of_Last_Screen %>%
		filter(age_group %in% ageGroups, Shiftable_year != "all") %>%
		group_by(Time_of_last_screen_years, Shiftable_year, sce, minAge, frequency, participation, testType, formattedName, formattedName_NoTest, cancertype) %>%
		summarize(expr_value = sum(expr_value)) %>%
		pivot_wider(names_from = Time_of_last_screen_years, values_from = expr_value) %>%
		mutate(
			percentScreenedWithin3Years = (Last_screen_within_0_1 + Last_screen_within_1_2 + Last_screen_within_2_3) / all,
			percentScreenedWithin2Years = (Last_screen_within_0_1 + Last_screen_within_1_2) / all,
			percentScreenedWithin1Years = (Last_screen_within_0_1) / all,
			Shiftable_year = as.numeric(Shiftable_year)
		)

	bind_rows(lungParticipation, colonParticipation, breastParticipation)
}


createCombinedCancerTypesData <- function(cancerMetricsTable) {
	#Not great, but just multiplying resource usage (Screening Cost, Screen Num) by 2. Add Screening_Cost to total cost to simulate double the screening cost (since males are also theoretically being screened)
	femaleOnlyTable = cancerMetricsTable %>% filter(cancertype %in% c("Breast")) %>% mutate(Total_Screens = Total_Screens * 2, Screening_Cost = Screening_Cost * 2, Total_Cost = Total_Cost + Screening_Cost)
	allSexTable = cancerMetricsTable %>% filter(cancertype %in% c("Lung", "Colorectal"))

	#For MCED, clinical attributes (deaths, cases, TotalCost-ScreeningCost) must be summed. Resource usage (Screening Cost, number of screens) must be averaged
	cancerMetricsTableAllCancerMCED = bind_rows(femaleOnlyTable, allSexTable) %>%
		select(-cancertype) %>%
		filter(sce != "BaseCase" & !grepl("NonMCED", sce)) %>%
		group_by(Shiftable_year, sce, formattedName, formattedName_NoTest, minAge, frequency, participation, testType) %>%
		summarize(
			Detected_Cases = sum(Detected_Cases),
			Cancer_Deaths = sum(Cancer_Deaths),
			Total_Screens = mean(Total_Screens),
			Total_Cost = sum(Total_Cost - Screening_Cost) + mean(Screening_Cost),
			Screening_Cost = mean(Screening_Cost),
			Person_years = sum(Person_years),
			Persons = sum(Persons)
		) %>%
		mutate(cancertype = "Combined")

	cancerMetricsTableAllCancerBaseCase = cancerMetricsTable %>% #Don't need to multiply breast x2 for base case
		select(-cancertype) %>%
		filter(sce == "BaseCase" | grepl("NonMCED", sce)) %>%
		group_by(Shiftable_year, sce, formattedName, formattedName_NoTest, minAge, frequency, participation, testType) %>%
		summarize(
			Detected_Cases = sum(Detected_Cases),
			Cancer_Deaths = sum(Cancer_Deaths),
			Total_Screens = sum(Total_Screens),
			Total_Cost = sum(Total_Cost),
			Screening_Cost = sum(Screening_Cost),
			Person_years = sum(Person_years),
			Persons = sum(Persons)
		) %>%
		mutate(cancertype = "Combined")

	combinedCancerTable = bind_rows(cancerMetricsTableAllCancerMCED, cancerMetricsTableAllCancerBaseCase)
}


generateStageTables <- function(importedData) {
	#Stage data (Total of stages should add up to incidence_Cases)

	breastCases = importedData$Breast_Cancer_StageDist_AY_Table %>% filter(expr_name == "Incidence_cases") %>% mutate(cases = expr_value) %>% select(-c("expr_name", "expr_value"))
	breastStages = importedData$Breast_Cancer_StageDist_AY_Table %>%
		filter(!expr_name %in% c("Incidence_cases", "Person_years")) %>%
		left_join(breastCases) %>%
		mutate(expr_value = expr_value * cases) %>% # Make into full number
		filter(Age == "all", Shiftable_year %in% yearsToFilterWith) %>%
		select(-c("Age", "Shiftable_year")) %>%
		group_by(sce, cancertype, formattedName, formattedName_NoTest, minAge, frequency, participation, testType, expr_name) %>%
		summarize(expr_value = sum(expr_value), cases = sum(cases)) %>% # Get sum of totals
		mutate(expr_value = expr_value / cases) %>% #Turn back into ratio
		select(-cases)

	colonCases = importedData$Colorectal_Cancer_StageDist_Table %>% filter(expr_name == "Incidence_cases") %>% mutate(cases = expr_value) %>% select(-c("expr_name", "expr_value"))
	colonStages = importedData$Colorectal_Cancer_StageDist_Table %>%
		filter(!expr_name %in% c("Incidence_cases", "Person_years")) %>%
		left_join(colonCases) %>%
		mutate(expr_value = expr_value * cases) %>% # Make into full number
		filter(Age == "all", Sex == "all", Colorectal_cancer_site == "all", Shiftable_year %in% yearsToFilterWith) %>%
		select(-c("Age", "Sex", "Colorectal_cancer_site", "Shiftable_year")) %>%
		group_by(sce, cancertype, formattedName, formattedName_NoTest, minAge, frequency, participation, testType, expr_name) %>%
		summarize(expr_value = sum(expr_value), cases = sum(cases)) %>% # Get sum of totals
		mutate(expr_value = expr_value / cases) %>% #Turn back into ratio
		select(-cases)

	lungStages = importedData$Lung_Cancer_StageDist_Table

	stageTable = bind_rows(breastStages, colonStages, lungStages)
	stageTable$cancertype <- str_to_title(stageTable$cancertype)
	stageTable$cancertype <- factor(stageTable$cancertype, levels = c("Breast", "Colorectal", "Lung"))

	stageTable$expr_name <- gsub("Stage_IA", "Stage_I", stageTable$expr_name) #For lung, these are non-small cell
	stageTable$expr_name <- gsub("Stage_IB", "Stage_I", stageTable$expr_name)

	stageTable$expr_name <- gsub("Stage_IIA", "Stage_II", stageTable$expr_name)
	stageTable$expr_name <- gsub("Stage_IIB", "Stage_II", stageTable$expr_name)

	stageTable$expr_name <- gsub("Stage_IIIA", "Stage_III", stageTable$expr_name)
	stageTable$expr_name <- gsub("Stage_IIIB", "Stage_III", stageTable$expr_name)
	stageTable$expr_name <- gsub("Stage_IIIC", "Stage_III", stageTable$expr_name)

	stageTable$expr_name <- gsub("Extensive", "Extensive (Small Cell Lung)", stageTable$expr_name)
	stageTable$expr_name <- gsub("Limited", "Limited (Small Cell Lung)", stageTable$expr_name)

	stageTable$expr_name <- gsub("_", " ", stageTable$expr_name)

	stageTable$expr_name <- factor(stageTable$expr_name, levels = c("Stage 0", "Stage I", "Stage II", "Stage III", "Stage IV", "Limited (Small Cell Lung)", "Extensive (Small Cell Lung)"))
	stageTable
}

combineModelTables <- function(importedData) {
	processedModelResults <- list()

	cancerMetricsTable = extractGeneralMetrics(importedData)

	#No idea how person years should be factored in?
	#Maybe by adding together the differences from base case?

	combinedCancerMetricsTable = createCombinedCancerTypesData(cancerMetricsTable)

	cancerMetricsTable = bind_rows(cancerMetricsTable, combinedCancerMetricsTable)

	cancerMetricsTable$cancertype <- str_to_title(cancerMetricsTable$cancertype)
	cancerMetricsTable$cancertype <- factor(cancerMetricsTable$cancertype, levels = c("Breast", "Colorectal", "Lung", "Combined"))

	cancerMetricsTableBaseCase <- cancerMetricsTable %>% filter(sce == "BaseCase") %>% select(-c(sce, formattedName, formattedName_NoTest, minAge, frequency, participation, testType))
	names(cancerMetricsTableBaseCase) = paste("BASECASE", names(cancerMetricsTableBaseCase), sep = "_")
	names(cancerMetricsTableBaseCase) = gsub("BASECASE_Shiftable_year", "Shiftable_year", names(cancerMetricsTableBaseCase))
	names(cancerMetricsTableBaseCase) = gsub("BASECASE_cancertype", "cancertype", names(cancerMetricsTableBaseCase))

	cancerMetricsTable <- cancerMetricsTable %>%
		group_by(Shiftable_year, cancertype) %>%
		left_join(cancerMetricsTableBaseCase)

	createDifference = c("Detected_Cases", "Cancer_Deaths", "Total_Screens", "Total_Cost", "Screening_Cost", "Person_years")

	for (variable in createDifference) {
		cancerMetricsTable[[paste(variable, "Diff", sep = "_")]] = cancerMetricsTable[[variable]] - cancerMetricsTable[[paste("BASECASE", variable, sep = "_")]]
	}

	cancerMetricsTable = cancerMetricsTable[, !grepl("BASECASE", names(cancerMetricsTable))]

	processedModelResults["PrimaryTable"] = list(cancerMetricsTable)

	cancerMetricsTableCombinedYears = cancerMetricsTable %>%
		group_by(sce, formattedName, formattedName_NoTest, minAge, frequency, participation, testType, cancertype) %>%
		filter(Shiftable_year %in% yearsToFilterWith) %>%
		select(-Shiftable_year) %>%
		summarize(
			Detected_Cases = sum(Detected_Cases),
			Cancer_Deaths = sum(Cancer_Deaths),
			Total_Screens = sum(Total_Screens),
			Total_Cost = sum(Total_Cost),
			Screening_Cost = sum(Screening_Cost),
			Detected_Cases_Diff = sum(Detected_Cases_Diff),
			Cancer_Deaths_Diff = sum(Cancer_Deaths_Diff),
			Total_Screens_Diff = sum(Total_Screens_Diff),
			Total_Cost_Diff = sum(Total_Cost_Diff),
			Screening_Cost_Diff = sum(Screening_Cost_Diff),
			Health_adjusted_person_years = sum(Health_adjusted_person_years),
			Person_years = sum(Person_years),
			Person_years_Diff = sum(Person_years_Diff),

			Persons = sum(Persons)
		)
	#Add stage analysis.

	cancerMetricsTableCombinedYears$ICER_ReducedDeaths = cancerMetricsTableCombinedYears$Total_Cost_Diff / (cancerMetricsTableCombinedYears$Cancer_Deaths_Diff * -1)
	cancerMetricsTableCombinedYears$ICER_DetectedCases = cancerMetricsTableCombinedYears$Total_Cost_Diff / (cancerMetricsTableCombinedYears$Detected_Cases_Diff)
	cancerMetricsTableCombinedYears$ICER_PersonYears = cancerMetricsTableCombinedYears$Total_Cost_Diff / (cancerMetricsTableCombinedYears$Person_years_Diff)

	#One row per set of years
	if (!(nrow(cancerMetricsTableCombinedYears) * length(yearsToFilterWith) == nrow(cancerMetricsTable %>% filter(Shiftable_year %in% yearsToFilterWith)))) {
		stop("ERROR. NOT ADDING UP TO CORRECT VALUE")
	}

	processedModelResults["PrimaryTable_2025-2051"] = list(cancerMetricsTableCombinedYears)

	processedModelResults["StageDist"] = list(generateStageTables(importedData))
	processedModelResults["Participation"] = list(extractParticipation(importedData))

	processedModelResults
}
