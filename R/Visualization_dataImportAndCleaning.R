library(tidyverse)
library(ggplot2)


dataDirectory = "Output/Runs"


getCombinedTable <- function(tableName, modelType) {
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
			currentTable$cancertype = modelType
			currentTable$formattedName = name

			if (!grepl("BaseCase", name)) {
				nameSplit <- strsplit(name, "_")[[1]]
				currentTable$minAge <- nameSplit[1]
				currentTable$frequency <- nameSplit[2]
				currentTable$participation <- nameSplit[3]
				currentTable$testType <- nameSplit[4]
				currentTable$formattedName = paste(currentTable$testType, "\n PPT: ", currentTable$participation, "%\n", "MinAge: ", currentTable$minAge, "\nFreq: ", currentTable$frequency, " Years", sep = "")
			}
			totalTable = bind_rows(totalTable, currentTable)
		}
	}
	return(totalTable)
}


combineModelTables <- function(importedData) {
	combinedTables <- list()

	exprRename = list()
	exprRename["Breast_cancer_detected_by_screen"] = "Detected_Cases"
	exprRename["Screen_detected_cancers"] = "Detected_Cases"
	exprRename["Cervical_cance_x_ected_by_screen"] = "Detected_Cases"
	exprRename["Screen_detecte_x_over_detected"] = "Detected_Cases"

	exprRename["Deaths_cause_specific"] = "Cancer_Deaths"
	exprRename["Lung_cancer_deaths"] = "Cancer_Deaths"

	exprRename["Number_of_breast_cancer_screens"] = "Total_Screens"
	exprRename["Total_screens_x_iple_per_visit"] = "Total_Screens"
	exprRename["Primary_cytology_screening_count"] = "Total_Screens"
	exprRename["Total_screens_attended"] = "Total_Screens"

	exprRename["Total_cost_as_x_and_treatment"] = "Total_Cost"
	exprRename["Total_cost_sc_x_ng_treatment"] = "Total_Cost"
	exprRename["Total_cost_va_x_all_treatment"] = "Total_Cost"
	exprRename["Total_cost"] = "Total_Cost"

	exprRename["Cost_of_screening"] = "Screening_Cost"
	exprRename["Cost_of_screen_x_reen_positives"] = "Screening_Cost"
	exprRename["Cost_of_screen_x_tected_cancers"] = "Screening_Cost"

	exprRename["Person_years"] = "Person_Years"

	breastCasesTable = importedData$Breast_Cancer_Cases_Table %>% filter(expr_name %in% c("Breast_cancer_detected_by_screen", "Deaths_cause_specific"), Hackable_province == "all") %>% select(-Hackable_province)
	breastScreenTable = importedData$Breast_Cancer_Screen_Outcome_Modality_Table %>%
		filter(expr_name %in% c("Number_of_breast_cancer_screens"), Breast_screening_modalities == "all", Age == "all", Hackable_province == "all", Shiftable_year != "all") %>%
		mutate(Shiftable_year = as.numeric(Shiftable_year)) %>%
		select(-c(Hackable_province, Breast_screening_modalities, Age))

	breastICERTable = importedData$Breast_Cancer_ICER_Table %>% filter(expr_name %in% c("Total_cost_as_x_and_treatment", "Cost_of_screening", "Person_years"), Province == "all", Shiftable_year %in% 2015:2051) %>% select(-Province)

	colorectalScreenTable = importedData$Colorectal_Cancer_Screen_Table %>% filter(expr_name %in% c("Screen_detected_cancers", "Total_screens_x_iple_per_visit"), Hackable_province == "all", Shiftable_year != "all") %>% mutate(Shiftable_year = as.numeric(Shiftable_year)) %>% select(-Hackable_province)
	colorectalCasesTable = importedData$Colorectal_Cancer_Cases_Table %>% filter(expr_name %in% c("Deaths_cause_specific"), Province == "all", Sex == "all", Colorectal_cancer_site == "all") %>% mutate(Shiftable_year = as.numeric(Shiftable_year)) %>% select(-c(Colorectal_cancer_site, Province, Sex))
	colorectalICERTable = importedData$Colorectal_Cancer_ICER_Table %>% filter(expr_name %in% c("Total_cost_sc_x_ng_treatment", "Cost_of_screen_x_reen_positives", "Person_years"), Province == "all", Shiftable_year %in% 2015:2051) %>% select(-Province)

	cervicalCasesTable = importedData$Cervical_Cancer_Cases_Table %>% filter(expr_name %in% c("Cervical_cance_x_ected_by_screen", "Deaths_cause_specific"), Hackable_province == "all", Shiftable_year %in% 2015:2051) %>% select(-Hackable_province)
	cervicalScreenTable = importedData$Hpv_Screening_Costs_Prov_Table %>% filter(expr_name %in% c("Primary_cytology_screening_count"), Sex == "all", Hackable_province == "all", Shiftable_year %in% 2015:2051) %>% mutate(Shiftable_year = as.numeric(Shiftable_year)) %>% select(-c(Hackable_province, Sex))
	cervicalICERTable = importedData$Cervical_Cancer_ICER_Table %>% filter(expr_name %in% c("Total_cost_va_x_all_treatment", "Cost_of_screening", "Person_years"), Province == "all", Shiftable_year %in% 2015:2051) %>% select(-Province)

	lungCasesTable = importedData$Lung_Cancer_ScreenCases_Year_Table %>% filter(expr_name %in% c("Screen_detecte_x_over_detected", "Lung_cancer_deaths", "Total_screens_attended"), Hackable_province == "all", Year != "all") %>% mutate(Shiftable_year = as.numeric(Year)) %>% select(-c(Hackable_province, Year))
	lungICERTable = importedData$Lung_Cancer_ICER_Table %>%
		filter(expr_name %in% c("Total_cost", "Cost_of_screen_x_tected_cancers", "Person_years"), Province == "all", Shiftable_year %in% 2015:2051, lc_screen_participated_ever == "all", lc_cessation_p_x_rticipated_ever == "all") %>%
		select(-c(Province, lc_screen_participated_ever, lc_cessation_p_x_rticipated_ever))

	combinedTable = bind_rows(breastCasesTable, breastScreenTable, breastICERTable, colorectalCasesTable, colorectalScreenTable, colorectalICERTable, cervicalCasesTable, cervicalScreenTable, cervicalICERTable, lungCasesTable, lungICERTable)

	combinedTable$expr_name = as.character(exprRename[combinedTable$expr_name])

	combinedTable = combinedTable %>% pivot_wider(names_from = expr_name, values_from = expr_value)

	#No idea how person years should be factored in?
	#Maybe by adding together the differences from base case?

	#Not great, but just multiplying resource usage (Screening Cost, Screen Num) by 2. Add Screening_Cost to total cost to simulate double the screening cost (since males are also being screened)
	femaleOnlyTable = combinedTable %>% filter(cancertype %in% c("cervical", "breast")) %>% mutate(Total_Screens = Total_Screens * 2, Screening_Cost = Screening_Cost * 2, Total_Cost = Total_Cost + Screening_Cost)
	allSexTable = combinedTable %>% filter(cancertype %in% c("lung", "colorectal"))

	combinedTableAllCancer = bind_rows(femaleOnlyTable, allSexTable) %>%
		select(-cancertype) %>%
		group_by(Shiftable_year, sce, formattedName, minAge, frequency, participation, testType) %>%
		summarize(Detected_Cases = sum(Detected_Cases), Cancer_Deaths = sum(Cancer_Deaths), Total_Screens = mean(Total_Screens), Total_Cost = sum(Total_Cost - Screening_Cost) + mean(Screening_Cost), Screening_Cost = mean(Screening_Cost)) %>%
		mutate(cancertype = "Combined") #Total cost may be entirely wrong

	combinedTable = bind_rows(combinedTable, combinedTableAllCancer)

	combinedTable$cancertype <- str_to_title(combinedTable$cancertype)
	combinedTable$cancertype <- factor(combinedTable$cancertype, levels = c("Breast", "Cervical", "Colorectal", "Lung", "Combined"))

	combinedTableBaseCase <- combinedTable %>% filter(sce == "BaseCase") %>% select(-c(sce, formattedName, minAge, frequency, participation, testType))
	names(combinedTableBaseCase) = paste("BASECASE", names(combinedTableBaseCase), sep = "_")
	names(combinedTableBaseCase) = gsub("BASECASE_Shiftable_year", "Shiftable_year", names(combinedTableBaseCase))
	names(combinedTableBaseCase) = gsub("BASECASE_cancertype", "cancertype", names(combinedTableBaseCase))

	combinedTable <- combinedTable %>%
		group_by(Shiftable_year, cancertype) %>%
		left_join(combinedTableBaseCase)

	createDifference = c("Detected_Cases", "Cancer_Deaths", "Total_Screens", "Total_Cost", "Screening_Cost")

	for (variable in createDifference) {
		combinedTable[[paste(variable, "Diff", sep = "_")]] = combinedTable[[variable]] - combinedTable[[paste("BASECASE", variable, sep = "_")]]
	}

	combinedTable = combinedTable[, !grepl("BASECASE", names(combinedTable))]

	combinedTables["PrimaryTable"] = list(combinedTable)

	yearsToFilterWith = 2025:2051
	combinedTableCombinedYears = combinedTable %>%
		group_by(sce, formattedName, minAge, frequency, participation, testType, cancertype) %>%
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
			Screening_Cost_Diff = sum(Screening_Cost_Diff)
		)
	#Add stage analysis. Fix sex option in combined

	combinedTableCombinedYears$ICER_ReducedDeaths = combinedTableCombinedYears$Total_Cost_Diff - (combinedTableCombinedYears$Cancer_Deaths_Diff * -1)

	#One row per set of years
	if (!(nrow(combinedTableCombinedYears) * length(yearsToFilterWith) == nrow(combinedTable %>% filter(Shiftable_year %in% yearsToFilterWith)))) {
		stop("ERROR. NOT ADDING UP TO CORRECT VALUE")
	}

	combinedTables["PrimaryTable_2025-2051"] = list(combinedTableCombinedYears)

	combinedTables
}
