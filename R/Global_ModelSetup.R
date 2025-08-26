modelDigests = list()
modelDigests["OncoSim-lung"] = "07a27aa297b2248b4f01b9300787bb54"
modelDigests["OncoSim-breast"] = "b7052122a73609179d8dcf40f467ee2e"
modelDigests["OncoSim-colorectal"] = "03dfd40c7e00f6b95564376a38ad2680"

paramsToAlter <- list()
paramsToAlter["OncoSim-breast"] <- list(c(
	"BreastScreeningProtocolDispatcher",
	"BreastScreeningUseLeanDispatcher",
	"BreastScreeningSpecificityNew",
	"BreastScreeningSensitivity",
	"BreastScreeningCosts"
))
paramsToAlter["OncoSim-colorectal"] <- list(c(
	"CrcScreenRecruitCanada",
	"CrcScreenPlanCanada",
	"CrcTestPositivitySuccinct",
	"CrcScreenCostCanada"
))
paramsToAlter["OncoSim-lung"] <- list(c(
	"LungCancerScreenFollowupProtcol",
	"LcDispatcherDefault",
	"LungRadsScreenRegularDist",
	"LcScreenCost",
	"LcScreenEligibilityRiskThreshold",
	"SwitchEligibilityRule",
	"ReportYearPooled"
))


outputTables <- list()

outputTables["OncoSim-colorectal"] <- list(c(
	"Colorectal_Cancer_Cases_Table",
	"Colorectal_Cancer_Screen_Table",
	"Colorectal_Cancer_ScreenState_Outcomes_Table",
	"Colorectal_Cancer_ICER_Table",
	"Colorectal_Cancer_ICER_Table_Discounted",
	"Colorectal_Cancer_StageDist_Table",
	"Colorectal_Cancer_Screen_Table_By_FH",
	"Colorectal_Cancer_Timing_Of_Last_Screen"
))
outputTables["OncoSim-breast"] <- list(c(
	"Breast_Cancer_Cases_Table",
	"Breast_Cancer_Screen_Outcome_Modality_Table",
	"Breast_Cancer_ICER_Table",
	"Breast_Cancer_StageDist_AY_Table",
	"Breast_Cancer_Screen_Outcome_EachSingleAge_Table",
	"Breast_Cancer_Screen_Outcome_Positivity_Table",
	"Breast_Cancer_Costs_Table",
	"Breast_Cancer_ICER_Table_Discounted",
	"Breast_Cancer_Screen_Outcome_Modality_Table",
	"Breast_Cancer_Timing_Of_Last_Screen"
))
outputTables["OncoSim-lung"] <- list(c(
	"Lung_Cancer_Cases_Table",
	"Lung_Cancer_ScreenCases_Year_Table",
	"Lung_Cancer_Costs_Table",
	"Lung_Cancer_ICER_Table",
	"Lung_Cancer_Smoking_Cessation_Table",
	"Lung_Cancer_StageDist_Table",
	"Lung_Cancer_Timing_Of_Last_Screen"
))

generateModelParameters <- function(modelsToUse) {
	testCosts = list()

	#https://www.galleri.com/patient/the-galleri-test/cost
	# $949 USD = 1,312.48
	testCosts["Galleri"] = 1312.48

	#https://pmc.ncbi.nlm.nih.gov/articles/PMC6080308/
	# "We estimate the cost of the test to be less than $500,"
	# $500 USD = $691.50
	testCosts["CancerSeek"] = 691.50

	#Sensitivity THEN specificity
	testPerformance = list()

	#https://www.ncbi.nlm.nih.gov/books/NBK611745/
	#https://www.annalsofoncology.org/article/S0923-7534(21)02046-9/fulltext CCGA3
	# Using site-specific sensitivity and overall specificity
	testPerformance["OncoSim-colorectal-Galleri"] = list(c(0.82, 0.995))
	testPerformance["OncoSim-breast-Galleri"] = list(c(0.305, 0.995))
	testPerformance["OncoSim-lung-Galleri"] = list(c(0.748, 0.995))

	#https://www.ncbi.nlm.nih.gov/books/NBK611745/
	#https://pmc.ncbi.nlm.nih.gov/articles/PMC6080308/
	# The specificity of CancerSEEK was greater than 99%
	testPerformance["OncoSim-colorectal-CancerSeek"] = list(c(0.649, 0.99))
	testPerformance["OncoSim-breast-CancerSeek"] = list(c(0.335, 0.99))
	testPerformance["OncoSim-lung-CancerSeek"] = list(c(0.587, 0.99))

	parameterTables = data.frame()

	scenarios <- c()

	for (scenarioType in c("Galleri", "CancerSeek")) {
		for (startAge in c(45, 50)) {
			for (testFrequency in c(1, 3)) {
				for (participation in c(30, 50, 70, 90)) {
					for (cancerType in modelsToUse) {
						scenarioName = paste(
							cancerType,
							startAge,
							testFrequency,
							participation,
							scenarioType,
							sep = "_"
						)
						currentTable = data.frame(name = scenarioName)

						currentTable$type = scenarioType
						currentTable$model = cancerType
						currentTable$startAge = startAge
						currentTable$endAge = 74
						currentTable$frequency = testFrequency

						currentTable$participation = participation / 100

						currentTestParameters = testPerformance[paste(
							cancerType,
							scenarioType,
							sep = "-"
						)][[1]]
						currentTable$sensitivity = currentTestParameters[1]
						currentTable$specificity = currentTestParameters[2]

						currentTable$cost = testCosts[scenarioType][[1]]

						parameterTables = bind_rows(parameterTables, currentTable)
					}
				}
			}
		}
	}

	for (scenarioType in c("BaseCase")) {
		for (cancerType in modelsToUse) {
			scenarioName = paste(cancerType, scenarioType, sep = "_")
			currentTable = data.frame(name = scenarioName)

			currentTable$model = cancerType
			currentTable$type = scenarioType

			parameterTables = bind_rows(parameterTables, currentTable)
		}
	}

	print(nrow(parameterTables))

	#parameterTables = parameterTables[grepl("45_1_90", parameterTables$name) | grepl("50_3_30", parameterTables$name) | grepl("BaseCase", parameterTables$name), ]
	#parameterTables = head(parameterTables, 1)

	parameterTables
}
