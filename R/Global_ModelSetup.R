modelDigests = list()
modelDigests["OncoSim-lung"] = "201e47e22502a6f0ec0593f7f539970c"
modelDigests["OncoSim-breast"] = "7b6012a86dcbfce878eca24e4845736a"
modelDigests["OncoSim-colorectal"] = "5af0eb8d41728e927b1e21e97f5434f1"
modelDigests["OncoSim-allcancers"] = "c2cc80bad8c67abf432029f62682e97f"

paramsToAlter <- list()
paramsToAlter["OncoSim-breast"] <- list(c(
	"BreastScreeningProtocolDispatcher",
	"BreastScreeningUseLeanDispatcher",
	"BreastScreeningSpecificityNew",
	"BreastScreeningSensitivity",
	"BreastScreeningCosts",
	"BreastCancerCosts",
	"BreastCancerCosts2"
))
paramsToAlter["OncoSim-colorectal"] <- list(c(
	"CrcScreenRecruitCanada",
	"CrcScreenPlanCanada",
	"CrcTestPositivitySuccinct",
	"CrcScreenCostCanada",
	"TimeToRegularScreeningAfterColonoscopy",
	"ColonCancerTreatmentCosts",
	"RectalCancerTreatmentCosts"
))
paramsToAlter["OncoSim-lung"] <- list(c(
	"LungCancerScreenFollowupProtcol",
	"LcDispatcherDefault",
	"LungRadsScreenRegularDist",
	"LcScreenCost",
	"LcScreenEligibilityRiskThreshold",
	"SwitchEligibilityRule",
	"ReportYearPooled",
	"LungCancerTreatmentCosts",
	"LungRadsScreenRegularTimeToNext",
	"LungRadsScreenFollowupTimeToNextScanAfterFP"
))

treatmentCostMultiplier = list()
treatmentCostMultiplier["OncoSim-colorectal"] = 1.637795
treatmentCostMultiplier["OncoSim-breast"] = 1.145763
treatmentCostMultiplier["OncoSim-lung"] = 2.780313


outputTables <- list()

outputTables["OncoSim-colorectal"] <- list(c(
	"Colorectal_Cancer_Cases_Table",
	"Colorectal_Cancer_Screen_Table",
	"Colorectal_Cancer_ScreenState_Outcomes_Table",
	"Colorectal_Cancer_ICER_Table",
	"Colorectal_Cancer_ICER_Table_Discounted",
	"Colorectal_Cancer_StageDist_Table",
	"Colorectal_Cancer_Screen_Table_By_FH",
	"Colorectal_Cancer_Timing_Of_Last_Screen",
	"ColorectalCancer_TreatmentCost_Table",
	"Colorectal_Cancer_Cases_PASY_Table",
	"Colorectal_Cancer_Survival_Table",
	"Colorectal_Cancer_Costs_Table",
	"Colorectal_Cancer_Outcomes_GASY_UP_Table"
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
	"Breast_Cancer_Timing_Of_Last_Screen",
	"Breast_Cancer_TreatmentCost_Table",
	"Breast_Cancer_Table_By_Density"
))
outputTables["OncoSim-lung"] <- list(c(
	"Lung_Cancer_Cases_Table",
	"Lung_Cancer_ScreenCases_Year_Table",
	"Lung_Cancer_Costs_Table",
	"Lung_Cancer_ICER_Table",
	"Lung_Cancer_Smoking_Cessation_Table",
	"Lung_Cancer_StageDist_Table",
	"Lung_Cancer_Timing_Of_Last_Screen",
	"LungCancer_TreatmentCost_Table",
	"Lung_Cancer_Survival_Table",
	"Lung_Cancer_ScreenCases_Year_Table_Filtered",
	"Lung_Cancer_Time_Table_ScreenCohort"
))

generateModelParameters <- function(modelsToUse) {
	testCosts = list()

	#https://www.galleri.com/patient/the-galleri-test/cost
	# $949 USD ( $1,304.03 CAD)
	testCosts["Galleri"] = 1304.03

	#https://www.cancerguard.com/
	# $689 USD ($946.66 CAD)
	testCosts["CancerSeek"] = 946.66

	#Sensitivity THEN specificity
	testPerformance = list()

	#https://www.ncbi.nlm.nih.gov/books/NBK611745/
	#https://www.annalsofoncology.org/article/S0923-7534(21)02046-9/fulltext CCGA3
	# Using site-specific sensitivity and overall specificity
	testPerformance["OncoSim-colorectal-Galleri"] = list(c(0.82, 0.995)) #Better than FIT (75)
	testPerformance["OncoSim-breast-Galleri"] = list(c(0.305, 0.995))
	testPerformance["OncoSim-lung-Galleri"] = list(c(0.748, 0.995))

	#https://www.ncbi.nlm.nih.gov/books/NBK611745/
	#https://pmc.ncbi.nlm.nih.gov/articles/PMC6080308/
	# The specificity of CancerSEEK was greater than 99%
	testPerformance["OncoSim-colorectal-CancerSeek"] = list(c(0.649, 0.99)) # Worse than FIT (75)
	testPerformance["OncoSim-breast-CancerSeek"] = list(c(0.335, 0.99))
	testPerformance["OncoSim-lung-CancerSeek"] = list(c(0.587, 0.99))

	parameterTables = data.frame()

	scenarios <- c()

	for (scenarioType in c("Galleri", "CancerSeek", "NonMCED")) {
		for (startAge in c(45, 50)) {
			for (testFrequency in c(2, 3)) {
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

						if (scenarioType != "NonMCED") {
							currentTestParameters = testPerformance[paste(
								cancerType,
								scenarioType,
								sep = "-"
							)][[1]]
							currentTable$sensitivity = currentTestParameters[1]
							currentTable$specificity = currentTestParameters[2]
							currentTable$cost = testCosts[scenarioType][[1]]
						}

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

	#parameterTables = parameterTables[grepl("BaseCase", parameterTables$name), ]
	#parameterTables = parameterTables[grepl("NonMCED", parameterTables$name), ]

	#parameterTables = head(parameterTables, 1)

	parameterTables
}
