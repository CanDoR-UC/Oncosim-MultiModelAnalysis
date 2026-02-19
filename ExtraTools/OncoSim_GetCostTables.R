modelDigests = list()
modelDigests["OncoSim-lung"] = "07a27aa297b2248b4f01b9300787bb54"
modelDigests["OncoSim-breast"] = "b7052122a73609179d8dcf40f467ee2e"
modelDigests["OncoSim-colorectal"] = "03dfd40c7e00f6b95564376a38ad2680"
modelDigests["OncoSim-allcancers"] = "c2650ffa5e64cb0bb4fd3dcfb82575f5"


#Colorectal

colorectalScenario = load_scenario(modelDigests["OncoSim-colorectal"][[1]], "Default")
rectalDefaultCosts = colorectalScenario$get_param("RectalCancerTreatmentCosts")
colonDefaultCosts = colorectalScenario$get_param("ColonCancerTreatmentCosts")

rectalDefaultCosts = rectalDefaultCosts %>%
	mutate(sum = rowSums(across(where(is.numeric)), na.rm = TRUE), Stage = gsub("_incurable", "", gsub("_curable", "", Stage_for_treatment))) %>%
	group_by(Stage) %>%
	summarize(Rectal_Default = sum(sum))

totalTable = rectalDefaultCosts

colonDefaultCosts = colonDefaultCosts %>%
	mutate(sum = rowSums(across(where(is.numeric)), na.rm = TRUE), Stage = gsub("_incurable", "", gsub("_curable", "", Stage_for_treatment))) %>%
	group_by(Stage) %>%
	summarize(Colon_Default = sum(sum))

totalTable = full_join(totalTable, colonDefaultCosts)

rectalAdjustedCosts = read.csv("Output/Scenarios/MCEDProjectOncoSim-colorectal_BaseCase/RectalCancerTreatmentCosts.csv")
colonAdjustedCosts = read.csv("Output/Scenarios/MCEDProjectOncoSim-colorectal_BaseCase/ColonCancerTreatmentCosts.csv")

rectalAdjustedCosts = rectalAdjustedCosts %>%
	mutate(sum = rowSums(across(where(is.numeric)), na.rm = TRUE), Stage = gsub("_incurable", "", gsub("_curable", "", Stage_for_treatment))) %>%
	group_by(Stage) %>%
	summarize(Rectal_NewCosts = sum(sum))

colonAdjustedCosts = colonAdjustedCosts %>%
	mutate(sum = rowSums(across(where(is.numeric)), na.rm = TRUE), Stage = gsub("_incurable", "", gsub("_curable", "", Stage_for_treatment))) %>%
	group_by(Stage) %>%
	summarize(Colon_NewCosts = sum(sum))
totalTable = full_join(totalTable, rectalAdjustedCosts)
totalTable = full_join(totalTable, colonAdjustedCosts)

# Breast

breastScenario = load_scenario(modelDigests["OncoSim-breast"][[1]], "Default")
stageDict = list()
stageDict["Stage_1"] = "Stage_I"
stageDict["Stage_2"] = "Stage_II"
stageDict["Stage_3"] = "Stage_III"
stageDict["Stage_4"] = "Stage_IV"

breastDefaultCost1 = breastScenario$get_param("BreastCancerCosts") %>% filter(Breast_cancer_costs %in% names(stageDict)) %>% mutate(Stage = as.character(stageDict[Breast_cancer_costs]))
breastDefaultCost2 = breastScenario$get_param("BreastCancerCosts2") %>% filter(Breast_cancer_costs %in% names(stageDict)) %>% mutate(Stage = as.character(stageDict[Breast_cancer_costs]))

breastData = breastDefaultCost1 %>% filter(Age_group == "(-∞,70)") %>% mutate(BreastBelow70_Default = Cost) %>% select(Stage, BreastBelow70_Default)
breastData = full_join(breastData, breastDefaultCost1 %>% filter(Age_group == "[70,∞)") %>% mutate(BreastAbove70_Default = Cost) %>% select(Stage, BreastAbove70_Default))
breastData = full_join(breastData, breastDefaultCost2 %>% filter(Age_group == "[70,∞)") %>% mutate(BreastHormonal_Default = Cost) %>% select(Stage, BreastHormonal_Default))

BreastAdjustedCosts1 = read.csv("Output/Scenarios/MCEDProjectOncoSim-breast_BaseCase/BreastCancerCosts.csv") %>% filter(Breast_cancer_costs %in% names(stageDict)) %>% mutate(Stage = as.character(stageDict[Breast_cancer_costs]))
BreastAdjustedCosts2 = read.csv("Output/Scenarios/MCEDProjectOncoSim-breast_BaseCase/BreastCancerCosts2.csv") %>% filter(Breast_cancer_costs %in% names(stageDict)) %>% mutate(Stage = as.character(stageDict[Breast_cancer_costs]))

breastData = full_join(breastData, BreastAdjustedCosts1 %>% filter(Age_group == "(-∞,70)") %>% mutate(BreastBelow70_Adjusted = Cost) %>% select(Stage, BreastBelow70_Adjusted))
breastData = full_join(breastData, BreastAdjustedCosts1 %>% filter(Age_group == "[70,∞)") %>% mutate(BreastAbove70_Adjusted = Cost) %>% select(Stage, BreastAbove70_Adjusted))
breastData = full_join(breastData, BreastAdjustedCosts2 %>% filter(Age_group == "[70,∞)") %>% mutate(BreastHormonal_Adjusted = Cost) %>% select(Stage, BreastHormonal_Adjusted))

totalTable = full_join(totalTable, breastData)


stageDict = list()
stageDict["Non_small_cell_stage_I"] = "Stage_I"
stageDict["Non_small_cell_stage_II"] = "Stage_II"
stageDict["Non_small_cell_stage_IIIA"] = "Stage_III"
stageDict["Non_small_cell_stage_IIIB"] = "Stage_III"
stageDict["Non_small_cell_stage_IV"] = "Stage_IV"
stageDict["Small_cell_limited"] = "Small_Cell_Limited"
stageDict["Small_cell_extensive"] = "Small_cell_extensive"
stageDict["Relapse_non_small_cell"] = "Relapse_Non_Small_Cell"
stageDict["Relapse_small_cell"] = "Relapse_Small_Cell"


lungScenario = load_scenario(modelDigests["OncoSim-lung"][[1]], "Default")
lungDefaultCosts = lungScenario$get_param("LungCancerTreatmentCosts") %>%
	mutate(sum = rowSums(across(where(is.numeric)), na.rm = TRUE), Stage = as.character(stageDict[Stage])) %>%
	filter(Current_practice_vs_Intervention == "Current_practice") %>%
	group_by(Stage) %>%
	summarize(Lung_Default = sum(sum))

totalTable = full_join(totalTable, lungDefaultCosts)

lungAdjustedCosts = read.csv("Output/Scenarios/MCEDProjectOncoSim-lung_BaseCase/LungCancerTreatmentCosts.csv") %>%
	mutate(sum = rowSums(across(where(is.numeric)), na.rm = TRUE), Stage = as.character(stageDict[Stage])) %>%
	filter(Current_practice_vs_Intervention == "Current_practice") %>%
	group_by(Stage) %>%
	summarize(Lung_Adjusted = sum(sum))
totalTable = full_join(totalTable, lungAdjustedCosts)


for (column in names(totalTable)) {
	if (column != "Stage") {
		totalTable[[column]] = scales::dollar(totalTable[[column]], accuracy = 0.01)
	}
}
write.csv(t(totalTable), "CostTable.csv")
