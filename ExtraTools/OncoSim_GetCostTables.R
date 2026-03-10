source("_oncosim.R")
modelDigests = list()
modelDigests["OncoSim-lung"] = "361bc9283e954a392aee3d4b0569aeb3"
modelDigests["OncoSim-breast"] = "f3c2d5330103e5bc41c253256507db0c"
modelDigests["OncoSim-colorectal"] = "0832127dd0045abd29bf6f1ca5941f23"
modelDigests["OncoSim-allcancers"] = "fe7c0e930312efb7ab2087c9e88c1741"


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


totalYears = 2015:2051

lungModelRuns = load_model(modelDigests["OncoSim-lung"][[1]])$ModelRuns
defaultLung = load_run(modelDigests["OncoSim-lung"][[1]], lungModelRuns$RunDigest[lungModelRuns$Name == "Default_first_run_48M_cases_12_subs"])
defaultLungCost = defaultLung$get_table("LungCancer_TreatmentCost_Table") %>% mutate(Type = "Default")
baseCaseLungCost = read.csv("Output/Runs/AutomatedRun_ MCEDProject OncoSim-lung_BaseCase/LungCancer_TreatmentCost_Table.csv") %>% mutate(Type = "MCEDScenarios")


defaultLungCostFiltered = bind_rows(defaultLungCost, baseCaseLungCost) %>%
	filter(Hackable_province == "all", Shiftable_year == 2022) %>%
	pivot_wider(names_from = expr_name, values_from = expr_value) %>%
	summarize(AverageTreatmentCost = sum(Cost) / max(Number_of_patients_treated), .by = c("Stage_at_diagnosis_or_recurrence", "Type")) %>%
	pivot_wider(names_from = Type, values_from = AverageTreatmentCost) %>%
	mutate(Ratio = `MCEDScenarios` / Default)


breastModelRuns = load_model(modelDigests["OncoSim-breast"][[1]])$ModelRuns
defaultBreast = load_run(modelDigests["OncoSim-breast"][[1]], breastModelRuns$RunDigest[breastModelRuns$Name == "Default_first_run_48M_cases_12_subs"])
defaultBreastCost = defaultBreast$get_table("Breast_Cancer_TreatmentCost_Table") %>% mutate(Type = "Default")
baseCaseBreastCost = read.csv("Output/Runs/AutomatedRun_ MCEDProject OncoSim-breast_BaseCase/Breast_Cancer_TreatmentCost_Table.csv") %>% mutate(Type = "MCEDScenarios")


defaultBreastCostFiltered = bind_rows(defaultBreastCost, baseCaseBreastCost) %>%
	filter(Hackable_province == "all", Shiftable_year == 2022) %>%
	pivot_wider(names_from = expr_name, values_from = expr_value) %>%
	mutate(Number_of_patients = Treatment_cost / Average_treatm_x_r_person_year) %>%
	summarize(AverageTreatmentCost = sum(Treatment_cost) / max(Number_of_patients), .by = c("Stage", "Type")) %>%
	pivot_wider(names_from = Type, values_from = AverageTreatmentCost) %>%
	mutate(Ratio = `MCEDScenarios` / Default)

colorectalModelRuns = load_model(modelDigests["OncoSim-colorectal"][[1]])$ModelRuns
defaultColorectal = load_run(modelDigests["OncoSim-colorectal"][[1]], colorectalModelRuns$RunDigest[colorectalModelRuns$Name == "Default_first_run_48M_cases_12_subs"])
defaultColorectalCost = defaultColorectal$get_table("ColorectalCancer_TreatmentCost_Table") %>% mutate(Type = "Default")
baseCaseColorectalCost = read.csv("Output/Runs/AutomatedRun_ MCEDProject OncoSim-colorectal_BaseCase/ColorectalCancer_TreatmentCost_Table.csv") %>% mutate(Type = "MCEDScenarios")


defaultColorectalCostFiltered = bind_rows(defaultColorectalCost, baseCaseColorectalCost) %>%
	filter(Hackable_province == "all", Shiftable_year == 2022) %>%
	pivot_wider(names_from = expr_name, values_from = expr_value) %>%
	mutate(Treatment_cost = Average_treatm_x_patient_treated * Number_of_patients_treated) %>%
	mutate(Treatment_cost = ifelse(Number_of_patients_treated == 0, 0, Treatment_cost)) %>%
	summarize(AverageTreatmentCost = sum(Treatment_cost) / max(Number_of_patients_treated), .by = c("Stage_of_invasive_cancer_TNM", "Type")) %>%
	pivot_wider(names_from = Type, values_from = AverageTreatmentCost) %>%
	mutate(Ratio = `MCEDScenarios` / Default)
