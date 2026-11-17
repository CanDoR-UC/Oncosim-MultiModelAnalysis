source("_oncosim.R")
modelDigests <- list()
modelDigests["OncoSim-lung"] <- "201e47e22502a6f0ec0593f7f539970c"
modelDigests["OncoSim-breast"] <- "7b6012a86dcbfce878eca24e4845736a"
modelDigests["OncoSim-colorectal"] <- "5af0eb8d41728e927b1e21e97f5434f1"
modelDigests["OncoSim-allcancers"] <- "c2cc80bad8c67abf432029f62682e97f"


stageDict <- list()
stageDict["Non_small_cell_stage_I"] <- "Stage_I"
stageDict["Non_small_cell_stage_II"] <- "Stage_II"
stageDict["Non_small_cell_stage_IIIA"] <- "Stage_III"
stageDict["Non_small_cell_stage_IIIB"] <- "Stage_III"
stageDict["Non_small_cell_stage_IV"] <- "Stage_IV"
stageDict["Small_cell_limited"] <- "Small_Cell_Limited"
stageDict["Small_cell_extensive"] <- "Small_cell_extensive"
stageDict["Relapse_non_small_cell"] <- "Relapse_Non_Small_Cell"
stageDict["Relapse_small_cell"] <- "Relapse_Small_Cell"


totalYears <- 2015:2051

lungModelRuns <- load_model(modelDigests["OncoSim-lung"][[1]])$ModelRuns
defaultLung <- load_run(modelDigests["OncoSim-lung"][[1]], lungModelRuns$RunDigest[lungModelRuns$Name == "Default_first_run_48M_cases_12_subs"])
defaultLungCost <- defaultLung$get_table("LungCancer_TreatmentCost_Table") %>% mutate(Type = "Default")
CurrentScreeningLungCost <- read.csv("Output/Runs/AutomatedRun_ MCEDProject OncoSim-lung_CurrentScreening/LungCancer_TreatmentCost_Table.csv") %>% mutate(Type = "MCEDScenarios")


defaultLungCostFiltered <- bind_rows(defaultLungCost, CurrentScreeningLungCost) %>%
	filter(Hackable_province == "all", Shiftable_year %in% totalYears) %>%
	pivot_wider(names_from = expr_name, values_from = expr_value) %>%
	summarize(MaxPatientsTreated = max(Number_of_patients_treated), TreatmentCost = sum(Cost), .by = c("Stage_at_diagnosis_or_recurrence", "Type", "Shiftable_year")) %>%
	summarize(AverageTreatmentCost = sum(TreatmentCost) / sum(MaxPatientsTreated), .by = c("Stage_at_diagnosis_or_recurrence", "Type")) %>%
	pivot_wider(names_from = Type, values_from = AverageTreatmentCost) %>%
	mutate(Ratio = `MCEDScenarios` / Default) %>%
	mutate(MCEDScenarios = scales::dollar(MCEDScenarios, accuracy = 0.01))


breastModelRuns <- load_model(modelDigests["OncoSim-breast"][[1]])$ModelRuns
defaultBreast <- load_run(modelDigests["OncoSim-breast"][[1]], breastModelRuns$RunDigest[breastModelRuns$Name == "Default_first_run_48M_cases_12_subs"])
defaultBreastCost <- defaultBreast$get_table("Breast_Cancer_TreatmentCost_Table") %>% mutate(Type = "Default")
CurrentScreeningBreastCost <- read.csv("Output/Runs/AutomatedRun_ MCEDProject OncoSim-breast_CurrentScreening/Breast_Cancer_TreatmentCost_Table.csv") %>% mutate(Type = "MCEDScenarios")


defaultBreastCostFiltered <- bind_rows(defaultBreastCost, CurrentScreeningBreastCost) %>%
	filter(Hackable_province == "all", Shiftable_year %in% totalYears) %>%
	pivot_wider(names_from = expr_name, values_from = expr_value) %>%
	mutate(Number_of_patients = Treatment_cost / Average_treatm_x_r_person_year) %>%
	summarize(MaxPatientsTreated = max(Number_of_patients), TreatmentCost = sum(Treatment_cost), .by = c("Stage", "Type", "Shiftable_year")) %>%
	summarize(AverageTreatmentCost = sum(TreatmentCost) / sum(MaxPatientsTreated), .by = c("Stage", "Type")) %>%
	pivot_wider(names_from = Type, values_from = AverageTreatmentCost) %>%
	mutate(Ratio = `MCEDScenarios` / Default) %>%
	mutate(MCEDScenarios = scales::dollar(MCEDScenarios, accuracy = 0.01))

colorectalModelRuns <- load_model(modelDigests["OncoSim-colorectal"][[1]])$ModelRuns
defaultColorectal <- load_run(modelDigests["OncoSim-colorectal"][[1]], colorectalModelRuns$RunDigest[colorectalModelRuns$Name == "Default_first_run_48M_cases_12_subs"])
defaultColorectalCost <- defaultColorectal$get_table("ColorectalCancer_TreatmentCost_Table") %>% mutate(Type = "Default")
CurrentScreeningColorectalCost <- read.csv("Output/Runs/AutomatedRun_ MCEDProject OncoSim-colorectal_CurrentScreening/ColorectalCancer_TreatmentCost_Table.csv") %>% mutate(Type = "MCEDScenarios")


defaultColorectalCostFiltered <- bind_rows(defaultColorectalCost, CurrentScreeningColorectalCost) %>%
	filter(Hackable_province == "all", Shiftable_year %in% totalYears) %>%
	pivot_wider(names_from = expr_name, values_from = expr_value) %>%
	mutate(Treatment_cost = Average_treatm_x_patient_treated * Number_of_patients_treated) %>%
	mutate(Treatment_cost = ifelse(Number_of_patients_treated == 0, 0, Treatment_cost)) %>%
	summarize(MaxPatientsTreated = max(Number_of_patients_treated), TreatmentCost = sum(Treatment_cost), .by = c("Stage_of_invasive_cancer_TNM", "Type", "Shiftable_year")) %>%
	summarize(AverageTreatmentCost = sum(TreatmentCost) / sum(MaxPatientsTreated), .by = c("Stage_of_invasive_cancer_TNM", "Type")) %>%
	pivot_wider(names_from = Type, values_from = AverageTreatmentCost) %>%
	mutate(Ratio = `MCEDScenarios` / Default) %>%
	mutate(MCEDScenarios = scales::dollar(MCEDScenarios, accuracy = 0.01))
