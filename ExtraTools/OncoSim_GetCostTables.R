modelDigests = list()
modelDigests["OncoSim-lung"] = "07a27aa297b2248b4f01b9300787bb54"
modelDigests["OncoSim-breast"] = "b7052122a73609179d8dcf40f467ee2e"
modelDigests["OncoSim-colorectal"] = "03dfd40c7e00f6b95564376a38ad2680"
modelDigests["OncoSim-allcancers"] = "c2650ffa5e64cb0bb4fd3dcfb82575f5"


#Colorectal

colorectalModelRuns = load_model(modelDigests["OncoSim-colorectal"][[1]])$ModelRuns
defaultColorectal = load_run(modelDigests["OncoSim-colorectal"][[1]], colorectalModelRuns$RunDigest[colorectalModelRuns$Name == "Default_first_run_48M_cases_12_subs"])
defaultCRCTreatmentCost = defaultColorectal$get_table("Colorectal_Cancer_Summary_Outcomes_Stage_Table")

defaultCRCTreatmentCostFiltered = defaultCRCTreatmentCost %>%
	filter(expr_name %in% c("Cost_of_cancer_management", "Persons")) %>%
	pivot_wider(names_from = "expr_name", values_from = "expr_value") %>%
	mutate(CRCCostPerPerson = Cost_of_cancer_management / Persons, Stage = Stage_of_invasive_cancer_TNM) %>%
	select(Stage, CRCCostPerPerson)


breastModelRuns = load_model(modelDigests["OncoSim-breast"][[1]])$ModelRuns
defaultBreast = load_run(modelDigests["OncoSim-breast"][[1]], breastModelRuns$RunDigest[breastModelRuns$Name == "Default_first_run_48M_cases_12_subs"])
defaultBreastCosts = defaultBreast$get_table("Breast_Cancer_TreatmentCost_Table")


defaultBreastCosts$Stage_NEW[defaultBreastCosts$Stage %in% c("Breast_cancer_stage_IA", "Breast_cancer_stage_IB")] = "Stage_I"
defaultBreastCosts$Stage_NEW[defaultBreastCosts$Stage %in% c("Breast_cancer_stage_IIA", "Breast_cancer_stage_IIB")] = "Stage_II"
defaultBreastCosts$Stage_NEW[defaultBreastCosts$Stage %in% c("Breast_cancer_stage_IIIA", "Breast_cancer_stage_IIIB", "Breast_cancer_stage_IIIC")] = "Stage_III"
defaultBreastCosts$Stage_NEW[defaultBreastCosts$Stage %in% c("Breast_cancer_stage_IV")] = "Stage_IV"


defaultBreastCostsFiltered = defaultBreastCosts %>%
	filter(Hackable_province == "all", !is.na(Stage_NEW)) %>%
	pivot_wider(values_from = expr_value, names_from = expr_name) %>%
	mutate(Persons = Treatment_cost / Average_treatm_x_r_person_year) %>%
	group_by(Stage_NEW) %>%
	summarize(Treatment_cost = sum(Treatment_cost), Persons = sum(Persons)) %>%
	mutate(breastCostPerPerson = Treatment_cost / Persons, Stage = Stage_NEW) %>%
	select(Stage, breastCostPerPerson)


lungModelRuns = load_model(modelDigests["OncoSim-lung"][[1]])$ModelRuns
defaultLung = load_run(modelDigests["OncoSim-lung"][[1]], lungModelRuns$RunDigest[lungModelRuns$Name == "Default_first_run_48M_cases_12_subs"])
defaultLungCosts = defaultLung$get_table("LungCancer_TreatmentCost_Table")
defaultLungPrevalence = defaultLung$get_table("Lung_Cancer_LifetimeCostComponent_Table")


defaultLungCosts$Stage = defaultLungCosts$Stage_at_diagnosis_or_recurrence
defaultLungCosts$Stage[defaultLungCosts$Stage_at_diagnosis_or_recurrence %in% c("Non_small_cell_stage_I")] = "Stage_I"
defaultLungCosts$Stage[defaultLungCosts$Stage_at_diagnosis_or_recurrence %in% c("Non_small_cell_stage_II")] = "Stage_II"
defaultLungCosts$Stage[defaultLungCosts$Stage_at_diagnosis_or_recurrence %in% c("Non_small_cell_stage_IIIA", "Non_small_cell_stage_IIIB")] = "Stage_III"
defaultLungCosts$Stage[defaultLungCosts$Stage_at_diagnosis_or_recurrence %in% c("Non_small_cell_stage_IV")] = "Stage_IV"

defaultLungPrevalence$Stage = defaultLungPrevalence$Stage_at_initial_diagnosis
defaultLungPrevalence$Stage[defaultLungPrevalence$Stage_at_initial_diagnosis %in% c("Non_small_cell_stage_I")] = "Stage_I"
defaultLungPrevalence$Stage[defaultLungPrevalence$Stage_at_initial_diagnosis %in% c("Non_small_cell_stage_II")] = "Stage_II"
defaultLungPrevalence$Stage[defaultLungPrevalence$Stage_at_initial_diagnosis %in% c("Non_small_cell_stage_IIIA", "Non_small_cell_stage_IIIB")] = "Stage_III"
defaultLungPrevalence$Stage[defaultLungPrevalence$Stage_at_initial_diagnosis %in% c("Non_small_cell_stage_IV")] = "Stage_IV"

defaultLungCostsFiltered = defaultLungCosts %>%
	filter(Hackable_province == "all", expr_name %in% c("Cost")) %>%
	pivot_wider(values_from = expr_value, names_from = expr_name) %>%
	group_by(Stage) %>%
	summarize(Cost = sum(Cost)) %>%
	select(Stage, Cost)

defaultLungPrevalenceFiltered = defaultLungPrevalence %>% filter(expr_name == "Number_of_patients_alive") %>% group_by(Stage) %>% summarize(Persons = sum(expr_value))

defaultLungCostsFiltered = full_join(defaultLungCostsFiltered, defaultLungPrevalenceFiltered) %>%
	mutate(LungCostPerPerson = Cost / Persons) %>%
	select(Stage, LungCostPerPerson)


allCosts = full_join(defaultLungCostsFiltered, defaultBreastCostsFiltered)
allCosts = full_join(allCosts, defaultCRCTreatmentCostFiltered)
