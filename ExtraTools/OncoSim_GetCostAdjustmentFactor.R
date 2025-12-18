modelDigests = list()
modelDigests["OncoSim-lung"] = "07a27aa297b2248b4f01b9300787bb54"
modelDigests["OncoSim-breast"] = "b7052122a73609179d8dcf40f467ee2e"
modelDigests["OncoSim-colorectal"] = "03dfd40c7e00f6b95564376a38ad2680"
modelDigests["OncoSim-allcancers"] = "c2650ffa5e64cb0bb4fd3dcfb82575f5"


allCancerModelRuns = load_model(modelDigests["OncoSim-allcancers"][[1]])$ModelRuns
defaultAllCancer = load_run(modelDigests["OncoSim-allcancers"][[1]], allCancerModelRuns$RunDigest[allCancerModelRuns$Name == "Default_first_run_48M_cases_12_subs"])
defaultAllCancerCost = defaultAllCancer$get_table("All_Cancer_Cost_Table")
defaultAllCancerIncidence = defaultAllCancer$get_table("All_Cancer_Active_Incidence_ByProv_Table")

referenceCost = defaultAllCancerCost %>% filter(Sex == "all", expr_name == "Health_System_Cost", Shiftable_year %in% 2015:2051) %>% mutate(cost = expr_value) %>% select(Cancers, Shiftable_year, cost)
referenceIncidence = defaultAllCancerIncidence %>% filter(expr_name == "Number_of_new_cases", Province == "all", Age_group == "all", Sex == "all", Shiftable_year %in% 2015:2051) %>% mutate(cases = expr_value) %>% select(Cancers, Shiftable_year, cases)
referenceICER = full_join(referenceCost, referenceIncidence) %>% mutate(averageCostPer = cost / cases) %>% group_by(Cancers) %>% summarize(cases = sum(cases), cost = sum(cost), averageCostPer = mean(averageCostPer))


referenceColorectalValue = referenceICER %>% filter(Cancers == "Colorectal") %>% select(averageCostPer)
referenceBreastValue = referenceICER %>% filter(Cancers == "Breast") %>% select(averageCostPer)
referenceLungValue = referenceICER %>% filter(Cancers == "Lung") %>% select(averageCostPer)


colorectalModelRuns = load_model(modelDigests["OncoSim-colorectal"][[1]])$ModelRuns
defaultColorectal = load_run(modelDigests["OncoSim-colorectal"][[1]], colorectalModelRuns$RunDigest[colorectalModelRuns$Name == "Default_first_run_48M_cases_12_subs"])
defaultColorectalICER = defaultColorectal$get_table("Colorectal_Cancer_ICER_Table")

colorectalValue = defaultColorectalICER %>%
	filter(Province == "all", Shiftable_year %in% 2015:2051, expr_name %in% c("Cost_of_cancer_management", "Incidence_cases")) %>%
	pivot_wider(values_from = expr_value, names_from = expr_name) %>%
	mutate(averageCostPer = Cost_of_cancer_management / Incidence_cases) %>%
	summarize(Incidence_cases = sum(Incidence_cases), Cost_of_cancer_management = sum(Cost_of_cancer_management), averageCostPer = mean(averageCostPer))


colorectalRatio = referenceColorectalValue / colorectalValue$averageCostPer


breastModelRuns = load_model(modelDigests["OncoSim-breast"][[1]])$ModelRuns
defaultBreast = load_run(modelDigests["OncoSim-breast"][[1]], breastModelRuns$RunDigest[breastModelRuns$Name == "Default_first_run_48M_cases_12_subs"])
defaultBreastICER = defaultBreast$get_table("Breast_Cancer_ICER_Table")

breastValue = defaultBreastICER %>%
	filter(Province == "all", Shiftable_year %in% 2015:2051, expr_name %in% c("Cost_of_cancer_management", "Incidence_cases_staged")) %>%
	pivot_wider(values_from = expr_value, names_from = expr_name) %>%
	mutate(averageCostPer = Cost_of_cancer_management / Incidence_cases_staged) %>%
	summarize(Incidence_cases_staged = sum(Incidence_cases_staged), Cost_of_cancer_management = sum(Cost_of_cancer_management), averageCostPer = mean(averageCostPer))

breastRatio = referenceBreastValue / breastValue$averageCostPer


lungModelRuns = load_model(modelDigests["OncoSim-lung"][[1]])$ModelRuns
defaultLung = load_run(modelDigests["OncoSim-lung"][[1]], lungModelRuns$RunDigest[lungModelRuns$Name == "Default_first_run_48M_cases_12_subs"])
defaultLungICER = defaultLung$get_table("Lung_Cancer_ICER_Table")
defaultLungCases = defaultLung$get_table("Lung_Cancer_Cases_Table")

lungCost = defaultLungICER %>%
	filter(Province == "all", lc_screen_participated_ever == "all", lc_cessation_p_x_rticipated_ever == "all", Shiftable_year %in% 2015:2051, expr_name %in% c("Cost_of_cancer_x_ent_treatment")) %>%
	pivot_wider(values_from = expr_value, names_from = expr_name) %>%
	select(Shiftable_year, Cost_of_cancer_x_ent_treatment)

lungCase = defaultLungCases %>%
	filter(Hackable_province == "all", Sex == "all", Shiftable_year %in% 2015:2051, expr_name %in% c("Incidence_cases")) %>%
	pivot_wider(values_from = expr_value, names_from = expr_name) %>%
	select(Shiftable_year, Incidence_cases)

lungValue = full_join(lungCost, lungCase) %>%
	mutate(averageCostPer = Cost_of_cancer_x_ent_treatment / Incidence_cases) %>%
	summarize(Incidence_cases = sum(Incidence_cases), Cost_of_cancer_x_ent_treatment = sum(Cost_of_cancer_x_ent_treatment), averageCostPer = mean(averageCostPer))

lungRatio = referenceLungValue / lungValue$averageCostPer
