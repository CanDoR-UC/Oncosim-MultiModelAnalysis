modelDigests = list()
modelDigests["OncoSim-lung"] = "201e47e22502a6f0ec0593f7f539970c"
modelDigests["OncoSim-breast"] = "7b6012a86dcbfce878eca24e4845736a"
modelDigests["OncoSim-colorectal"] = "5af0eb8d41728e927b1e21e97f5434f1"
modelDigests["OncoSim-allcancers"] = "c2cc80bad8c67abf432029f62682e97f"


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


lungModelRuns = load_model(modelDigests["OncoSim-lung"][[1]])$ModelRuns
defaultLung = load_run(modelDigests["OncoSim-lung"][[1]], lungModelRuns$RunDigest[lungModelRuns$Name == "Default_first_run_48M_cases_12_subs"])
defaultLungCosts = defaultLung$get_table("Lung_Cancer_Costs_SingleYear_Table")
