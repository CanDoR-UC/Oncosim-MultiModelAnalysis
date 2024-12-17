library(targets)
library(tidyverse)
library(openmpp)


openmpp::use_OpenMpp_remote()

## Source R files ----
purrr::walk(fs::dir_ls("./R", glob = "*.R$"), source)

analysisName = "MCEDTesting"

#This number is added to scenario and run names to avoid collision
randomNum <- sample(1000000:90000000,1)

#modelsToUse <- c("OncoSim-breast","OncoSim-colorectal","OncoSim-cervical","OncoSim-lung")
modelsToUse <- c("OncoSim-colorectal")
paramsToAlter <- list()
paramsToAlter["OncoSim-breast"] <- list(c("BreastScreeningProtocolDispatcher","BreastCancerScreeningCost","BreastScreeningSpecificityNew")) #Hack specific type of mammogram
paramsToAlter["OncoSim-colorectal"] <- list(c("CrcScreenRecruitCanada","CrcScreenPlanCanada","CrcTestPositivitySuccinct","CrcScreenCostCanada"))#Planning to hack FOBT to use as MCED
paramsToAlter["OncoSim-cervical"] <- list(c("HpvScreeningProtocolDispatcher","HpvDnaTestSensitivitySpecificity","CervicalTestCompliance")) #Planning to hack HPV DNA testing to use as MCED
paramsToAlter["OncoSim-lung"] <- list(c("LcDispatcherDefault")) #Set threshold to 0 for PLC



#These are probably wrong. THESE ARE MEANT AS A TEST. DO NOT KEEP FOR FURTHER ANALYSES
baseParticipation = list()
baseParticipation["OncoSim-colorectal"] = list(c(0.43,0.9))

galleriParticipation = list()
galleriParticipation["OncoSim-colorectal"] = list(c(0.90,1))



galleriEffectivenessValues = list()

#Positivity values obtained from https://journals.sagepub.com/doi/10.1177/10732748241307360
#These are probably wrong. THESE ARE MEANT AS A TEST. DO NOT KEEP FOR FURTHER ANALYSES
galleriEffectivenessValues["OncoSim-colorectal"] = list(c(0.05,0.887,0.887,0.887,0.887))


testCosts = list()
#https://www.galleri.com/patient/the-galleri-test/cost
testCosts["GalleriMCED"] = 1351.76

testFrequency = list()
testFrequency["GalleriMCED"] = 3



scenarios <- c()
for(scenarioType in c("BaseCase","GalleriMCED")){
  for(cancerType in modelsToUse){
    scenarios <- c(scenarios,paste(cancerType,scenarioType,sep="_"))
  }
}


scenarios <- unique(scenarios)


tar_option_set(packages=c("openmpp","tidyverse"),workspace_on_error = TRUE)


list(
  tar_target(scenariosToRun,scenarios),
  tar_target(createdScenarios,CreateScenario(scenariosToRun,Sys.time()),pattern=map(scenariosToRun),iteration="list"),
  tar_target(parametersAltered,SetParameters(createdScenarios),pattern=map(createdScenarios),iteration="list"),
  tar_target(runScenarios,RunScenario(parametersAltered),pattern=map(parametersAltered),iteration = "list")
)