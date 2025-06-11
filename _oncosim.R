library(targets)
library(tarchetypes)
library(openmpp)


use_OpenMpp_CPAC()

## Source R files ----
purrr::walk(fs::dir_ls("./R", glob = "*.R$"), source)

analysisName = "MCEDTesting"

#This number is added to scenario and run names to avoid collision

modelsToUse <- c("OncoSim-breast","OncoSim-colorectal","OncoSim-cervical","OncoSim-lung")
#modelsToUse <- c("OncoSim-breast")




actuallyRun = TRUE

tar_option_set(packages=c("openmpp","tidyverse"),workspace_on_error = TRUE)


list(
  tar_target(models,modelsToUse),
  tar_target(scenarioSpecifications,generateModelParameters(models)),
  tar_target(scenarioSpecificationsSplit,setNames(split(scenarioSpecifications,seq(nrow(scenarioSpecifications))),scenarioSpecifications$name)),
  tar_target(createdScenarios,CreateScenario(scenarioSpecificationsSplit),pattern=map(scenarioSpecificationsSplit),iteration="list"),
  tar_target(parametersAltered,SetParameters(createdScenarios),pattern=map(createdScenarios),iteration="list"),
  tar_target(runScenarios,RunScenario(parametersAltered),pattern=map(parametersAltered),iteration = "list")
)




# Colorectal seems to work. Galleri has more screen_detected cancers than CancerSeek, which makes sense. in 2027, base case still has FIT and the MCED only have FOBT as expected.
# May want to improve the polyp detection?

# Lung should be OK? The screen positives are odd (More in CancerSeek), but the screen detected cancners are as they should be (More in Galleri)
# Will need to figure out how many screens make sense for the base case.


# Breast should be OK. No easily discernable problems so far.

#Cervical seems to be working. May have to recheck the FollowUpCytologyProtocol1 setup