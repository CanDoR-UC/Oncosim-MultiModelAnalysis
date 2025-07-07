library(targets)
library(tarchetypes)
library(openmpp)


use_OpenMpp_CPAC()

## Source R files ----
purrr::walk(fs::dir_ls("./R", glob = "*.R$"), source)

analysisName = "MCEDProject"

modelsToUse <- c("OncoSim-breast", "OncoSim-colorectal", "OncoSim-cervical", "OncoSim-lung")
#modelsToUse <- c("OncoSim-breast")

actuallyRun = TRUE

tar_option_set(packages = c("openmpp", "tidyverse"), workspace_on_error = TRUE)


list(
	tar_target(models, modelsToUse),
	tar_target(scenarioSpecifications, generateModelParameters(models)),
	tar_target(scenarioSpecificationsSplit, setNames(split(scenarioSpecifications, seq(nrow(scenarioSpecifications))), scenarioSpecifications$name)),
	tar_target(createdScenarios, CreateScenario(scenarioSpecificationsSplit), pattern = map(scenarioSpecificationsSplit), iteration = "list"),
	tar_target(parametersAltered, SetParameters(createdScenarios), pattern = map(createdScenarios), iteration = "list"),
	tar_target(runScenarios, RunScenario(parametersAltered), pattern = map(parametersAltered), iteration = "list")
)

#3 year jumps around a lot. MCED is almost always worse. Trying 1 year for calibration purposes
