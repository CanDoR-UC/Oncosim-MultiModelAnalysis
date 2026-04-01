library(targets)
library(tarchetypes)
library(openmpp)

openmpp::use_OpenMpp_CPAC()

## Source R files ----
purrr::walk(fs::dir_ls("./R", glob = "*.R$"), source)

analysisName = "MCEDProject"

modelsToUse <- c("OncoSim-breast", "OncoSim-colorectal", "OncoSim-lung")

actuallyRun = FALSE

tar_option_set(packages = c("openmpp", "tidyverse"), workspace_on_error = TRUE)


list(
	tar_target(models, modelsToUse),
	tar_target(scenarioSpecifications, generateModelParameters(models)), #49 per model
	tar_target(scenarioSpecificationsSplit, setNames(split(scenarioSpecifications, seq(nrow(scenarioSpecifications))), scenarioSpecifications$name)),
	tar_target(createdScenarios, CreateScenario(scenarioSpecificationsSplit), pattern = map(scenarioSpecificationsSplit), iteration = "list"),
	tar_target(parametersAltered, SetParameters(createdScenarios), pattern = map(createdScenarios), iteration = "list"),
	tar_target(runScenarios, RunScenario(parametersAltered), pattern = map(parametersAltered), iteration = "list")
)
