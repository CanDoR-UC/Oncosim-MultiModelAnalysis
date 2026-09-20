CleanUpOldVersions <- function() {
	for (modelToUse in modelsToUse) {
		modelDigest = modelDigests[modelToUse][[1]]

		model = load_model(modelDigest)
		allPreviousNames = model$ModelScenarios$Name

		for (oldScenario in allPreviousNames) {
			if (grepl(analysisName, oldScenario)) {
				scenarioToRemove = load_scenario(modelDigest, oldScenario)
				scenarioToRemove$ReadOnly <- FALSE
				delete_scenario(modelDigest, oldScenario)
			}
		}

		allPreviousRuns = model$ModelRuns[
			grepl(analysisName, model$ModelRuns$Name),
		]$RunDigest

		for (oldRun in allPreviousRuns) {
			delete_run(modelDigest, oldRun)
		}
	}
	unlink("store_oncosim", recursive = TRUE)
	unlink("store_visualize", recursive = TRUE)
}

CreateScenario <- function(currentParameters) {
	currentParameters = currentParameters[[1]]
	modelDigest = modelDigests[currentParameters$model][[1]]
	model = load_model(modelDigest)
	baserun_digest = model$ModelRuns$RunDigest[[1]]

	scenarioName <- paste(analysisName, currentParameters$name, sep = "")

	## Create and Load Scenario (Delete if it previously existed)
	allPreviousNames <- model$ModelScenarios$Name
	matchingPrevious = allPreviousNames[grepl(scenarioName, allPreviousNames)]
	if (length(matchingPrevious) != 0) {
		for (name in matchingPrevious) {
			scenarioToUse = load_scenario(modelDigest, name)
			scenarioToUse$ReadOnly <- FALSE
			delete_scenario(modelDigest, name)
		}
	}

	## Remove run if it exists
	allPreviousRuns = model$ModelRuns[
		grepl(scenarioName, model$ModelRuns$Name),
	]$RunDigest
	for (oldRun in allPreviousRuns) {
		delete_run(model$ModelDigest, oldRun)
	}

	scenarioToUse = create_scenario(
		modelDigest,
		scenarioName,
		baserun_digest
	)
	scenarioToUse = load_scenario(modelDigest, scenarioName)
	for (param in paramsToAlter[[currentParameters$model]]) {
		scenarioToUse$copy_params(param)
	}

	currentParameters$scenario <- list(scenarioToUse)
	return(currentParameters)
}
