#Obtained from previous cfDNA repository
outputAllTables <- function(previousRunOutputs) {
	if (downloadOncoSimRuns) {
		openmpp::use_OpenMpp_CPAC()
		#unlink("Output", recursive = TRUE)
		models = c("OncoSim-colorectal", "OncoSim-lung", "OncoSim-breast")
		for (modelToUse in models) {
			print(modelToUse)
			modelDigest = modelDigests[modelToUse][[1]]

			model = load_model(modelDigest)

			outputDirectory <- paste("Output", sep = "")
			dir.create(outputDirectory, showWarnings = FALSE)

			scenarioDirectory <- paste(outputDirectory, "/Scenarios", sep = "")
			dir.create(scenarioDirectory, showWarnings = FALSE)

			runDirectory <- paste(outputDirectory, "/Runs", sep = "")
			dir.create(runDirectory, showWarnings = FALSE)

			#Write scenarios

			allScenarios <- model$ModelScenarios$Name
			allScenarios <- allScenarios[grepl(analysisName, allScenarios)]
			for (scenarioName in allScenarios) {
				print(scenarioName)
				scenario <- load_scenario(modelDigest, scenarioName)

				specificScenarioDir <- paste(scenarioDirectory, "/", scenarioName, sep = "")
				unlink(specificScenarioDir, recursive = TRUE)
				dir.create(specificScenarioDir, showWarnings = FALSE)

				for (table in paramsToAlter[modelToUse][[1]]) {
					currentTableDir <- paste(specificScenarioDir, "/", table, ".csv", sep = "")
					write.csv(scenario$get_param(table), currentTableDir, row.names = FALSE)
				}
			}

			#Write runs

			allRunTable <- model$ModelRuns

			allRuns <- model$ModelRuns$Name
			allRuns <- allRuns[grepl(analysisName, allRuns)]

			for (runName in allRuns) {
				modelDigest = modelDigests[modelToUse][[1]]

				model = load_model(modelDigest)

				model <- load_run(modelDigest, allRunTable[allRunTable$Name == runName, ]$RunDigest)

				specificRunDir <- paste(runDirectory, "/", runName, sep = "")
				unlink(specificRunDir, recursive = TRUE)
				dir.create(specificRunDir, showWarnings = FALSE)
				model$write_tables(specificRunDir)
			}
		}
	}

	allTables = list()

	for (breastTable in outputTables["OncoSim-breast"][[1]]) {
		allTables[breastTable] = list(getOncoSimTables(breastTable, "breast"))
	}
	for (colonTable in outputTables["OncoSim-colorectal"][[1]]) {
		allTables[colonTable] = list(getOncoSimTables(colonTable, "colorectal"))
	}
	for (lungTable in outputTables["OncoSim-lung"][[1]]) {
		allTables[lungTable] = list(getOncoSimTables(lungTable, "lung"))
	}
	allTables
}
