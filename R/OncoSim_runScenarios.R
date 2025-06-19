RunScenario <- function(parametersAltered) {
	currentScenario <- parametersAltered$scenario[[1]]

	currentScenario$ReadOnly <- TRUE

	fullSampleSize = 32000000
	testSampleSize = 300000

	if (actuallyRun) {
		#Change to 4
		currentScenario$run(
			paste("AutomatedRun_", analysisName, parametersAltered$name),
			opts = opts_run(
				Tables = outputTables[currentScenario$ModelName][[1]],
				SimulationCases = testSampleSize,
				Mpi = list(Np = 4, IsNotOnRoot = TRUE),
				Opts = list(OpenM.Threads = "4")
			)
		)
		print("RUN HERE")
	}

	parametersAltered
}
