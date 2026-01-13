library(targets)
library(tarchetypes)

tar_source()

tar_option_set(packages = c("tidyverse", "openmpp", "quarto"), workspace_on_error = TRUE)

analysisName = "MCEDProject"

## Source R files ----
purrr::walk(fs::dir_ls("./R", glob = "*.R$"), source)


#If OncoSim runs need to be downloaded. Disable this if they have been deleted from the server or have already been downloaded.
downloadOncoSimRuns = FALSE

list(
	tar_target(previousRunOutputs, "Placeholder"),
	tar_target(importedData, outputAllTables(previousRunOutputs)),
	tar_target(combinedTables, combineModelTables(importedData)),
	tar_target(validationReportQuarto, "Validation_Report.qmd", format = "file"),
	tar_target(finalReportQuarto, "Final_Report.qmd", format = "file"),
	tar_target(generateValidationReport, generateQuarto(validationReportQuarto, combinedTables)),
	tar_target(generateFinalReport, generateQuarto(finalReportQuarto, combinedTables))
)
