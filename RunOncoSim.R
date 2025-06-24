runOncoSim <- function() {
	library(targets)
	Sys.setenv(TAR_PROJECT = "project_oncosim")
	tar_make()
}
runOncoSim()
