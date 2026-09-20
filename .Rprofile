source("renv/activate.R")
runVisuals <- function() {
	library(targets)
	Sys.setenv(TAR_PROJECT = "project_visualize")
	tar_make()
}
runOncoSim <- function() {
	library(targets)
	Sys.setenv(TAR_PROJECT = "project_oncosim")
	tar_make()
}
