runVisuals <- function() {
	library(targets)
	Sys.setenv(TAR_PROJECT = "project_visualize")
	tar_make()
}
runVisuals()
