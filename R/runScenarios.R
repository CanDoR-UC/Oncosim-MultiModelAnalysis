RunScenario <- function(parametersAltered){
  currentScenario <- parametersAltered[[1]]

  currentScenario$ReadOnly <- TRUE
  if(grepl("colorectal",currentScenario$ModelName)){
    neededTables <- list("Colorectal_Cancer_Cases_Table",
    "Colorectal_Cancer_Screen_Table",
    "Colorectal_Cancer_Screen_Outcome_Positivity_Table",
    "Colorectal_Cancer_ScreenState_Outcomes_Table",
    "Colorectal_Cancer_ICER_Table",
    "Colorectal_Cancer_ICER_Table_Discounted",
    "Colorectal_Cancer_Outcomes_GASY_UP_Table",
    "Population_Year1972_2051_Table"
)
  }


  fullSampleSize = 32000000
  testSampleSize = 1000000
  currentScenario$run(paste("AutomatedRun_",analysisName,names(parametersAltered)),opts=opts_run(Tables=neededTables,SimulationCases=testSampleSize,Mpi = list(Np = 4, IsNotOnRoot = TRUE), Opts = list(OpenM.Threads = "4")))
  print("RUN HERE")


}