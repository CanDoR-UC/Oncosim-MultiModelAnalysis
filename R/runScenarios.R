RunScenario <- function(parametersAltered){
  currentScenario <- parametersAltered[[1]]

  currentScenario$ReadOnly <- TRUE
  if(grepl("colorectal",currentScenario$ModelName)){
    neededTables <- list("Colorectal_Cancer_Cases_Table",
    "Colorectal_Cancer_Screen_Table",
    "Colorectal_Cancer_ScreenState_Outcomes_Table"
)
  }
  if(grepl("breast",currentScenario$ModelName)){
    neededTables <- list("Breast_Cancer_Cases_Table",
    "Breast_Cancer_Screen_Outcome_Modality_Table"
)
  }


  fullSampleSize = 32000000
  testSampleSize = 100000
  if(actuallyRun){#Change to 4
    currentScenario$run(paste("AutomatedRun_",analysisName,names(parametersAltered)),opts=opts_run(Tables=neededTables,SimulationCases=testSampleSize,Mpi = list(Np = 4, IsNotOnRoot = TRUE), Opts = list(OpenM.Threads = "4")))
    print("RUN HERE")
  }


}