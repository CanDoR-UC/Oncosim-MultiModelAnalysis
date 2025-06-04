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
  if(grepl("cervical",currentScenario$ModelName)){
    neededTables <- list(
      "Cervical_Cancer_Cases_Table",
      "Cervical_Cancer_Timing_Of_Last_Screen",
      "Cervical_Cancer_Costs_Table",
      "Cervical_Cancer_ICER_Table",
      "Hpv_Screening_Costs_Prov_Table",
      "HPV_Colposcopy_Results_Table",
      "Hpv_Screening_Costs_Age_Table",
      "HPV_DNA_Results_Table")
  }

  if(grepl("lung",currentScenario$ModelName)){
    neededTables <- list(
      "Lung_Cancer_Cases_Table",
      "Lung_Cancer_ScreenCases_Year_Table",
      "Lung_Cancer_Costs_Table",
      "Lung_Cancer_ICER_Table"
    )
  }


  fullSampleSize = 32000000
  testSampleSize = 1000000
  if(actuallyRun){#Change to 4
    currentScenario$run(paste("AutomatedRun_",analysisName,names(parametersAltered)),opts=opts_run(Tables=neededTables,SimulationCases=testSampleSize,Mpi = list(Np = 4, IsNotOnRoot = TRUE), Opts = list(OpenM.Threads = "4")))
    print("RUN HERE")
  }


}