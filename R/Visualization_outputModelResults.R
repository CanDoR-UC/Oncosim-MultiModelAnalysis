#Obtained from previous cfDNA repository
outputAllTables <- function(previousRunOutputs){

  if(!downloadOncoSimRuns){
    return()
  }
  openmpp::use_OpenMpp_CPAC()
  unlink("Output",recursive = TRUE)

  for(modelToUse in c("OncoSim-colorectal","OncoSim-lung","OncoSim-breast","OncoSim-cervical")){

    model= load_model(modelToUse)


    outputDirectory <- paste("Output",sep="")
    dir.create(outputDirectory,showWarnings = FALSE)
  
    scenarioDirectory <- paste(outputDirectory,"/Scenarios",sep="")
    dir.create(scenarioDirectory,showWarnings = FALSE)
  
    runDirectory <- paste(outputDirectory,"/Runs",sep="")
    dir.create(runDirectory,showWarnings = FALSE)
  
  
  
    #Write scenarios
  
    allScenarios <- model$ModelScenarios$Name
    allScenarios <- allScenarios[grepl(analysisName,allScenarios)]
    for(scenarioName in allScenarios){
      scenario <- load_scenario(modelToUse,scenarioName)
  
      specificScenarioDir <- paste(scenarioDirectory,"/",scenarioName,sep="")
      dir.create(specificScenarioDir,showWarnings = FALSE)
  
      for(table in paramsToAlter[modelToUse][[1]]){
        currentTableDir <- paste(specificScenarioDir,"/",table,".csv",sep="")
        write.csv(scenario$get_param(table),currentTableDir,row.names=FALSE)
      }
  
    }

    #Write runs
  
    allRunTable <- model$ModelRuns
  
    allRuns <- model$ModelRuns$Name
    allRuns <- allRuns[grepl(analysisName,allRuns)]
  
    for(runName in allRuns){
      model <- load_run(modelToUse,allRunTable[allRunTable$Name == runName,]$RunDigest)
  
      specificRunDir <- paste(runDirectory,"/",runName,sep="")
      dir.create(specificRunDir,showWarnings = FALSE)
      model$write_tables(specificRunDir)

  }
    
  }
}

