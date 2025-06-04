CleanUpOldVersions <- function(){
  for(modelToUse in modelsToUse){
    print(modelsToUse)
    model = load_model(modelToUse)
    allPreviousNames = model$ModelScenarios$Name

    for(oldScenario in allPreviousNames){
      if(grepl(analysisName,oldScenario)){
        scenarioToRemove = load_scenario(modelToUse,oldScenario)
        scenarioToRemove$ReadOnly <- FALSE
        delete_scenario(modelToUse,oldScenario)
      }
    }

    allPreviousRuns = model$ModelRuns[grepl(analysisName,model$ModelRuns$Name),]$RunDigest

    for(oldRun in allPreviousRuns){
        delete_run(model$ModelDigest,oldRun)
    }

  }
}

CreateScenario <- function(scenarioToCreate,time){

  currentParameters = parameterTables[parameterTables$name == scenarioToCreate,]

  model= load_model(currentParameters$model)
  baserun_digest = model$ModelRuns$RunDigest[[1]]

  scenarioName <- paste(analysisName,scenarioToCreate,sep="")
  scenarioNameRandom <- paste(scenarioName,randomNum,sep="-")

  ## Create and Load Scenario (Delete if it previously existed)
  allPreviousNames <- model$ModelScenarios$Name
  matchingPrevious = allPreviousNames[grepl(scenarioName,allPreviousNames)]
  if(length(matchingPrevious) != 0){
    for(name in matchingPrevious){
      scenarioToUse = load_scenario(currentParameters$model,name)
      scenarioToUse$ReadOnly <- FALSE
      delete_scenario(currentParameters$model,name)
    }
  }

  scenarioToUse = create_scenario(currentParameters$model,scenarioNameRandom,baserun_digest)
  scenarioToUse = load_scenario(currentParameters$model,scenarioNameRandom)
  for(param in paramsToAlter[[currentParameters$model]]){
    scenarioToUse$copy_params(param)
  }

  returnList <- list()
  returnList[scenarioToCreate] <- list(scenarioToUse)
  return(returnList)

}