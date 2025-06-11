CleanUpOldVersions <- function(){
  for(modelToUse in modelsToUse){
    print(modelToUse)
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

CreateScenario <- function(currentParameters){

  currentParameters = currentParameters[[1]]

  model= load_model(currentParameters$model)
  baserun_digest = model$ModelRuns$RunDigest[[1]]

  scenarioName <- paste(analysisName,currentParameters$name,sep="")

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

  ## Remove run if it exists
  allPreviousRuns = model$ModelRuns[grepl(scenarioName,model$ModelRuns$Name),]$RunDigest
  for(oldRun in allPreviousRuns){
    delete_run(model$ModelDigest,oldRun)
  }

  scenarioToUse = create_scenario(currentParameters$model,scenarioName,baserun_digest)
  scenarioToUse = load_scenario(currentParameters$model,scenarioName)
  for(param in paramsToAlter[[currentParameters$model]]){
    scenarioToUse$copy_params(param)
  }

  currentParameters$scenario <- list(scenarioToUse)
  return(currentParameters)

}