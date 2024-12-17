CleanUpOldVersions <- function(){
  for(modelToUse in modelsToUse){
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
  scenarioElements = strsplit(scenarioToCreate,"_")[[1]]
  modelToUse = scenarioElements[1]
  model= load_model(modelToUse)
  baserun_digest = model$ModelRuns$RunDigest[[1]]

  scenarioName <- paste(analysisName,scenarioToCreate,sep="")
  scenarioNameRandom <- paste(scenarioName,randomNum,sep="-")

  ## Create and Load Scenario (Delete if it previously existed)
  allPreviousNames <- model$ModelScenarios$Name
  matchingPrevious = allPreviousNames[grepl(scenarioName,allPreviousNames)]
  if(length(matchingPrevious) != 0){
    for(name in matchingPrevious){
      scenarioToUse = load_scenario(modelToUse,name)
      scenarioToUse$ReadOnly <- FALSE
      delete_scenario(modelToUse,name)
    }
  }

  scenarioToUse = create_scenario(modelToUse,scenarioNameRandom,baserun_digest)
  scenarioToUse = load_scenario(modelToUse,scenarioNameRandom)
  for(param in paramsToAlter[[modelToUse]]){
    scenarioToUse$copy_params(param)
  }

  returnList <- list()
  returnList[scenarioToCreate] <- list(scenarioToUse)
  return(returnList)

}