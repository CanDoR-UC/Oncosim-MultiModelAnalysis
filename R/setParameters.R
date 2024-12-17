SetParameters <- function(createdScenarios){

  scenario <- createdScenarios[[1]]
  scenarioName = names(createdScenarios)
  parameters <- strsplit(scenarioName,"_")[[1]]

  if(parameters[2] == "GalleriMCED"){
    participationNumbers = galleriParticipation[parameters[1]][[1]]
    effectivenessValues = galleriEffectivenessValues[parameters[1]][[1]]
  }

  if(parameters[2] == "BaseCase"){
    participationNumbers = baseParticipation[parameters[1]][[1]]
    effectivenessValues = "NONE"
    cost = -1
    frequency = -1
  }else{
    cost = testCosts[parameters[2]][[1]]
    frequency = testFrequency[parameters[2]][[1]]
  }

  participation = participationNumbers[1]
  adherence = participationNumbers[2]

  #Mainly derived from the cfDNA project
  if(parameters[1] == "OncoSim-colorectal"){
    costs <- scenario$get_param("CrcScreenCostCanada")
    if(cost != -1){
      costs$FOBT_KIT <- cost
      costs$FOBT_PROCESSING <- 0
    }

    #Removed age. Leave at default.
    scenario$set_param("CrcScreenCostCanada",costs)

    screen_param <- scenario$get_param("CrcScreenRecruitCanada")
    screen_param$`5` <- screen_param$`4` #first copy over element 4 to element 5 to make subsequent change easier
    screen_param$`3`[screen_param$Overall_program_controls=='SR_FIRST_PARTICIPATION'] <- baseParticipation[parameters[1]][[1]][1]
    screen_param$`4`[screen_param$Overall_program_controls=='SR_FIRST_PARTICIPATION'] <- baseParticipation[parameters[1]][[1]][1]
    screen_param$`4`[screen_param$Overall_program_controls=='Recruitment_end_year_integer'] <- 2024
    screen_param$`4`[screen_param$Overall_program_controls=='SR_FIRST_PHASEIN'] <- 2
    screen_param$`5`[screen_param$Overall_program_controls=='Recruitment_start_year_integer'] <- 2025
    screen_param$`5`[screen_param$Overall_program_controls=='SR_FIRST_PARTICIPATION'] <- participation
    screen_param$`5`[screen_param$Overall_program_controls=='SR_FIRST_PHASEIN'] <- 2
    screen_param$`5`[screen_param$Overall_program_controls=='Assign_screening_group'] <- 5
    scenario$set_param("CrcScreenRecruitCanada",screen_param)

    modality_param <- scenario$get_param("CrcScreenPlanCanada")

    modality_param$B <- modality_param$A
    modality_param$H <- modality_param$G #Copy over the existing parameters to make changes easier
    modality_param$B[modality_param$Screening_modality_plan=='Screening_group'] <- 5
    modality_param$H[modality_param$Screening_modality_plan=='Screening_group'] <- -5
    modality_param$B[modality_param$Screening_modality_plan=='Adherence_probability'] <- adherence
    modality_param$H[modality_param$Screening_modality_plan=='Adherence_probability'] <- -1*adherence
    if(frequency >0){
      modality_param$B[modality_param$Screening_modality_plan=='SP_FREQUENCY'] <- frequency
      modality_param$H[modality_param$Screening_modality_plan=='SP_FREQUENCY'] <- frequency
    }
    modality_param$B[modality_param$Screening_modality_plan=='Modality'] <- ifelse(grepl("MCED",scenarioName), 1, 2) #For MCED scenarios, hack the FOBT modality. For fit100, keep the FIT modality.
    
    scenario$set_param("CrcScreenPlanCanada",modality_param)


    fit_param <- scenario$get_param("CrcTestPositivitySuccinct")

    if(effectivenessValues[1] != "NONE"){
      fit_param$FOBT_guaiac[1:5] <- effectivenessValues
      fit_param$FOBT_guaiac[11:15] <- effectivenessValues
    }

    scenario$set_param("CrcTestPositivitySuccinct",fit_param)

  }
  return(createdScenarios)

}