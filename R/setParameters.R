SetParameters <- function(createdScenarios){

  scenario <- createdScenarios[[1]]
  scenarioName = names(createdScenarios)
  parameters <- strsplit(scenarioName,"_")[[1]]

  if(parameters[2] == "GalleriMCED"){
    participationNumbers = galleriParticipation[parameters[1]][[1]]
    effectivenessValues = galleriEffectivenessValues[parameters[1]][[1]]
  }

  if(parameters[2] != "BaseCase"){
    cost = testCosts[parameters[2]][[1]]
    frequency = testFrequency[parameters[2]][[1]]

    participation = participationNumbers[1]
    adherence = participationNumbers[2]
  }



  if(parameters[1] == "OncoSim-colorectal"){

    #Set MCED Cost
    if(parameters[2] != "BaseCase"){
      costs <- scenario$get_param("CrcScreenCostCanada")
      if(cost != -1){
        costs$FOBT_KIT <- cost
        costs$FOBT_PROCESSING <- 0
      }
  
      scenario$set_param("CrcScreenCostCanada",costs)
  
      #Set up parameters for how MCED screening works.
      screen_param <- scenario$get_param("CrcScreenRecruitCanada")
      screen_param$`5` <- screen_param$`4` #first copy over element 4 to element 5 to make subsequent change easier
      screen_param$`5`[screen_param$Overall_program_controls=='Recruitment_start_year_integer'] <- 2025
      screen_param$`5`[screen_param$Overall_program_controls=='Recruitment_start_age_integer'] <- startAge
      screen_param$`5`[screen_param$Overall_program_controls=='Recruitment_end_age_integer'] <- endAge
      screen_param$`5`[screen_param$Overall_program_controls=='SR_FIRST_PARTICIPATION'] <- participation
      screen_param$`5`[screen_param$Overall_program_controls=='SR_FIRST_PHASEIN'] <- 2
      screen_param$`5`[screen_param$Overall_program_controls=='Assign_screening_group'] <- 0 #0 Means that anyone can be in this group
      scenario$set_param("CrcScreenRecruitCanada",screen_param)
  
      #Creating plan to dictate how people are screened with MCED
      modality_param <- scenario$get_param("CrcScreenPlanCanada")
      modality_param$B <- modality_param$A
      modality_param$B[modality_param$Screening_modality_plan=='Screening_group'] <- 0
      modality_param$B[modality_param$Screening_modality_plan=='Adherence_probability'] <- adherence
      modality_param$B[modality_param$Screening_modality_plan=='SP_FREQUENCY'] <- frequency
      modality_param$B[modality_param$Screening_modality_plan=='Modality'] <- 1  #For MCED scenarios, hack the FOBT modality

      scenario$set_param("CrcScreenPlanCanada",modality_param)
  
      #Altering effectiveness to emulate MCED
      fit_param <- scenario$get_param("CrcTestPositivitySuccinct")
      if(effectivenessValues[1] != "NONE"){
        fit_param$FOBT_guaiac[1:5] <- effectivenessValues
        fit_param$FOBT_guaiac[11:15] <- effectivenessValues
      }
      scenario$set_param("CrcTestPositivitySuccinct",fit_param)
    }else{
      #Set base parameters here.
    }
    

  }
  if(parameters[1] == "OncoSim-breast"){

    if(parameters[2] != "BaseCase"){
     #Set MCED Cost May have to change the other costs too?
     costs <- scenario$get_param("BreastScreeningCosts")
     if(cost != -1){
       costs[costs$Breast_screening_modalities == "Alternative_modality_E",]$Cost_of_screen <- cost
     }
     scenario$set_param("BreastScreeningCosts",costs)
      
      #Set up parameters for how MCED screening works.
      screen_param <- scenario$get_param("BreastScreeningProtocolDispatcher")
      reference = screen_param$`3`[screen_param$Screening_era == "Future_screening_era"]
      screen_param$`5`[screen_param$Screening_era == "Future_screening_era"] <- reference
      screen_param$`5`[screen_param$Screening_era == "Past_screening_era"] <- reference

      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='Visibility_prob_visible'] <- 1
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='Minimum_recruit_age_integer'] <- startAge
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='Maximum_recruit_age_integer'] <- endAge
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='Recruitment_rate'] <- participation
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='Rescreen_rate'] <- adherence #Might be wrong
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='Protocol_to_be_used'] <- 1 
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='BREAST_SCREENI_x_RTING_APPARATUS'] <- 8 #Alt modality E
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='BREAST_SCREENI_x_L_MIN_FREQUENCY'] <- frequency
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='BREAST_SCREENI_x_L_MAX_FREQUENCY'] <- frequency 
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='BREAST_SCREENI_x_IN_RECRUIT_TIME'] <- 2025 

      scenario$set_param("BreastScreeningProtocolDispatcher",screen_param)

      specificity_param <- scenario$get_param("BreastScreeningSpecificityNew")
      specificity = effectivenessValues[1]
      specificity_param[specificity_param$Breast_screening_modalities=="Alternative_modality_E",]$`(-∞,40)` = specificity
      specificity_param[specificity_param$Breast_screening_modalities=="Alternative_modality_E",]$`[40,50)` = specificity
      specificity_param[specificity_param$Breast_screening_modalities=="Alternative_modality_E",]$`[50,60)` = specificity
      specificity_param[specificity_param$Breast_screening_modalities=="Alternative_modality_E",]$`[60,70)` = specificity
      specificity_param[specificity_param$Breast_screening_modalities=="Alternative_modality_E",]$`[70,∞)` = specificity
      scenario$set_param("BreastScreeningSpecificityNew",specificity_param)

      sensitivity_param <- scenario$get_param("BreastScreeningSensitivity")
      sensitivity = effectivenessValues[2]
      for(column in c("Tumor_size_data_point_1","Tumor_size_data_point_2","Tumor_size_data_point_3","Tumor_size_data_point_4","Tumor_size_data_point_5","Tumor_size_data_point_6","Tumor_size_data_point_7","Tumor_size_data_point_8","Tumor_size_data_point_9","Tumor_size_data_point_10")){
        sensitivity_param[sensitivity_param$Breast_screening_modalities=="Alternative_modality_E",][[column]] = sensitivity
      }
      scenario$set_param("BreastScreeningSensitivity",sensitivity_param)

    }


    
  }

  return(createdScenarios)

}