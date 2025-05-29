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

  if(parameters[1] == "OncoSim-cervical"){
    if(parameters[2] != "BaseCase"){

      #Set colposcopy Cost to what we need
      testCosts = scenario$get_param("HpvScreeningPreCancerTreatmentCosts")
      testCosts$HPV_TEST_TREAT_x_OSCOPAP_INITIAL = cost
      testCosts$HPV_TEST_TREAT_x_REASSESS_EARLY = cost
      testCosts$HPV_TEST_TREAT_x_P_REASSESS_LATE = cost
      testCosts$HPV_TEST_TREAT_x_OLPONLY_INITIAL = cost
      testCosts$HPV_TEST_TREAT_x_REASSESS_Enum6 = cost
      testCosts$HPV_TEST_TREAT_x_Y_REASSESS_LATE = cost

      scenario$set_param("HpvScreeningPreCancerTreatmentCosts",testCosts)


      dispatcher = scenario$get_param("HpvScreeningProtocolDispatcher")
      dispatcher$`15` = dispatcher$`2`
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS=="Visibility_prob_visible"] = 1
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS %in% c("Minimum_recruit_age_integer","Minimum_re_screen_age_integer")] = startAge
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS=="Maximum_recruit_age_integer"] = endAge
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS %in% c("HPV_SCREENING_x_IN_RECRUIT_TIME","HPV_SCREENING_x_N_RESCREEN_TIME")] = 2025
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS=="Recruitment_rate"] = participation
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS=="Rescreen_rate"] = adherence
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS %in% c("HPV_SCREENING_x_L_MIN_FREQUENCY","HPV_SCREENING_x_L_MAX_FREQUENCY")] = frequency
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS=="Protocol_to_be_used"] = 3
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS=="HPV_SCREENING_x_RTING_APPARATUS"] = 3.1 #3.1 : Colposcopy (with or without PAP) with follow-up logic 1

      scenario$set_param("HpvScreeningProtocolDispatcher",dispatcher)

      sensSpec = scenario$get_param("CytologyResultCarcino")
      sensSpec[5:12] = 0
      sensSpec$All_cells_normal = 1

      sensSpec$All_cells_normal[sensSpec$Progression_status == "No_infection_and_no_lesion"] = effectivenessValues[2]#Probability of correctly saying that its negative (specificity)
      sensSpec$Has_cervical_cancer[sensSpec$Progression_status == "No_infection_and_no_lesion"] = 1-effectivenessValues[2]# 1- Probability of correctly saying that its negative (specificity)

      sensSpec$All_cells_normal[sensSpec$Progression_status== "Cervical_cancer"] = 1-effectivenessValues[1] #1- Probability of correctly sayign there is cervical cancer (sensitivity)
      sensSpec$Has_cervical_cancer[sensSpec$Progression_status== "Cervical_cancer"] = effectivenessValues[1] #1- Probability of correctly sayign there is cervical cancer (sensitivity)
      scenario$set_param("CytologyResultCarcino",sensSpec)

    }
  }

  if(parameters[1] == "OncoSim-lung"){

    followUpProtocol = scenario$get_param("LungCancerScreenFollowupProtcol")
    followUpProtocol[[1]] = "Custom"
    scenario$set_param("LungCancerScreenFollowupProtcol",followUpProtocol)

    if(parameters[2] != "BaseCase"){
      LcScreenCost = scenario$get_param("LcScreenCost") #Set screen as MCED cost. No interpretation cost
      LcScreenCost$LCS_LDCT_SCAN = cost
      LcScreenCost$LCS_LDCT_SCAN_PROCESSING = 0
      scenario$set_param("LcScreenCost",LcScreenCost)

      LcScreenEfficacy = scenario$get_param("LcScreenEfficacy") #Set sensitivty and specificity. May have NO impact on the selected screening type?
      LcScreenEfficacy$Sensitivity = participationNumbers[1]
      LcScreenEfficacy$Specificity = participationNumbers[2]
      scenario$set_param("LcScreenEfficacy",LcScreenEfficacy)

      LcDispatcherDefault = scenario$get_param("LcDispatcherDefault") #Screening dispatcher

      #Only need one column. Blank out the other one
      LcDispatcherDefault$`3` = 0
      LcDispatcherDefault$`2`[LcDispatcherDefault$Characteristics_of_program=="LUNG_SCREENING_x_OCOL_VISIBILITY"] = 1
      LcDispatcherDefault$`2`[LcDispatcherDefault$Characteristics_of_program=="LUNG_SCREENING_x_MIN_RECRUIT_AGE"] = startAge
      LcDispatcherDefault$`2`[LcDispatcherDefault$Characteristics_of_program=="LUNG_SCREENING_x_MAX_SCREEN_AGE"] = endAge
      LcDispatcherDefault$`2`[LcDispatcherDefault$Characteristics_of_program=="LUNG_SCREENING_x_MAX_RECRUIT_AGE"] = endAge
      LcDispatcherDefault$`2`[LcDispatcherDefault$Characteristics_of_program=="Recruitment_start_year_integer"] = 2025
      LcDispatcherDefault$`2`[LcDispatcherDefault$Characteristics_of_program=="LUNG_SCREENING_x_N_PARTICIPATION"] = participation
      LcDispatcherDefault$`2`[LcDispatcherDefault$Characteristics_of_program=="Rescreen_rate"] = adherence

      LcDispatcherDefault$`2`[LcDispatcherDefault$Characteristics_of_program %in% c("LUNG_SCREENING_x_ST_SEQ_HIGHRISK","LUNG_SCREENING_x_ND_SEQ_HIGHRISK","LUNG_SCREENING_x_EQ_VERYHIGHRISK","LUNG_SCREENING_x_EQ_VERYHIEnum22")] = frequency
      scenario$set_param("LcDispatcherDefault",LcDispatcherDefault)

    }
  }

  return(createdScenarios)

}