SetParameters <- function(createdScenarios){
  
  scenario <- createdScenarios$scenario[[1]]
  scenarioName = createdScenarios$name
  currentParameters = createdScenarios




  if(currentParameters$model =="OncoSim-colorectal"){

    if(currentParameters$type != "BaseCase"){

      #Set MCED Cost
      costs <- scenario$get_param("CrcScreenCostCanada")
      costs$FOBT_KIT <- currentParameters$cost
      costs$FOBT_PROCESSING <- 0
      scenario$set_param("CrcScreenCostCanada",costs)
  
      #Set up parameters for how MCED screening works.
      screen_param <- scenario$get_param("CrcScreenRecruitCanada")
      screen_param$`5` <- screen_param$`4` #first copy over element 4 to element 5 to make subsequent change easier
      screen_param$`5`[screen_param$Overall_program_controls=='Recruitment_start_year_integer'] <- 2025
      screen_param$`5`[screen_param$Overall_program_controls=='Recruitment_start_age_integer'] <- currentParameters$startAge
      screen_param$`5`[screen_param$Overall_program_controls=='Recruitment_end_year_integer'] <- 9999
      screen_param$`5`[screen_param$Overall_program_controls=='Recruitment_end_age_integer'] <- currentParameters$endAge
      screen_param$`5`[screen_param$Overall_program_controls=='SR_FIRST_PARTICIPATION'] <- currentParameters$actualParticipation
      screen_param$`5`[screen_param$Overall_program_controls=='SR_FIRST_PHASEIN'] <- 2
      screen_param$`5`[screen_param$Overall_program_controls=='Assign_screening_group'] <- 5

      #DISABLE #4 TO ENSURE NO FIT TESTS ARE PERFORMED ANYMORE
      screen_param$`4`[screen_param$Overall_program_controls=='Recruitment_end_year_integer'] <- 2025

      scenario$set_param("CrcScreenRecruitCanada",screen_param)
  
      #Creating plan to dictate how people are screened with MCED
      modality_param <- scenario$get_param("CrcScreenPlanCanada")
      modality_param$B <- modality_param$A
      modality_param$B[modality_param$Screening_modality_plan=='Screening_group'] <- 5
      modality_param$B[modality_param$Screening_modality_plan=='Adherence_probability'] <- currentParameters$actualParticipation
      modality_param$B[modality_param$Screening_modality_plan=='SP_FREQUENCY'] <- currentParameters$frequency      
      modality_param$B[modality_param$Screening_modality_plan=='Age_start_in_ticks'] <- currentParameters$startAge
      modality_param$B[modality_param$Screening_modality_plan=='Modality'] <- 1  #For MCED scenarios, hack the FOBT modality

      modality_param$A[modality_param$Screening_modality_plan=='Time_end_continuous_time'] <- 2026


      scenario$set_param("CrcScreenPlanCanada",modality_param)
  
      #Altering effectiveness to emulate MCED
      fit_param <- scenario$get_param("CrcTestPositivitySuccinct")
      # 1-Specificity for "No polyp or cancer" and all others. If a polyp is detected, this is a false positive. Sensitivity for cancer
      otherValues = as.numeric(scales::number(1-currentParameters$specificity,accuracy=0.001))
      effectivenessValues = c(otherValues,otherValues,otherValues,otherValues,currentParameters$sensitivity)
      fit_param$FOBT_guaiac[1:5] <- effectivenessValues
      fit_param$FOBT_guaiac[11:15] <- effectivenessValues
      
      scenario$set_param("CrcTestPositivitySuccinct",fit_param)
    }else{
      #Set base parameters here.
    }
    

  }
  if(currentParameters$model == "OncoSim-breast"){

    if(currentParameters$type != "BaseCase"){
     #Set MCED Cost May have to change the other costs too?
     costs <- scenario$get_param("BreastScreeningCosts")
     costs[costs$Breast_screening_modalities == "Alternative_modality_E",]$Cost_of_screen <- currentParameters$cost
     scenario$set_param("BreastScreeningCosts",costs)
      
      #Set up parameters for how MCED screening works.
      screen_param <- scenario$get_param("BreastScreeningProtocolDispatcher")
      reference = screen_param$`3`[screen_param$Screening_era == "Future_screening_era"]
      screen_param$`5`[screen_param$Screening_era == "Future_screening_era"] <- reference
      screen_param$`5`[screen_param$Screening_era == "Past_screening_era"] <- reference

      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='Visibility_prob_visible'] <- 1
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='Minimum_recruit_age_integer'] <- currentParameters$startAge
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='Maximum_recruit_age_integer'] <- currentParameters$endAge
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='Recruitment_rate'] <- currentParameters$actualParticipation
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='Rescreen_rate'] <- currentParameters$actualAdherence
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='Protocol_to_be_used'] <- 1 
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='BREAST_SCREENI_x_RTING_APPARATUS'] <- 8 #Alt modality E
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='BREAST_SCREENI_x_L_MIN_FREQUENCY'] <- currentParameters$frequency
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='BREAST_SCREENI_x_L_MAX_FREQUENCY'] <- currentParameters$frequency 
      screen_param$`5`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='BREAST_SCREENI_x_IN_RECRUIT_TIME'] <- 2025 

      #Disable previous
      screen_param$`2`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='BREAST_SCREENI_x_AX_RECRUIT_TIME'] <- 2025 
      screen_param$`3`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='BREAST_SCREENI_x_AX_RECRUIT_TIME'] <- 2025 
      screen_param$`2`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='BREAST_SCREENI_x_X_RESCREEN_TIME'] <- 2025 
      screen_param$`3`[screen_param$BREAST_SCREENI_x_CHARACTERISTICS=='BREAST_SCREENI_x_X_RESCREEN_TIME'] <- 2025 

      
      scenario$set_param("BreastScreeningProtocolDispatcher",screen_param)

      specificity_param <- scenario$get_param("BreastScreeningSpecificityNew")
      specificity = currentParameters$specificity
      specificity_param[specificity_param$Breast_screening_modalities=="Alternative_modality_E",]$`(-∞,40)` = specificity
      specificity_param[specificity_param$Breast_screening_modalities=="Alternative_modality_E",]$`[40,50)` = specificity
      specificity_param[specificity_param$Breast_screening_modalities=="Alternative_modality_E",]$`[50,60)` = specificity
      specificity_param[specificity_param$Breast_screening_modalities=="Alternative_modality_E",]$`[60,70)` = specificity
      specificity_param[specificity_param$Breast_screening_modalities=="Alternative_modality_E",]$`[70,∞)` = specificity
      scenario$set_param("BreastScreeningSpecificityNew",specificity_param)

      sensitivity_param <- scenario$get_param("BreastScreeningSensitivity")
      sensitivity = currentParameters$sensitivity
      for(column in c("Tumor_size_data_point_1","Tumor_size_data_point_2","Tumor_size_data_point_3","Tumor_size_data_point_4","Tumor_size_data_point_5","Tumor_size_data_point_6","Tumor_size_data_point_7","Tumor_size_data_point_8","Tumor_size_data_point_9","Tumor_size_data_point_10")){
        sensitivity_param[sensitivity_param$Breast_screening_modalities=="Alternative_modality_E",][[column]] = sensitivity
      }
      scenario$set_param("BreastScreeningSensitivity",sensitivity_param)

    }


    
  }

  if(currentParameters$model == "OncoSim-cervical"){
    
    if(currentParameters$type != "BaseCase"){

      #Set pap Cost to what we need. Only the initial 
      testCosts = scenario$get_param("HpvScreeningPreCancerTreatmentCosts")
      testCosts$Cytology_PAP_screen = currentParameters$cost

      scenario$set_param("HpvScreeningPreCancerTreatmentCosts",testCosts)


      dispatcher = scenario$get_param("HpvScreeningProtocolDispatcher")
      dispatcher$`15` = dispatcher$`2`
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS=="Visibility_prob_visible"] = 1
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS %in% c("Minimum_recruit_age_integer","Minimum_re_screen_age_integer")] = currentParameters$startAge
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS=="Maximum_recruit_age_integer"] = currentParameters$endAge
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS %in% c("HPV_SCREENING_x_IN_RECRUIT_TIME","HPV_SCREENING_x_N_RESCREEN_TIME")] = 2025
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS=="Recruitment_rate"] = currentParameters$actualParticipation
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS=="Rescreen_rate"] = currentParameters$actualAdherence
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS %in% c("HPV_SCREENING_x_L_MIN_FREQUENCY","HPV_SCREENING_x_L_MAX_FREQUENCY")] = currentParameters$frequency
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS=="Protocol_to_be_used"] = 1
      dispatcher$`15`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS=="HPV_SCREENING_x_RTING_APPARATUS"] = 1.1 # 1.1  1.1 : Cytology (PAP) with follow-up logic 1

      dispatcher$`2`[dispatcher$HPV_SCREENING_x_CHARACTERISTICS %in% c("HPV_SCREENING_x_AX_RECRUIT_TIME","HPV_SCREENING_x_X_RESCREEN_TIME")] = 2025


      scenario$set_param("HpvScreeningProtocolDispatcher",dispatcher)

      sensSpec = scenario$get_param("CytologyResultCarcino")
      sensSpec[5:12] = 0
      sensSpec$All_cells_normal = 1

      sensSpec$All_cells_normal[sensSpec$Progression_status == "No_infection_and_no_lesion"] = currentParameters$specificity#Probability of correctly saying that its negative (specificity)
      sensSpec$Has_cervical_cancer[sensSpec$Progression_status == "No_infection_and_no_lesion"] = 1-currentParameters$specificity# 1- Probability of correctly saying that its negative (specificity)

      sensSpec$All_cells_normal[sensSpec$Progression_status== "Cervical_cancer"] = 1-currentParameters$sensitivity #1- Probability of correctly sayign there is cervical cancer (sensitivity)
      sensSpec$Has_cervical_cancer[sensSpec$Progression_status== "Cervical_cancer"] = currentParameters$sensitivity #1- Probability of correctly sayign there is cervical cancer (sensitivity)
      scenario$set_param("CytologyResultCarcino",sensSpec)


      FollowUpCytologyProtocol = scenario$get_param("FollowUpCytologyProtocol1")
      #MCED only detects cancer or no cancer. Meaning that all non-cancer findings must be referred to the "all-cells normal" pipeline
      FollowUpCytologyProtocol$Outcome_of_the_test_treatment = 1 
      
      #May have to recheck
      FollowUpCytologyProtocol[FollowUpCytologyProtocol$Follow_up_element==7,]$Outcome_of_the_test_treatment = 8

      FollowUpCytologyProtocol[FollowUpCytologyProtocol$Follow_up_element==8,]$Outcome_of_the_test_treatment = 8

      #Colposcopy should always follow. Never go back to pap after detection
      FollowUpCytologyProtocol[FollowUpCytologyProtocol$Follow_up_element==1,]$Apparatus_used_in_the_follow_up = 3.2

      scenario$set_param("FollowUpCytologyProtocol1",FollowUpCytologyProtocol)

    }
  }

  if(currentParameters$model == "OncoSim-lung"){

    #Need to alter the follow-up prototocol
    followUpProtocol = scenario$get_param("LungCancerScreenFollowupProtcol")
    followUpProtocol[[1]] = "Custom"
    scenario$set_param("LungCancerScreenFollowupProtcol",followUpProtocol)

    LcDispatcherDefault = scenario$get_param("LcDispatcherDefault") #Screening dispatcher
    #Enable screening
    LcDispatcherDefault$`3`[LcDispatcherDefault$Characteristics_of_program=="LUNG_SCREENING_x_OCOL_VISIBILITY"] = 1
    LcDispatcherDefault$`2`[LcDispatcherDefault$Characteristics_of_program=="LUNG_SCREENING_x_OCOL_VISIBILITY"] = 1
    scenario$set_param("LcDispatcherDefault",LcDispatcherDefault)


    if(currentParameters$type != "BaseCase"){
      LcScreenCost = scenario$get_param("LcScreenCost") #Set screen as MCED cost. No interpretation cost
      LcScreenCost$LCS_LDCT_SCAN = currentParameters$cost
      LcScreenCost$LCS_LDCT_SCAN_PROCESSING = 0
      scenario$set_param("LcScreenCost",LcScreenCost)


      #Cancer detectable - 6(Highest risk) = True positive ~ Sensitivity
      # Cancer detectable - 1(Lowest risk) = False negative ~ 1 - Sensitivity
      # No cancer detectable - 6(Highest risk) = False positive ~ 1 - Specificity
      # No cancer detectable - 1(Lowest risk) = True negative ~ Specificity

      LungRadsScreenRegularDist = scenario$get_param("LungRadsScreenRegularDist") 


      LungRadsScreenRegularDist[4:9] = 0
      LungRadsScreenRegularDist[LungRadsScreenRegularDist$Cancer_detectable_at_screen == "No_cancer_detectable",]$X_1 = currentParameters$specificity
      LungRadsScreenRegularDist[LungRadsScreenRegularDist$Cancer_detectable_at_screen == "No_cancer_detectable",]$X_6 = 1 - currentParameters$specificity

      LungRadsScreenRegularDist[LungRadsScreenRegularDist$Cancer_detectable_at_screen == "Cancer_detectable",]$X_1 = 1 - currentParameters$sensitivity
      LungRadsScreenRegularDist[LungRadsScreenRegularDist$Cancer_detectable_at_screen == "Cancer_detectable",]$X_6 = currentParameters$sensitivity

      scenario$set_param("LungRadsScreenRegularDist",LungRadsScreenRegularDist)


    

      LcDispatcherDefault = scenario$get_param("LcDispatcherDefault") #Screening dispatcher

      #Only need one column. Blank out the other one
      LcDispatcherDefault$`3`[LcDispatcherDefault$Characteristics_of_program=="Recruitment_end_year_integer"] = 2025
      LcDispatcherDefault$`3`[LcDispatcherDefault$Characteristics_of_program=="LUNG_SCREENING_x_OCOL_VISIBILITY"] = 1

      LcDispatcherDefault$`2`[LcDispatcherDefault$Characteristics_of_program=="LUNG_SCREENING_x_OCOL_VISIBILITY"] = 1
      LcDispatcherDefault$`2`[LcDispatcherDefault$Characteristics_of_program=="LUNG_SCREENING_x_MIN_RECRUIT_AGE"] = currentParameters$startAge
      LcDispatcherDefault$`2`[LcDispatcherDefault$Characteristics_of_program=="LUNG_SCREENING_x_MAX_SCREEN_AGE"] = currentParameters$endAge
      LcDispatcherDefault$`2`[LcDispatcherDefault$Characteristics_of_program=="LUNG_SCREENING_x_MAX_RECRUIT_AGE"] = currentParameters$endAge
      LcDispatcherDefault$`2`[LcDispatcherDefault$Characteristics_of_program=="Recruitment_start_year_integer"] = 2025
      LcDispatcherDefault$`2`[LcDispatcherDefault$Characteristics_of_program=="LUNG_SCREENING_x_N_PARTICIPATION"] = currentParameters$actualParticipation
      LcDispatcherDefault$`2`[LcDispatcherDefault$Characteristics_of_program=="Rescreen_rate"] = currentParameters$actualAdherence

      LcDispatcherDefault$`2`[LcDispatcherDefault$Characteristics_of_program %in% c("LUNG_SCREENING_x_ST_SEQ_HIGHRISK","LUNG_SCREENING_x_ND_SEQ_HIGHRISK","LUNG_SCREENING_x_EQ_VERYHIGHRISK","LUNG_SCREENING_x_EQ_VERYHIEnum22")] = currentParameters$frequency
      scenario$set_param("LcDispatcherDefault",LcDispatcherDefault)

    }
  }

  return(currentParameters)

}