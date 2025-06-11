paramsToAlter <- list()
paramsToAlter["OncoSim-breast"] <- list(c("BreastScreeningProtocolDispatcher","BreastScreeningSpecificityNew","BreastScreeningSensitivity","BreastScreeningCosts")) # Hack alternate modality E
paramsToAlter["OncoSim-colorectal"] <- list(c("CrcScreenRecruitCanada","CrcScreenPlanCanada","CrcTestPositivitySuccinct","CrcScreenCostCanada"))#Planning to hack FOBT to use as MCED
paramsToAlter["OncoSim-cervical"] <- list(c("HpvScreeningProtocolDispatcher","HpvScreeningPreCancerTreatmentCosts","CytologyResultCarcino","FollowUpCytologyProtocol1")) #Planning to hack HPV DNA testing to use as MCED
paramsToAlter["OncoSim-lung"] <- list(c("LungCancerScreenFollowupProtcol", "LcDispatcherDefault","LungRadsScreenRegularDist","LcScreenCost")) #Efficacy may not work for this parameter?

outputTables <- list()

outputTables["OncoSim-colorectal"] <- list(c("Colorectal_Cancer_Cases_Table","Colorectal_Cancer_Screen_Table","Colorectal_Cancer_ScreenState_Outcomes_Table"))
outputTables["OncoSim-breast"] <- list(c("Breast_Cancer_Cases_Table","Breast_Cancer_Screen_Outcome_Modality_Table"))
outputTables["OncoSim-cervical"] <- list(c("Cervical_Cancer_Cases_Table","Cervical_Cancer_Timing_Of_Last_Screen","Cervical_Cancer_Costs_Table","Cervical_Cancer_ICER_Table","Hpv_Screening_Costs_Prov_Table","HPV_Colposcopy_Results_Table","Hpv_Screening_Costs_Age_Table","HPV_DNA_Results_Table"))
outputTables["OncoSim-lung"] <- list(c("Lung_Cancer_Cases_Table","Lung_Cancer_ScreenCases_Year_Table","Lung_Cancer_Costs_Table","Lung_Cancer_ICER_Table"))

generateModelParameters <- function(modelsToUse){
    participationMultipliers = list()
    participationMultipliers["OncoSim-breast"] = 1
    participationMultipliers["OncoSim-colorectal"] = 1
    participationMultipliers["OncoSim-cervical"] = 1
    participationMultipliers["OncoSim-lung"] = 1

    adherenceMultipliers = list()
    adherenceMultipliers["OncoSim-breast"] = 1
    adherenceMultipliers["OncoSim-colorectal"] = 1
    adherenceMultipliers["OncoSim-cervical"] = 1
    adherenceMultipliers["OncoSim-lung"] = 1


    testCosts = list()

    #https://www.galleri.com/patient/the-galleri-test/cost 
    # $949 USD = 1,298.21
    testCosts["Galleri"] = 1298.21

    #https://pmc.ncbi.nlm.nih.gov/articles/PMC6080308/
    # "We estimate the cost of the test to be less than $500," 
    # $500 USD = $684.04
    testCosts["CancerSeek"] = 684.04


    #Sensitivity THEN specificity
    testPerformance = list()

    #https://www.ncbi.nlm.nih.gov/books/NBK611745/
    #https://www.annalsofoncology.org/article/S0923-7534(21)02046-9/fulltext CCGA3
    # Using site-specific sensitivity and overall specificity
    testPerformance["OncoSim-colorectal-Galleri"] =  list(c(0.82,0.995))
    testPerformance["OncoSim-breast-Galleri"] =  list(c(0.305,0.995))
    testPerformance["OncoSim-cervical-Galleri"] =  list(c(0.80,0.995))
    testPerformance["OncoSim-lung-Galleri"] =  list(c(0.748,0.995))

    #https://www.ncbi.nlm.nih.gov/books/NBK611745/
    #https://pmc.ncbi.nlm.nih.gov/articles/PMC6080308/
    # The specificity of CancerSEEK was greater than 99%
    testPerformance["OncoSim-colorectal-CancerSeek"] =  list(c(0.649,0.99))
    testPerformance["OncoSim-breast-CancerSeek"] =  list(c(0.335,0.99))
    testPerformance["OncoSim-lung-CancerSeek"] =  list(c(0.587,0.99))
    # WAS NOT AVAILABLE USING OVERALL FOR CERVICAL
    testPerformance["OncoSim-cervical-CancerSeek"] =  list(c(0.623,0.99))



    parameterTables = data.frame()

    scenarios <- c()





    for(scenarioType in c("Galleri","CancerSeek")){
      for(startAge in c(45,50)[1]){
        for(testFrequency in c(3,5)[1]){
          for(participation in c(90)){#(3:9*10)
            for(cancerType in modelsToUse){

              scenarioName = paste(cancerType,startAge,testFrequency,participation,scenarioType,sep="_")
              currentTable = data.frame(name=scenarioName)

              currentTable$type = scenarioType
              currentTable$model = cancerType
              currentTable$startAge = startAge
              currentTable$endAge = 74
              currentTable$frequency = testFrequency

              currentTable$participation = participation/100

              currentTable$actualParticipation = currentTable$participation*participationMultipliers[cancerType][[1]]
              currentTable$actualAdherence = currentTable$participation*adherenceMultipliers[cancerType][[1]]

              currentTestParameters = testPerformance[paste(cancerType,scenarioType,sep="-")][[1]]
              currentTable$sensitivity = currentTestParameters[1]
              currentTable$specificity = currentTestParameters[2]

              currentTable$cost = testCosts[scenarioType][[1]]


              parameterTables = bind_rows(parameterTables,currentTable)
            }
          }
        }
      }
    }

    for(scenarioType in c("BaseCase")){
      for(cancerType in modelsToUse){

        scenarioName = paste(cancerType,scenarioType,sep="_")
        currentTable = data.frame(name=scenarioName)

        currentTable$model = cancerType
        currentTable$type = scenarioType

        parameterTables = bind_rows(parameterTables,currentTable)
      }
    }
    
    parameterTables
}