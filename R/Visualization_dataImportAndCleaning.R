library(tidyverse)
library(ggplot2)



dataDirectory = "Output/Runs"


getCombinedTable <- function(tableName,modelType){

    if(!grepl(".csv",tableName)){
        tableName = paste(tableName,".csv",sep="")
    }

    allScenarioNames = list.dirs(dataDirectory)
    allScenarioNames = allScenarioNames[allScenarioNames != dataDirectory]

    totalTable = data.frame()
    for(scenario in allScenarioNames){
        if(grepl(modelType,scenario)){
        currentTable = read.csv(file.path(scenario,tableName))
        name = strsplit(scenario,paste("OncoSim-",modelType,"_",sep=""))[[1]][2]
        currentTable$sce = name
        currentTable$cancertype = modelType
        currentTable$formattedName = name
            
        if(!grepl("BaseCase",name)){
            nameSplit <- strsplit(name,"_")[[1]]
            currentTable$minAge <- nameSplit[1]
            currentTable$frequency <- nameSplit[2]
            currentTable$participation <- nameSplit[3]
            currentTable$testType <- nameSplit[4]
            currentTable$formattedName = paste(currentTable$testType,"\n PPT: ",currentTable$participation,"%\n","MinAge: ",currentTable$minAge,"\nFreq: ",currentTable$frequency," Years",sep="")
        }
        totalTable = bind_rows(totalTable,currentTable)
        }
    }
    return(totalTable)
}


combineModelTables <- function(importedData){

    combinedTables <- list()

    #Detected cancers

    detectedBreast = importedData$Breast_Cancer_Cases_Table %>% filter(expr_name=="Breast_cancer_detected_by_screen",Hackable_province=="all") %>% mutate(expr_name="ScreenDetections") %>% select(-Hackable_province)
    detectedColorectal = importedData$Colorectal_Cancer_Screen_Table %>% filter(expr_name=="Screen_detected_cancers",Hackable_province=="all",Shiftable_year != "all") %>% mutate(expr_name="ScreenDetections",Shiftable_year=as.numeric(Shiftable_year))%>% select(-Hackable_province)
    detectedCervical = importedData$Cervical_Cancer_Cases_Table %>% filter(expr_name=="Cervical_cance_x_ected_by_screen",Hackable_province=="all") %>% mutate(expr_name="ScreenDetections")%>% select(-Hackable_province)
    detectedLung = importedData$Lung_Cancer_ScreenCases_Year_Table %>% filter(expr_name=="Screen_detecte_x_over_detected",Hackable_province=="all",Year != "all") %>% mutate(expr_name="ScreenDetections",,Shiftable_year=as.numeric(Year))%>% select(-c(Hackable_province,Year))

    allDetectedCancers = bind_rows(detectedBreast,detectedCervical,detectedColorectal,detectedLung)
    combinedTables["DetectedCancers"] = list(allDetectedCancers)

    #Cancer deaths

    deathsBreast = importedData$Breast_Cancer_Cases_Table %>% filter(expr_name=="Deaths_cause_specific",Hackable_province=="all") %>% mutate(expr_name="Deaths") %>% select(-Hackable_province)
    deathsColorectal = importedData$Colorectal_Cancer_Cases_Table %>% filter(expr_name=="Deaths_cause_specific",Province=="all",Sex=="all",Colorectal_cancer_site=="all") %>% mutate(expr_name="Deaths",Shiftable_year=as.numeric(Shiftable_year))%>% select(-c(Colorectal_cancer_site,Province,Sex))
    deathsCervical = importedData$Cervical_Cancer_Cases_Table %>% filter(expr_name=="Deaths_cause_specific",Hackable_province=="all") %>% mutate(expr_name="Deaths")%>% select(-Hackable_province)
    deathsLung = importedData$Lung_Cancer_ScreenCases_Year_Table %>% filter(expr_name=="Lung_cancer_deaths",Hackable_province=="all",Year != "all") %>% mutate(expr_name="Deaths",,Shiftable_year=as.numeric(Year))%>% select(-c(Hackable_province,Year))

    allDeathsCancers = bind_rows(detectedBreast,detectedCervical,detectedColorectal,detectedLung)
    combinedTables["CancerDeaths"] = list(allDeathsCancers)

    #Need to figure out a way to isolate 


    combinedTables
}