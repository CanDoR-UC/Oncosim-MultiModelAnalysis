SetParameters <- function(createdScenarios) {
	scenario <- createdScenarios$scenario[[1]]
	scenarioName <- createdScenarios$name
	currentParameters <- createdScenarios

	scenario$ReadOnly <- FALSE

	if (currentParameters$model == "OncoSim-colorectal") {
		modality_param <- scenario$get_param("CrcScreenPlanCanada")
		modality_param$A[
			modality_param$Screening_modality_plan == 'Adherence_probability'
		] <- 1
		scenario$set_param("CrcScreenPlanCanada", modality_param)

		baseCaseParticipationValue <- 0.5
		newParticipationValue <- ifelse(currentParameters$type != "BaseCase", currentParameters$participation, baseCaseParticipationValue)

		screen_param <- scenario$get_param("CrcScreenRecruitCanada")

		screen_param$`0` <- c(0, 1, 2009, 2009, 50, 74, 123, baseCaseParticipationValue, 2, 0, 99, newParticipationValue, 4)
		screen_param$`1` <- 0
		screen_param$`2` <- 0

		rectum_costs <- scenario$get_param("RectalCancerTreatmentCosts")
		colon_costs <- scenario$get_param("ColonCancerTreatmentCosts")

		rectum_costs[4:11] <- rectum_costs[4:11] * treatmentCostMultiplier["OncoSim-colorectal"][[1]]
		colon_costs[4:11] <- colon_costs[4:11] * treatmentCostMultiplier["OncoSim-colorectal"][[1]]

		scenario$set_param("RectalCancerTreatmentCosts", rectum_costs)
		scenario$set_param("ColonCancerTreatmentCosts", colon_costs)

		#Add extra recruitment to allow a sharp increase (if needed)
		if (newParticipationValue > baseCaseParticipationValue) {
			yearToSelect <- 2010
			ageToSelect <- 59
			yearsBetween <- 14
			for (column in 3:17) {
				screen_param[[paste(column)]] <- c(0, 1, yearToSelect, yearToSelect, 50, ageToSelect, 123, baseCaseParticipationValue, 1, 1, yearsBetween, newParticipationValue, 4)
				yearToSelect <- yearToSelect + 1
				ageToSelect <- ageToSelect + 1
				yearsBetween <- yearsBetween - 1
			}
		} else {
			screen_param$`3` <- 0
			screen_param$`4` <- 0
		}
		screen_param$`18` <- c(0, 1, 2010, 2024, 50, 74, 123, baseCaseParticipationValue, 1, 0, 99, newParticipationValue, 4)
		screen_param$`19` <- c(0, 1, 2025, 9999, ifelse(currentParameters$type != "BaseCase", currentParameters$startAge, 50), 74, 123, newParticipationValue, 0, 0, 99, newParticipationValue, ifelse(currentParameters$type != "BaseCase" & currentParameters$type != "Non-MC", 5, 4))

		scenario$set_param("CrcScreenRecruitCanada", screen_param)

		#withinModality <- scenario$get_param("ScreeningRoundWithinModality")
		#withinModality[[1]] = FALSE
		#scenario$set_param("ScreeningRoundWithinModality", withinModality)

		if (currentParameters$type != "BaseCase") {
			timeToReturn <- scenario$get_param("TimeToRegularScreeningAfterColonoscopy")
			timeToReturn[[1]] <- 0
			scenario$set_param("TimeToRegularScreeningAfterColonoscopy", timeToReturn)

			if (currentParameters$type != "Non-MC") {
				#Set MCED Cost
				costs <- scenario$get_param("CrcScreenCostCanada")
				costs$FOBT_KIT <- currentParameters$cost
				costs$FOBT_PROCESSING <- 0
				scenario$set_param("CrcScreenCostCanada", costs)
			}

			#Creating plan to dictate how people are screened with MCED
			modality_param <- scenario$get_param("CrcScreenPlanCanada")
			modality_param$B <- modality_param$A

			#Include people who were already recruited
			modality_param$B[
				modality_param$Screening_modality_plan == 'Screening_group'
			] <- 45
			modality_param$A[
				modality_param$Screening_modality_plan == 'Screening_group'
			] <- 45

			modality_param$B[
				modality_param$Screening_modality_plan == 'Adherence_probability'
			] <- 1

			modality_param$B[
				modality_param$Screening_modality_plan == 'SP_FREQUENCY'
			] <- currentParameters$frequency

			modality_param$B[
				modality_param$Screening_modality_plan == 'Age_start_in_ticks'
			] <- currentParameters$startAge

			modality_param$B[
				modality_param$Screening_modality_plan == 'Modality'
			] <- ifelse(currentParameters$type != "Non-MC", 1, 2) #For MCED scenarios, hack the FOBT modality. Otherwise, leave as 2

			#Enable MCED starting in 2025
			modality_param$B[
				modality_param$Screening_modality_plan == 'Time_start_continuous_time'
			] <- 2025

			modality_param$A[
				modality_param$Screening_modality_plan == 'Time_end_continuous_time'
			] <- 2025

			scenario$set_param("CrcScreenPlanCanada", modality_param)

			if (currentParameters$type != "Non-MC") {
				#Altering effectiveness to emulate MCED
				fit_param <- scenario$get_param("CrcTestPositivitySuccinct")
				# 1-Specificity for "No polyp or cancer" and all others. If a polyp is detected, this is a false positive. Sensitivity for cancer
				otherValues <- as.numeric(scales::number(
					1 - currentParameters$specificity,
					accuracy = 0.001
				))

				effectivenessValues <- c(
					otherValues,
					otherValues,
					otherValues,
					otherValues,
					currentParameters$sensitivity
				)

				fit_param$FOBT_guaiac[1:5] <- effectivenessValues
				fit_param$FOBT_guaiac[11:15] <- effectivenessValues

				scenario$set_param("CrcTestPositivitySuccinct", fit_param)
			}
		} else {
			#Set base parameters here.
		}
	}
	if (currentParameters$model == "OncoSim-breast") {
		#Only need the original dispatcher
		BreastScreeningUseLeanDispatcher <- scenario$get_param(
			"BreastScreeningUseLeanDispatcher"
		)
		BreastScreeningUseLeanDispatcher$Future_screening_era <- FALSE
		BreastScreeningUseLeanDispatcher$Past_screening_era <- FALSE

		scenario$set_param(
			"BreastScreeningUseLeanDispatcher",
			BreastScreeningUseLeanDispatcher
		)

		cost_1 <- scenario$get_param("BreastCancerCosts")
		cost_2 <- scenario$get_param("BreastCancerCosts2")

		cost_1$Cost <- cost_1$Cost * treatmentCostMultiplier["OncoSim-breast"][[1]]
		cost_2$Cost <- cost_2$Cost * treatmentCostMultiplier["OncoSim-breast"][[1]]

		scenario$set_param("BreastCancerCosts", cost_1)
		scenario$set_param("BreastCancerCosts2", cost_2)

		if (currentParameters$type != "BaseCase") {
			if (currentParameters$type != "Non-MC") {
				costs <- scenario$get_param("BreastScreeningCosts")
				costs[
					costs$Breast_screening_modalities == "Alternative_modality_E",
				]$Cost_of_screen <- currentParameters$cost
				scenario$set_param("BreastScreeningCosts", costs)
			}

			#Set up parameters for how MCED screening works.
			screen_param <- scenario$get_param("BreastScreeningProtocolDispatcher")
			reference <- screen_param$`3`[
				screen_param$Screening_era == "Future_screening_era"
			]

			screen_param$`5`[
				screen_param$Screening_era == "Future_screening_era"
			] <- reference

			futureScreeningEra <- screen_param$Screening_era == "Future_screening_era"

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'Visibility_prob_visible'
			] <- 1

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS %in% c('Minimum_recruit_age_integer', "Minimum_re_screen_age_integer")
			] <- currentParameters$startAge

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS %in% c('Maximum_recruit_age_integer', "Maximum_re_screen_age_integer")
			] <- currentParameters$endAge

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'Recruitment_rate'
			] <- currentParameters$participation

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'Rescreen_rate'
			] <- currentParameters$participation

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'Protocol_to_be_used'
			] <- 1

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS %in% c("BREAST_SCREENI_x_ATTEMPTS_GLOBAL", "BREAST_SCREENI_x_ATTEMPTS_LOCAL")
			] <- 99

			screen_param$`3`[
				screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'BREAST_SCREENI_x_X_RESCREEN_TIME'
			] <- 2025

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'BREAST_SCREENI_x_RTING_APPARATUS'
			] <- ifelse(currentParameters$type != "Non-MC", 8, 0) #Alt modality E if MCED. Default otherwise

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'BREAST_SCREENI_x_L_MIN_FREQUENCY'
			] <- currentParameters$frequency

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'BREAST_SCREENI_x_L_MAX_FREQUENCY'
			] <- currentParameters$frequency

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS %in% c('BREAST_SCREENI_x_IN_RECRUIT_TIME', "BREAST_SCREENI_x_N_RESCREEN_TIME")
			] <- 2025

			#Disable previous
			screen_param$`2`[
				screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'BREAST_SCREENI_x_AX_RECRUIT_TIME'
			] <- 2025

			screen_param$`3`[
				screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'BREAST_SCREENI_x_AX_RECRUIT_TIME'
			] <- 2025

			screen_param$`2`[
				screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'BREAST_SCREENI_x_X_RESCREEN_TIME'
			] <- 2025

			screen_param$`3`[
				screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'BREAST_SCREENI_x_X_RESCREEN_TIME'
			] <- 2025

			scenario$set_param("BreastScreeningProtocolDispatcher", screen_param)

			if (currentParameters$type != "Non-MC") {
				specificity_param <- scenario$get_param("BreastScreeningSpecificityNew")
				specificity <- currentParameters$specificity
				specificity_param[
					specificity_param$Breast_screening_modalities == "Alternative_modality_E",
				]$`(-∞,40)` <- specificity

				specificity_param[
					specificity_param$Breast_screening_modalities == "Alternative_modality_E",
				]$`[40,50)` <- specificity

				specificity_param[
					specificity_param$Breast_screening_modalities == "Alternative_modality_E",
				]$`[50,60)` <- specificity

				specificity_param[
					specificity_param$Breast_screening_modalities == "Alternative_modality_E",
				]$`[60,70)` <- specificity

				specificity_param[
					specificity_param$Breast_screening_modalities == "Alternative_modality_E",
				]$`[70,∞)` <- specificity

				scenario$set_param("BreastScreeningSpecificityNew", specificity_param)

				sensitivity_param <- scenario$get_param("BreastScreeningSensitivity")
				sensitivity <- currentParameters$sensitivity
				for (column in c(
					"Tumor_size_data_point_1",
					"Tumor_size_data_point_2",
					"Tumor_size_data_point_3",
					"Tumor_size_data_point_4",
					"Tumor_size_data_point_5",
					"Tumor_size_data_point_6",
					"Tumor_size_data_point_7",
					"Tumor_size_data_point_8",
					"Tumor_size_data_point_9",
					"Tumor_size_data_point_10"
				)) {
					sensitivity_param[
						sensitivity_param$Breast_screening_modalities == "Alternative_modality_E",
					][[column]] <- sensitivity
				}
				scenario$set_param("BreastScreeningSensitivity", sensitivity_param)
			}
		}
	}

	if (currentParameters$model == "OncoSim-lung") {
		calibratingCosts <- FALSE # THIS IS ONLY SET TO TRUE WHEN FIGURING OUT MULTIPLIERS!!
		if (!calibratingCosts) {
			ReportYearPooled <- scenario$get_param("ReportYearPooled")
			ReportYearPooled$Start_value <- 2025
			ReportYearPooled$End_value <- 2051
			scenario$set_param("ReportYearPooled", ReportYearPooled)

			followUpProtocol <- scenario$get_param("LungCancerScreenFollowupProtcol")
			followUpProtocol[[1]] <- "Custom"
			scenario$set_param("LungCancerScreenFollowupProtcol", followUpProtocol)

			LcDispatcherDefault <- scenario$get_param("LcDispatcherDefault") #Screening dispatcher

			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_OCOL_VISIBILITY"
			] <- 1

			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_ECRUIT_ATTEMPTS"
			] <- 1
			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program == "Participation_rate_for_screening"
			] <- 0.39

			scenario$set_param("LcDispatcherDefault", LcDispatcherDefault)
		}

		costs <- scenario$get_param("LungCancerTreatmentCosts")

		stageMultiplierRefTable <- read.csv("InputParameters/LungCost_Multipliers.csv")
		stageDrugMultipliers <- stageMultiplierRefTable[stageMultiplierRefTable$Type == "Medication_Ratio", ]
		stageRadioMultipliers <- stageMultiplierRefTable[stageMultiplierRefTable$Type == "Radiation_Ratio", ]

		stageOneCost <- costs %>% filter(Stage == "Non_small_cell_stage_I")
		stageOneDrug <- stageOneCost$Drug
		stageOneRadio <- stageOneCost$Radiotherapy

		costs[costs$Stage == "Non_small_cell_stage_II", ]$Drug <- stageOneDrug * stageDrugMultipliers$II

		costs[costs$Stage == "Non_small_cell_stage_IIIA", ]$Drug <- stageOneDrug * stageDrugMultipliers$III
		costs[costs$Stage == "Non_small_cell_stage_IIIB", ]$Drug <- stageOneDrug * stageDrugMultipliers$III

		costs[costs$Stage == "Non_small_cell_stage_IV", ]$Drug <- stageOneDrug * stageDrugMultipliers$IV
		costs[costs$Stage == "Relapse_non_small_cell", ]$Drug <- stageOneDrug * stageDrugMultipliers$IV

		costs[costs$Stage == "Small_cell_limited", ]$Drug <- stageOneDrug * stageDrugMultipliers$Small_Cell_Combined
		costs[costs$Stage == "Small_cell_extensive", ]$Drug <- stageOneDrug * stageDrugMultipliers$Small_Cell_Combined
		costs[costs$Stage == "Relapse_small_cell", ]$Drug <- stageOneDrug * stageDrugMultipliers$Small_Cell_Combined

		costs[costs$Stage == "Non_small_cell_stage_II", ]$Radiotherapy <- stageOneRadio * stageRadioMultipliers$II

		costs[costs$Stage == "Non_small_cell_stage_IIIA", ]$Radiotherapy <- stageOneRadio * stageRadioMultipliers$III
		costs[costs$Stage == "Non_small_cell_stage_IIIB", ]$Radiotherapy <- stageOneRadio * stageRadioMultipliers$III

		costs[costs$Stage == "Non_small_cell_stage_IV", ]$Radiotherapy <- stageOneRadio * stageRadioMultipliers$IV
		costs[costs$Stage == "Relapse_non_small_cell", ]$Radiotherapy <- stageOneRadio * stageRadioMultipliers$IV

		costs[costs$Stage == "Small_cell_limited", ]$Radiotherapy <- stageOneRadio * stageRadioMultipliers$Small_Cell_Combined
		costs[costs$Stage == "Small_cell_extensive", ]$Radiotherapy <- stageOneRadio * stageRadioMultipliers$Small_Cell_Combined
		costs[costs$Stage == "Relapse_small_cell", ]$Radiotherapy <- stageOneRadio * stageRadioMultipliers$Small_Cell_Combined

		costs[5:12] <- costs[5:12] * treatmentCostMultiplier["OncoSim-lung"][[1]]

		costs[grepl("Surveillance", costs$Treatment_state), ]$Diagnostics <- 21633.785 # May disable

		scenario$set_param("LungCancerTreatmentCosts", costs)

		if (currentParameters$type != "BaseCase" && !calibratingCosts) {
			if (currentParameters$type != "Non-MC") {
				SwitchEligibilityRule <- scenario$get_param("SwitchEligibilityRule")
				SwitchEligibilityRule$Radon_RR <- TRUE #Hack to get around eligibility rules and get high participation
				scenario$set_param("SwitchEligibilityRule", SwitchEligibilityRule)

				LcScreenEligibilityRiskThreshold <- scenario$get_param("LcScreenEligibilityRiskThreshold") # Make sure anyone can be screened
				LcScreenEligibilityRiskThreshold$High_risk_threshold <- 0
				LcScreenEligibilityRiskThreshold$Very_high_risk_threshold <- 0
				LcScreenEligibilityRiskThreshold$Prediction_horizon_years <- 99

				scenario$set_param("LcScreenEligibilityRiskThreshold", LcScreenEligibilityRiskThreshold)
				LcScreenCost <- scenario$get_param("LcScreenCost") #Set screen as MCED cost. No interpretation cost
				LcScreenCost$LCS_LDCT_SCAN <- currentParameters$cost
				LcScreenCost$LCS_LDCT_SCAN_PROCESSING <- 0
				scenario$set_param("LcScreenCost", LcScreenCost)

				#Cancer detectable - 4(Highest risk) = True positive ~ Sensitivity
				# Cancer detectable - 1(Lowest risk) = False negative ~ 1 - Sensitivity
				# No cancer detectable - 4(Highest risk) = False positive ~ 1 - Specificity
				# No cancer detectable - 1(Lowest risk) = True negative ~ Specificity
				# 1 means return to regular screening. 4 means do a follow-up scan

				LungRadsScreenRegularDist <- scenario$get_param(
					"LungRadsScreenRegularDist"
				)

				LungRadsScreenRegularDist[4:9] <- 0
				LungRadsScreenRegularDist[
					LungRadsScreenRegularDist$Cancer_detectable_at_screen == "No_cancer_detectable",
				]$X_1 <- currentParameters$specificity
				LungRadsScreenRegularDist[
					LungRadsScreenRegularDist$Cancer_detectable_at_screen == "No_cancer_detectable",
				]$X_4 <- 1 - currentParameters$specificity

				LungRadsScreenRegularDist[
					LungRadsScreenRegularDist$Cancer_detectable_at_screen == "Cancer_detectable",
				]$X_1 <- 1 - currentParameters$sensitivity
				LungRadsScreenRegularDist[
					LungRadsScreenRegularDist$Cancer_detectable_at_screen == "Cancer_detectable",
				]$X_4 <- currentParameters$sensitivity

				scenario$set_param("LungRadsScreenRegularDist", LungRadsScreenRegularDist)

				#Set follow-up time to 0 for 4
				LungRadsScreenRegularTimeToNext <- scenario$get_param(
					"LungRadsScreenRegularTimeToNext"
				)
				LungRadsScreenRegularTimeToNext$X_4 <- 0
				scenario$set_param("LungRadsScreenRegularTimeToNext", LungRadsScreenRegularTimeToNext)

				#Return to regular screening after screen
				LungRadsScreenFollowupTimeToNextScanAfterFP <- scenario$get_param(
					"LungRadsScreenFollowupTimeToNextScanAfterFP"
				)
				LungRadsScreenFollowupTimeToNextScanAfterFP[[1]] <- currentParameters$frequency
				scenario$set_param("LungRadsScreenFollowupTimeToNextScanAfterFP", LungRadsScreenFollowupTimeToNextScanAfterFP)
			}

			LcDispatcherDefault <- scenario$get_param("LcDispatcherDefault") #Screening dispatcher

			LcDispatcherDefault$`1` <- LcDispatcherDefault$`0`

			LcDispatcherDefault$`1`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_OCOL_VISIBILITY"
			] <- 1
			LcDispatcherDefault$`1`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_MIN_RECRUIT_AGE"
			] <- currentParameters$startAge
			LcDispatcherDefault$`1`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_MAX_SCREEN_AGE"
			] <- currentParameters$endAge
			LcDispatcherDefault$`1`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_MAX_RECRUIT_AGE"
			] <- currentParameters$endAge
			LcDispatcherDefault$`1`[
				LcDispatcherDefault$Characteristics_of_program == "Recruitment_start_year_integer"
			] <- 2025
			LcDispatcherDefault$`1`[
				LcDispatcherDefault$Characteristics_of_program == "Recruitment_end_year_integer"
			] <- 9999
			LcDispatcherDefault$`1`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_END_SCREEN_YEAR"
			] <- 9999
			LcDispatcherDefault$`1`[
				LcDispatcherDefault$Characteristics_of_program == "Participation_rate_for_screening"
			] <- currentParameters$participation
			LcDispatcherDefault$`1`[
				LcDispatcherDefault$Characteristics_of_program == "Rescreen_rate"
			] <- 1
			LcDispatcherDefault$`1`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_ECRUIT_ATTEMPTS"
			] <- 1
			LcDispatcherDefault$`1`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_OL_RECRUIT_FREQ"
			] <- 0

			LcDispatcherDefault$`1`[
				LcDispatcherDefault$Characteristics_of_program %in%
					c(
						"LUNG_SCREENING_x_ST_SEQ_HIGHRISK",
						"LUNG_SCREENING_x_ND_SEQ_HIGHRISK",
						"LUNG_SCREENING_x_EQ_VERYHIGHRISK",
						"LUNG_SCREENING_x_EQ_VERYHIEnum22"
					)
			] <- currentParameters$frequency
			LcDispatcherDefault$`1`[
				LcDispatcherDefault$Characteristics_of_program %in%
					c(
						"LUNG_SCREENING_x_ST_SEQ_HIEnum17",
						"LUNG_SCREENING_x_ND_SEQ_HIEnum19",
						"LUNG_SCREENING_x_EQ_VERYHIEnum21",
						"LUNG_SCREENING_x_EQ_VERYHIEnum23"
					)
			] <- 99
			LcDispatcherDefault$`1`[
				LcDispatcherDefault$Characteristics_of_program %in%
					c(
						"LUNG_SCREENING_x_OTHER_PHASE_IN",
						"LUNG_SCREENING_x_FIRST_PHASE_IN"
					)
			] <- 1
			LcDispatcherDefault$`1`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_ECRUIT_ATTEMPTS"
			] <- 1

			#End previous screening
			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program == "Recruitment_end_year_integer"
			] <- 2025
			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_END_SCREEN_YEAR"
			] <- 2025

			LcDispatcherDefault$`2`[
				LcDispatcherDefault$Characteristics_of_program == "Recruitment_end_year_integer"
			] <- 2025
			LcDispatcherDefault$`2`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_END_SCREEN_YEAR"
			] <- 2025
			LcDispatcherDefault$`3`[
				LcDispatcherDefault$Characteristics_of_program == "Recruitment_end_year_integer"
			] <- 2025
			LcDispatcherDefault$`3`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_END_SCREEN_YEAR"
			] <- 2025
			LcDispatcherDefault$`4`[
				LcDispatcherDefault$Characteristics_of_program == "Recruitment_end_year_integer"
			] <- 2025
			LcDispatcherDefault$`4`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_END_SCREEN_YEAR"
			] <- 2025

			first <- LcDispatcherDefault$`0`
			second <- LcDispatcherDefault$`1`

			LcDispatcherDefault$`0` <- second
			LcDispatcherDefault$`1` <- first

			LcDispatcherDefault$`1`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_OCOL_VISIBILITY"
			] <- 0 #Disable previous screening

			scenario$set_param("LcDispatcherDefault", LcDispatcherDefault)
		}
	}

	return(currentParameters)
}
