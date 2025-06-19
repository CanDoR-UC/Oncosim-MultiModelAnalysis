SetParameters <- function(createdScenarios) {
	scenario <- createdScenarios$scenario[[1]]
	scenarioName = createdScenarios$name
	currentParameters = createdScenarios

	scenario$ReadOnly = FALSE

	if (currentParameters$model == "OncoSim-colorectal") {
		if (currentParameters$type != "BaseCase") {
			#Set MCED Cost
			costs <- scenario$get_param("CrcScreenCostCanada")
			costs$FOBT_KIT <- currentParameters$cost
			costs$FOBT_PROCESSING <- 0
			scenario$set_param("CrcScreenCostCanada", costs)

			#Set up parameters for how MCED screening works.
			screen_param <- scenario$get_param("CrcScreenRecruitCanada")
			screen_param$`5` <- screen_param$`4` #first copy over element 4 to element 5 to make subsequent change easier
			screen_param$`5`[
				screen_param$Overall_program_controls == 'Recruitment_start_year_integer'
			] <- 2025

			screen_param$`5`[
				screen_param$Overall_program_controls == 'Recruitment_start_age_integer'
			] <- currentParameters$startAge
			screen_param$`5`[
				screen_param$Overall_program_controls == 'Recruitment_end_year_integer'
			] <- 9999

			screen_param$`5`[
				screen_param$Overall_program_controls == 'Recruitment_end_age_integer'
			] <- currentParameters$endAge

			screen_param$`5`[
				screen_param$Overall_program_controls == 'SR_FIRST_PARTICIPATION'
			] <- currentParameters$actualParticipation

			screen_param$`5`[
				screen_param$Overall_program_controls == 'SR_FIRST_PHASEIN'
			] <- 2

			screen_param$`5`[
				screen_param$Overall_program_controls == 'Assign_screening_group'
			] <- 5

			#DISABLE #4 TO ENSURE NO FIT TESTS ARE PERFORMED ANYMORE
			screen_param$`4`[
				screen_param$Overall_program_controls == 'Recruitment_end_year_integer'
			] <- 2025

			scenario$set_param("CrcScreenRecruitCanada", screen_param)

			#Creating plan to dictate how people are screened with MCED
			modality_param <- scenario$get_param("CrcScreenPlanCanada")
			modality_param$B <- modality_param$A

			modality_param$B[
				modality_param$Screening_modality_plan == 'Screening_group'
			] <- 5

			modality_param$B[
				modality_param$Screening_modality_plan == 'Adherence_probability'
			] <- currentParameters$actualParticipation

			modality_param$B[
				modality_param$Screening_modality_plan == 'SP_FREQUENCY'
			] <- currentParameters$frequency

			modality_param$B[
				modality_param$Screening_modality_plan == 'Age_start_in_ticks'
			] <- currentParameters$startAge

			modality_param$B[
				modality_param$Screening_modality_plan == 'Modality'
			] <- 1 #For MCED scenarios, hack the FOBT modality

			modality_param$A[
				modality_param$Screening_modality_plan == 'Time_end_continuous_time'
			] <- 2026

			scenario$set_param("CrcScreenPlanCanada", modality_param)

			#Altering effectiveness to emulate MCED
			fit_param <- scenario$get_param("CrcTestPositivitySuccinct")
			# 1-Specificity for "No polyp or cancer" and all others. If a polyp is detected, this is a false positive. Sensitivity for cancer
			otherValues = as.numeric(scales::number(
				1 - currentParameters$specificity,
				accuracy = 0.001
			))

			effectivenessValues = c(
				otherValues,
				otherValues,
				otherValues,
				otherValues,
				currentParameters$sensitivity
			)

			fit_param$FOBT_guaiac[1:5] <- effectivenessValues
			fit_param$FOBT_guaiac[11:15] <- effectivenessValues

			scenario$set_param("CrcTestPositivitySuccinct", fit_param)
		} else {
			#Set base parameters here.
		}
	}
	if (currentParameters$model == "OncoSim-breast") {
		#Only need the original dispatcher
		BreastScreeningUseLeanDispatcher = scenario$get_param(
			"BreastScreeningUseLeanDispatcher"
		)
		BreastScreeningUseLeanDispatcher$Future_screening_era = FALSE
		BreastScreeningUseLeanDispatcher$Past_screening_era = FALSE
		scenario$set_param(
			"BreastScreeningUseLeanDispatcher",
			BreastScreeningUseLeanDispatcher
		)

		if (currentParameters$type != "BaseCase") {
			#Set MCED Cost May have to change the other costs too?

			costs <- scenario$get_param("BreastScreeningCosts")
			costs[
				costs$Breast_screening_modalities == "Alternative_modality_E",
			]$Cost_of_screen <- currentParameters$cost
			scenario$set_param("BreastScreeningCosts", costs)

			#Set up parameters for how MCED screening works.
			screen_param <- scenario$get_param("BreastScreeningProtocolDispatcher")
			reference = screen_param$`3`[
				screen_param$Screening_era == "Future_screening_era"
			]
			screen_param$`5`[
				screen_param$Screening_era == "Future_screening_era"
			] <- reference

			futureScreeningEra = screen_param$Screening_era == "Future_screening_era"

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'Visibility_prob_visible'
			] <- 1

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'Minimum_recruit_age_integer'
			] <- currentParameters$startAge

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'Maximum_recruit_age_integer'
			] <- currentParameters$endAge

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'Recruitment_rate'
			] <- currentParameters$actualParticipation

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'Rescreen_rate'
			] <- currentParameters$actualAdherence

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'Protocol_to_be_used'
			] <- 1

			screen_param$`5`[
				futureScreeningEra &
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'BREAST_SCREENI_x_RTING_APPARATUS'
			] <- 8 #Alt modality E

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
					screen_param$BREAST_SCREENI_x_CHARACTERISTICS == 'BREAST_SCREENI_x_IN_RECRUIT_TIME'
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

			specificity_param <- scenario$get_param("BreastScreeningSpecificityNew")
			specificity = currentParameters$specificity
			specificity_param[
				specificity_param$Breast_screening_modalities == "Alternative_modality_E",
			]$`(-∞,40)` = specificity

			specificity_param[
				specificity_param$Breast_screening_modalities == "Alternative_modality_E",
			]$`[40,50)` = specificity

			specificity_param[
				specificity_param$Breast_screening_modalities == "Alternative_modality_E",
			]$`[50,60)` = specificity

			specificity_param[
				specificity_param$Breast_screening_modalities == "Alternative_modality_E",
			]$`[60,70)` = specificity

			specificity_param[
				specificity_param$Breast_screening_modalities == "Alternative_modality_E",
			]$`[70,∞)` = specificity

			scenario$set_param("BreastScreeningSpecificityNew", specificity_param)

			sensitivity_param <- scenario$get_param("BreastScreeningSensitivity")
			sensitivity = currentParameters$sensitivity
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
				][[column]] = sensitivity
			}
			scenario$set_param("BreastScreeningSensitivity", sensitivity_param)
		}
	}

	if (currentParameters$model == "OncoSim-cervical") {
		#Use regular (not lean) dispatcher
		HpvScreeningUseLeanDispatcher = scenario$get_param(
			"HpvScreeningUseLeanDispatcher"
		)
		HpvScreeningUseLeanDispatcher$Future_screening_era = FALSE
		HpvScreeningUseLeanDispatcher$Past_screening_era = FALSE
		scenario$set_param(
			"HpvScreeningUseLeanDispatcher",
			HpvScreeningUseLeanDispatcher
		)

		if (currentParameters$type != "BaseCase") {
			#Set pap Cost to what we need. Only the initial
			testCosts = scenario$get_param("HpvScreeningPreCancerTreatmentCosts")
			testCosts$Cytology_PAP_screen = currentParameters$cost

			scenario$set_param("HpvScreeningPreCancerTreatmentCosts", testCosts)

			dispatcher = scenario$get_param("HpvScreeningProtocolDispatcher")
			futureScreeningEra = dispatcher$Screening_era == "Future_screening_era"

			dispatcher$`1`[futureScreeningEra] = dispatcher$`2`[futureScreeningEra]

			dispatcher$`1`[
				futureScreeningEra &
					dispatcher$HPV_SCREENING_x_CHARACTERISTICS == "Visibility_prob_visible"
			] = 1

			dispatcher$`1`[
				futureScreeningEra &
					dispatcher$HPV_SCREENING_x_CHARACTERISTICS %in%
						c("Minimum_recruit_age_integer", "Minimum_re_screen_age_integer")
			] = currentParameters$startAge

			dispatcher$`1`[
				futureScreeningEra &
					dispatcher$HPV_SCREENING_x_CHARACTERISTICS == "Maximum_recruit_age_integer"
			] = currentParameters$endAge

			dispatcher$`1`[
				futureScreeningEra &
					dispatcher$HPV_SCREENING_x_CHARACTERISTICS %in%
						c(
							"HPV_SCREENING_x_IN_RECRUIT_TIME",
							"HPV_SCREENING_x_N_RESCREEN_TIME"
						)
			] = 2025

			dispatcher$`1`[
				futureScreeningEra &
					dispatcher$HPV_SCREENING_x_CHARACTERISTICS == "Recruitment_rate"
			] = currentParameters$actualParticipation

			dispatcher$`1`[
				futureScreeningEra &
					dispatcher$HPV_SCREENING_x_CHARACTERISTICS == "Rescreen_rate"
			] = currentParameters$actualAdherence

			dispatcher$`1`[
				futureScreeningEra &
					dispatcher$HPV_SCREENING_x_CHARACTERISTICS %in%
						c(
							"HPV_SCREENING_x_L_MIN_FREQUENCY",
							"HPV_SCREENING_x_L_MAX_FREQUENCY"
						)
			] = currentParameters$frequency

			dispatcher$`1`[
				futureScreeningEra &
					dispatcher$HPV_SCREENING_x_CHARACTERISTICS == "Protocol_to_be_used"
			] = 5 #Protocol 5 is overriden for MCED

			dispatcher$`1`[
				futureScreeningEra &
					dispatcher$HPV_SCREENING_x_CHARACTERISTICS == "HPV_SCREENING_x_RTING_APPARATUS"
			] = 1.1 # 1.1  1.1 : Cytology (PAP) with follow-up logic 1

			#End current screening at 2025
			dispatcher$`2`[
				futureScreeningEra &
					dispatcher$HPV_SCREENING_x_CHARACTERISTICS %in%
						c(
							"HPV_SCREENING_x_AX_RECRUIT_TIME",
							"HPV_SCREENING_x_X_RESCREEN_TIME"
						)
			] = 2025
			dispatcher$`0`[
				futureScreeningEra &
					dispatcher$HPV_SCREENING_x_CHARACTERISTICS %in%
						c(
							"HPV_SCREENING_x_X_RESCREEN_TIME" #
						)
			] = 2025
			dispatcher$`6`[
				futureScreeningEra &
					dispatcher$HPV_SCREENING_x_CHARACTERISTICS %in%
						c(
							"HPV_SCREENING_x_X_RESCREEN_TIME"
						)
			] = 2025

			scenario$set_param("HpvScreeningProtocolDispatcher", dispatcher)

			sensSpec = scenario$get_param("CytologyResultCarcino")

			sensSpecFilter = sensSpec$Sensitivity_era == "Future_sensitivity_era" &
				sensSpec$Cytology_type == "Conventional_cytology"

			sensSpec[sensSpecFilter, ][5:12] = 0

			sensSpec$All_cells_normal[
				sensSpec$Progression_status == "No_infection_and_no_lesion" &
					sensSpecFilter
			] = currentParameters$specificity * 100 #Probability of correctly saying that its negative (specificity)

			sensSpec$Has_cervical_cancer[
				sensSpec$Progression_status == "No_infection_and_no_lesion" &
					sensSpecFilter
			] = (1 - currentParameters$specificity) * 100 # 1- Probability of correctly saying that its negative (specificity)

			sensSpec$All_cells_normal[
				sensSpec$Progression_status == "Cervical_cancer" & sensSpecFilter
			] = (1 - currentParameters$sensitivity) * 100 #1- Probability of correctly sayign there is cervical cancer (sensitivity)

			sensSpec$Has_cervical_cancer[
				sensSpec$Progression_status == "Cervical_cancer" & sensSpecFilter
			] = currentParameters$sensitivity * 100 #1- Probability of correctly sayign there is cervical cancer (sensitivity)

			scenario$set_param("CytologyResultCarcino", sensSpec)

			FollowUpCytologyProtocol = scenario$get_param("FollowUpCytologyProtocol5")

			normalResultFilter = FollowUpCytologyProtocol$Outcome_of_the_test_treatment > 0 & FollowUpCytologyProtocol$Outcome_of_the_test_treatment != 8 & FollowUpCytologyProtocol$Follow_up_logics == "Follow_up_logic_1"
			#MCED only detects cancer or no cancer. Meaning that all non-cancer findings must be referred to the "all-cells normal" pipeline
			FollowUpCytologyProtocol$Apparatus_used_in_the_follow_up[normalResultFilter] = 1.1
			FollowUpCytologyProtocol$Lower_bound_for_follow_up_time[normalResultFilter] = 0
			FollowUpCytologyProtocol$Upper_bound_for_follow_up_time[normalResultFilter] = 0
			FollowUpCytologyProtocol$HPV_SCREENING_x_E_MEAN_LOCATION[normalResultFilter] = 0
			FollowUpCytologyProtocol$Negative_results_sought[normalResultFilter] = 0

			#Try to match Protocol 1
			FollowUpCytologyProtocol[
				FollowUpCytologyProtocol$Follow_up_element == 8 & FollowUpCytologyProtocol$Follow_up_logics == "Follow_up_logic_1",
			]$Apparatus_used_in_the_follow_up = 3.2

			#Use extra row for outcome 2 for 8. (If theres a positive test, MCED should continue unaltered)
			FollowUpCytologyProtocol[
				FollowUpCytologyProtocol$Follow_up_element == 2 & FollowUpCytologyProtocol$Follow_up_logics == "Follow_up_logic_1",
			]$Probability_of_this_follow_up = 1
			FollowUpCytologyProtocol[
				FollowUpCytologyProtocol$Follow_up_element == 2 & FollowUpCytologyProtocol$Follow_up_logics == "Follow_up_logic_1",
			]$Outcome_of_the_test_treatment = 8

			FollowUpCytologyProtocol[
				FollowUpCytologyProtocol$Follow_up_element == 1 & FollowUpCytologyProtocol$Follow_up_logics == "Follow_up_logic_1",
			]$Probability_of_this_follow_up = 1

			scenario$set_param("FollowUpCytologyProtocol5", FollowUpCytologyProtocol)
		}
	}

	if (currentParameters$model == "OncoSim-lung") {
		#Need to alter the follow-up prototocol
		followUpProtocol = scenario$get_param("LungCancerScreenFollowupProtcol")
		followUpProtocol[[1]] = "Custom"
		scenario$set_param("LungCancerScreenFollowupProtcol", followUpProtocol)

		LcDispatcherDefault = scenario$get_param("LcDispatcherDefault") #Screening dispatcher

		LcDispatcherDefault$`0`[
			LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_ECRUIT_ATTEMPTS"
		] = 1
		LcDispatcherDefault$`0`[
			LcDispatcherDefault$Characteristics_of_program == "Participation_rate_for_screening"
		] = 0.05
		LcDispatcherDefault$`0`[
			LcDispatcherDefault$Characteristics_of_program == "Recruitment_start_year_integer"
		] = 2000
		LcDispatcherDefault$`0`[
			LcDispatcherDefault$Characteristics_of_program == "Recruitment_end_year_integer"
		] = 2025
		LcDispatcherDefault$`0`[
			LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_END_SCREEN_YEAR"
		] = 2025

		scenario$set_param("LcDispatcherDefault", LcDispatcherDefault)

		if (currentParameters$type != "BaseCase") {
			LcScreenCost = scenario$get_param("LcScreenCost") #Set screen as MCED cost. No interpretation cost
			LcScreenCost$LCS_LDCT_SCAN = currentParameters$cost
			LcScreenCost$LCS_LDCT_SCAN_PROCESSING = 0
			scenario$set_param("LcScreenCost", LcScreenCost)

			#Cancer detectable - 6(Highest risk) = True positive ~ Sensitivity
			# Cancer detectable - 1(Lowest risk) = False negative ~ 1 - Sensitivity
			# No cancer detectable - 6(Highest risk) = False positive ~ 1 - Specificity
			# No cancer detectable - 1(Lowest risk) = True negative ~ Specificity

			LungRadsScreenRegularDist = scenario$get_param(
				"LungRadsScreenRegularDist"
			)

			LungRadsScreenRegularDist[4:9] = 0
			LungRadsScreenRegularDist[
				LungRadsScreenRegularDist$Cancer_detectable_at_screen == "No_cancer_detectable",
			]$X_1 = currentParameters$specificity
			LungRadsScreenRegularDist[
				LungRadsScreenRegularDist$Cancer_detectable_at_screen == "No_cancer_detectable",
			]$X_6 = 1 - currentParameters$specificity

			LungRadsScreenRegularDist[
				LungRadsScreenRegularDist$Cancer_detectable_at_screen == "Cancer_detectable",
			]$X_1 = 1 - currentParameters$sensitivity
			LungRadsScreenRegularDist[
				LungRadsScreenRegularDist$Cancer_detectable_at_screen == "Cancer_detectable",
			]$X_6 = currentParameters$sensitivity

			scenario$set_param("LungRadsScreenRegularDist", LungRadsScreenRegularDist)

			LcDispatcherDefault = scenario$get_param("LcDispatcherDefault") #Screening dispatcher

			LcDispatcherDefault$`1` = LcDispatcherDefault$`0`

			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_OCOL_VISIBILITY"
			] = 1
			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_MIN_RECRUIT_AGE"
			] = currentParameters$startAge
			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_MAX_SCREEN_AGE"
			] = currentParameters$endAge
			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_MAX_RECRUIT_AGE"
			] = currentParameters$endAge
			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program == "Recruitment_start_year_integer"
			] = 2025
			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program == "Recruitment_end_year_integer"
			] = 9999
			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_END_SCREEN_YEAR"
			] = 9999
			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program == "Participation_rate_for_screening"
			] = currentParameters$actualParticipation
			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program == "Rescreen_rate"
			] = currentParameters$actualAdherence
			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_ECRUIT_ATTEMPTS"
			] = 1

			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program == "LUNG_SCREENING_x_N_PARTICIPATION"
			] = 0

			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program %in%
					c(
						"LUNG_SCREENING_x_ST_SEQ_HIGHRISK",
						"LUNG_SCREENING_x_ND_SEQ_HIGHRISK",
						"LUNG_SCREENING_x_EQ_VERYHIGHRISK",
						"LUNG_SCREENING_x_EQ_VERYHIEnum22"
					)
			] = currentParameters$frequency
			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program %in%
					c(
						"LUNG_SCREENING_x_ST_SEQ_HIEnum17",
						"LUNG_SCREENING_x_ND_SEQ_HIEnum19",
						"LUNG_SCREENING_x_EQ_VERYHIEnum21",
						"LUNG_SCREENING_x_EQ_VERYHIEnum23"
					)
			] = 99
			LcDispatcherDefault$`0`[
				LcDispatcherDefault$Characteristics_of_program %in%
					c(
						"LUNG_SCREENING_x_OTHER_PHASE_IN",
						"LUNG_SCREENING_x_FIRST_PHASE_IN"
					)
			] = 0

			scenario$set_param("LcDispatcherDefault", LcDispatcherDefault)
		}
	}

	return(currentParameters)
}
