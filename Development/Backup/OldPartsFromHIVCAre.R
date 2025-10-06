

#===============================================================================
# PATIENT SELECTION
#===============================================================================
#   - Keep only patients with ICD codes for cancer and/or HIV
#   - Keep only patients with main documentation starting at adult age
#-------------------------------------------------------------------------------

  ADS$Patients <- ADS$Patients %>%
                      filter(PatientHoldsCancerCodes == TRUE | PatientHoldsHIVCodes == TRUE) %>%
                      filter(FirstRelevantAdmissionAge >= 18)


  # ------------ Temporary
  AuxSize <- ADS$Patients %>% ungroup() %>% summarize(Step = "After fist selection", SampleSize = n_distinct(PatientPseudonym))
  Aux.SampleSize <- rbind(Aux.SampleSize, AuxSize)



#===============================================================================
# PATIENT SELECTION ADS$Patients
#===============================================================================
#   - Among cancer coded patients, keep only ones with a plausible presumed cancer diagnosis, so with least one "main cancer code"
#-------------------------------------------------------------------------------
# Define main patient subgroups:
#     - Patients with presumed cancer only
#     - Patients with presumed HIV only
#     - Patients with presumed HIV infection and cancer
# Take group names and labeling from global variable
#-------------------------------------------------------------------------------

  ADS$Patients <- ADS$Patients %>%
                      filter((PatientHoldsCancerCodes == TRUE & PatientHoldsMainCancerDiagnosis == TRUE) |
                                (PatientHoldsCancerCodes == FALSE & PatientHoldsHIVCodes == TRUE)) %>%
                      #--- Add Subgroup variable ---------------------------------
                      mutate(PatientSubgroup = case_when(PatientHoldsCancerCodes == TRUE & PatientHoldsHIVCodes == FALSE ~ "Cancer+/HIV-",
                                                         PatientHoldsCancerCodes == TRUE & PatientHoldsHIVCodes == TRUE ~ "Cancer+/HIV+",
                                                         PatientHoldsCancerCodes == FALSE & PatientHoldsHIVCodes == TRUE ~ "Cancer-/HIV+"),
                             PatientSubgroup = factor(PatientSubgroup,
                                                      levels = vc_MainSubgroups,
                                                      labels = names(vc_MainSubgroups)),
                                  .after = PatientPseudonym)
                      #=== Update Progress Bar ===
                      try(ProgressBar$tick())

  # ------------ Temporary
  AuxSize <- ADS$Patients %>% ungroup() %>% summarize(Step = "After second filtering", SampleSize = n_distinct(PatientPseudonym))
  Aux.SampleSize <- rbind(Aux.SampleSize, AuxSize)

  # Update Attrition Tracker
  # ------------------------------------------------------------------------------
  df_Mon_AttritionTracker_AllPatients <- df_Mon_AttritionTracker_AllPatients %>%
                                              f_UpdateAttritionTracker(inp_NewSampleSize = n_distinct(df_ADM_Patients$PatientPseudonym),
                                                                       inp_InclusionComment = "Patients with plausibly coded cancer diagnosis")



  #===============================================================================
  # PATIENT SELECTION df_ADM_PatientsCancer: Only patients with cancer
  #===============================================================================

  df_ADM_PatientsCancer <- df_ADM_Patients %>%
                                filter(PatientHoldsMainCancerDiagnosis == TRUE)


  # Update Attrition Tracker
  # ------------------------------------------------------------------------------
  df_Mon_AttritionTracker_CancerPatients <- df_Mon_AttritionTracker_CancerPatients %>%
                                                f_UpdateAttritionTracker(inp_NewSampleSize = n_distinct(df_ADM_PatientsCancer$PatientPseudonym),
                                                                         inp_InclusionComment = "Initial Sample Size")

