
#' P21.AugmentDataDS
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Transforms Curated Data Set (CDS) into Augmented Data Set (ADS)
#'
#' Server-side ASSIGN method
#'
#' @param CuratedDataSetName.S \code{character} - Name of the Curated Data Set object on server - Default: 'P21.CuratedDataSet'
#' @param Settings.S \code{list} - Settings passed to function
#'                   \itemize{\item CreateSubsets \code{list}
#'                                \itemize{\item Cancer \code{logical} - Default: \code{TRUE}
#'                                         \item HIVCancer \code{logical} - Default: \code{FALSE}}}
#'
#' @return A \code{list} containing the following objects:
#'         \itemize{\item AugmentedDataSet \code{list}
#'                      \itemize{\item Case \code{data.frame}
#'                               \item Diagnosis \code{data.frame}
#'                               \item Events \code{data.frame}
#'                               \item Patient \code{data.frame}
#'                               \item PatientCancer \code{data.frame}
#'                               \item PatientHIVCancer \code{data.frame}
#'                               \item Procedures \code{data.frame} }
#'                  \item AugmentationReport \code{list}
#'                  \item Messages \code{list}}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
P21.AugmentDataDS <- function(CuratedDataSetName.S = "P21.CuratedDataSet",
                              Settings.S = list(CreateSubsets = list(Cancer = TRUE,
                                                                     HIVCancer = FALSE)))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OVERVIEW
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   SETUP
#   ...
#===============================================================================

  # --- For Testing Purposes ---
  # CDS <- CuratedDataSet
  # Settings.S <- list(CreateSubsets = list(Cancer = TRUE,
  #                                         HIVCancer = TRUE))

  # --- Equip 'Settings' with default values in case of missing arguments ---

  # Rename 'Settings.S' argument for better code readability
  Settings <- Settings.S

  # If list of 'Settings' passed to function is incomplete, complete it with default values
  if (is.null(Settings$CreateSubsets$Cancer)) { Settings$CreateSubsets$Cancer <- TRUE }
  if (is.null(Settings$CreateSubsets$HIVCancer)) { Settings$CreateSubsets$HIVCancer <- FALSE }

  # --- Argument Validation ---
  assert_that(is.string(CuratedDataSetName.S))


#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  CDS <- eval(parse(text = CuratedDataSetName.S), envir = parent.frame())


#-------------------------------------------------------------------------------
# - Initial statements -
#-------------------------------------------------------------------------------

  # Print starting message
  cat("\n")
  Message <- paste0("Starting Data Augmentation...")
  cli::cat_bullet(Message, bullet = "star")
  cat("\n")

  # Suppress summarize info messages
  options(dplyr.summarise.inform = FALSE)

  # Initiate output objects
  ADS <- list()
  AugmentationReport <- NULL

  # Initiate Messaging objects
  Messages <- list()
  Messages$CheckAugmentationCompletion <- "red"
  Messages$FinalMessage <- "Augmentation not completed"

  # ---------- Temporary
  Aux.SampleSize <- CDS$Case %>%
                          ungroup() %>%
                          summarize(Step = "Initial data",
                                    SampleSize = n_distinct(PatientPseudonym))

#-------------------------------------------------------------------------------

  # General Cancer Codes
  # Metastasis codes are included ("C") but will be excluded in script
  Aux.CancerCodes <- c("C",                                                        # All malignant neoplasms (primary and secondary)
                      "D0",                                                       # In-situ neoplasms
                      "D37", "D38", "D39", "D40", "D41", "D42", "D43", "D44", "D45", "D46", "D47", "D48",      # Neoplasms of uncertain / unknown behaviour
                      "Z08",                                                      # Follow-up examination after treatment for malignant neoplasms
                      "Z85", "Z92.6")                                             # Personal history of malignant neoplasms

  # General Codes for Cancer Metastases
  Aux.MetastasisCode <- c("C77", "C78", "C79")

  # Primary HIV codes indicating HIV infection (as used in ZI HIV coding manual)
  # O98.7, Z21, B20, B21, B22, B23.-, B23.0, B23.8, B24
  Aux.HIVCodes <- Res.HIVCoding.Status %>% pull(PrimaryICD10Code) %>% unique()                                                     # Unspecified HIV disease


  # Use tryCatch to catch warnings and errors
  # Note: Warnings and errors must be defined and thrown explicitly for this to work. Unspecified errors will not be caught directly but will also not lead to harsh stops.
  # tryCatch({



#===============================================================================
# Augmented Data Set (ADS)
#===============================================================================

#       _______________         ________________
#      / ADS$Diagnosis \       / ADS$Procedures \
#      \_______________/       \________________/
#
#                      ____________
#                     / ADM$Events \
#                     \____________/
#
#                      __________
#                     / ADS$Case \
#                     \__________/
#
#                      _____________
#                     / ADS$Patient \
#                     \_____________/
#                     --- Optional -------------
#                      ___________________
#                     / ADS$PatientCancer \
#                     \___________________/
#                      ______________________
#                     / ADS$PatientHIVCancer \
#                     \______________________/



#-------------------------------------------------------------------------------
  ProgressBar <- progress_bar$new(format = "Augmenting data... [:bar] :percent in :elapsed  :spin",
                                  total = 26, clear = FALSE, width = 100)
#-------------------------------------------------------------------------------



#===============================================================================
# Initiate Data Frame containing Case data and corresponding (Transfer-related) Events
#===============================================================================
# Add case-specific variables:
#     - CaseNumber
#     - AdmissionYear: Year of admission for subgrouping in reports
#     - LengthOfStay: Length of stay per case in days
#-------------------------------------------------------------------------------
# Join CDS$Case with CDS$Department
#-------------------------------------------------------------------------------
# Look up labels of coded variables via join()
#     - AdmissionCause
#     - DischargeReason
#     - Department
#-------------------------------------------------------------------------------

  cli::cat_bullet("Initiating ADS$Events", bullet = "info")
  #-----------------------------------------------------------------------------
  ADS$Events <- CDS$Case %>%
                    group_by(PatientPseudonym) %>%
                        mutate(CaseNumber = row_number(),
                               AdmissionYear = lubridate::year(AdmissionDate),
                               LengthOfStay = ceiling(as.numeric(difftime(DischargeDate, AdmissionDate, units = "days") + 1))) %>%
                    ungroup() %>%
                    full_join(CDS$Department, by = join_by(CasePseudonym)) %>%
                    filter(is.na(PatientPseudonym) == FALSE) %>%
                    left_join(dsFredaP21::Res.P21.AdmissionCauses, by = join_by(AdmissionCauseCode)) %>%
                    left_join(dsFredaP21::Res.P21.DischargeReasons, by = join_by(DischargeReasonCode)) %>%
                    left_join(dsFredaP21::Res.P21.Departments, by = join_by(DepartmentCode)) %>%
                    arrange(PatientPseudonym, AdmissionDate, DepartmentAdmissionDate) %>%
                    select(PatientPseudonym,
                           CasePseudonym,
                           CaseNumber,
                           YearOfBirth,
                           Sex,
                           PostalCode,
                           AdmissionDate,
                           AdmissionYear,
                           AdmissionAge,
                           AdmissionCauseCode,
                           AdmissionCause,
                           DepartmentCode,
                           Department,
                           OperatingSpecialty,
                           DepartmentAdmissionToICU,
                           DepartmentAdmissionDate,
                           DepartmentDischargeDate,
                           TimeInICU,
                           DischargeDate,
                           DischargeCategory,
                           LengthOfStay,
                           #--- Deselected Features --- # Listed for clarity
                           -AdmissionCauseLabel,
                           -AdmissionCauseLabel.Original,
                           -DischargeReasonCode,
                           -DischargeReasonOriginalLabel,
                           -DepartmentOriginalLabel,
                           -Subspecialty)
                    #=== Update Progress Bar ===
                    try(ProgressBar$tick())


  ADS$Events <- ADS$Events %>%
                    arrange(PatientPseudonym, AdmissionDate, DepartmentAdmissionDate) %>%
                    mutate(DepartmentIsPseudo = (Department == "Pseudo")) %>%
                    group_by(PatientPseudonym, CasePseudonym) %>%
                        add_tally(name = "CountTransfersWithinCase") %>%
                        add_tally(DepartmentAdmissionToICU,
                                  name = "CountTransfersICU") %>%
                        add_tally(DepartmentIsPseudo,
                                  name = "CountTransfersPseudo")
                    #=== Update Progress Bar ===
                    try(ProgressBar$tick())


  cli::cat_bullet("Processing df_ADM_Events: Discrimination of stays", bullet = "info")
  #-----------------------------------------------------------------------------
  ADS$Events <- ADS$Events %>%
                    ungroup() %>%
                    arrange(PatientPseudonym, AdmissionDate, DepartmentAdmissionDate) %>%
                    #--- If Department is "Pseudo" and Transfer Time is neighboring Admission to non-Pseudo-Department, turn it into "real" Department
                    mutate(DepartmentCode = if_else(Department == "Pseudo" & (DepartmentAdmissionDate == tidytable::lag(DepartmentDischargeDate)),
                                                    tidytable::lag(DepartmentCode),
                                                    DepartmentCode)) %>%
                    group_by(across(c(-Department,
                                      -OperatingSpecialty,
                                      -DepartmentIsPseudo,
                                      -DepartmentAdmissionDate,
                                      -DepartmentDischargeDate))) %>%
                        mutate(HasStayedWithoutPause = (DepartmentAdmissionDate == tidytable::lag(DepartmentDischargeDate, default = lubridate::as_date(0))),      # Check: DepartmentAdmissionDate == Previous DepartmentDischargeDate ?
                               WillStayWithoutPause = (DepartmentDischargeDate == tidytable::lead(DepartmentAdmissionDate, default = lubridate::as_date(0))))      # Check: DepartmentDischargeDate == Next DepartmentAdmissionDate ?
                    #=== Update Progress Bar ===
                    try(ProgressBar$tick())


  cli::cat_bullet("Processing ADS$Events: Sorting out real admission and discharge dates", bullet = "info")
  #-----------------------------------------------------------------------------
  ADS$Events <- ADS$Events %>%
                    filter(HasStayedWithoutPause == FALSE | WillStayWithoutPause == FALSE) %>%
                    mutate(DepartmentDischargeDate = if_else(WillStayWithoutPause == TRUE,
                                                             tidytable::lead(DepartmentDischargeDate),
                                                             DepartmentDischargeDate),
                           DepartmentAdmissionDate = if_else(HasStayedWithoutPause == TRUE,
                                                             tidytable::lag(DepartmentAdmissionDate),
                                                             DepartmentAdmissionDate)) %>%
                    ungroup() %>%
                    select(-HasStayedWithoutPause,
                           -WillStayWithoutPause) %>%
                    distinct(PatientPseudonym,
                             CasePseudonym,
                             DepartmentCode,
                             DepartmentAdmissionToICU,
                             DepartmentAdmissionDate,
                             DepartmentDischargeDate,
                             .keep_all = TRUE)
                    #=== Update Progress Bar ===
                    try(ProgressBar$tick())


  ADS$Events <- ADS$Events %>%
                    arrange(PatientPseudonym, AdmissionDate, DepartmentAdmissionDate) %>%
                    group_by(PatientPseudonym, CasePseudonym) %>%
                        mutate(TransferNumber = row_number(),
                               EventDate = DepartmentAdmissionDate,
                               EventClass = "Administration",
                               EventSubclass = case_when(TransferNumber == 1 ~ "Admission",
                                                         TransferNumber > 1 ~ "Transfer"),
                               EventSpecification.A = case_when(TransferNumber == 1 ~ AdmissionCause,
                                                              TransferNumber > 1 ~ Department),
                               EventSpecification.B = case_when(TransferNumber == 1 ~ AdmissionCauseCode,
                                                       TransferNumber > 1 ~ NA),
                               EventSpecification.C = NA,
                               EventLevelOfCare = case_when(DepartmentAdmissionToICU == TRUE ~ "ICU",
                                                            DepartmentAdmissionToICU == FALSE ~ "NonICU",
                                                            is.na(DepartmentAdmissionToICU) ~ "Unknown"),
                               AdmittingDepartmentCode = first(DepartmentCode),
                               AdmittingDepartment = first(Department),
                               TransferLengthOfStay = round(as.numeric(difftime(DepartmentDischargeDate, DepartmentAdmissionDate, units = "days")), 2),
                               CalculatedTimeInICU = sum(TransferLengthOfStay[DepartmentAdmissionToICU == TRUE]))
                    #=== Update Progress Bar ===
                    try(ProgressBar$tick())



  # ---------- Temporary
  AuxSize <- ADS$Events %>% ungroup() %>% summarize(Step = "After first event handling", SampleSize = n_distinct(PatientPseudonym))
  Aux.SampleSize <- rbind(Aux.SampleSize, AuxSize)


#===============================================================================
# Initiate Data Frame of consolidated Case Data
#===============================================================================

  ADS$Case <- ADS$Events %>%
                  group_by(PatientPseudonym, CasePseudonym) %>%
                      filter(TransferNumber == 1) %>%
                      select(PatientPseudonym,
                             CasePseudonym,
                             CaseNumber,
                             YearOfBirth,
                             Sex,
                             PostalCode,
                             AdmissionDate,
                             AdmissionYear,
                             AdmissionAge,
                             AdmissionCauseCode,
                             AdmissionCause,
                             AdmittingDepartmentCode,
                             AdmittingDepartment,
                             TimeInICU,
                             CalculatedTimeInICU,
                             CountTransfersWithinCase,
                             CountTransfersICU,
                             CountTransfersPseudo,
                             DischargeDate,
                             DischargeCategory,
                             LengthOfStay,
                             #--- Deselected Features ---   # Only listed for clarity
                             -TransferNumber,
                             -EventDate,
                             -EventClass,
                             -EventSubclass,
                             -EventSpecification.A,
                             -EventSpecification.B,
                             -EventSpecification.C,
                             -EventLevelOfCare,
                             -DepartmentCode,
                             -Department,
                             -OperatingSpecialty,
                             -DepartmentAdmissionDate,
                             -DepartmentDischargeDate,
                             -DepartmentAdmissionToICU,
                             -DepartmentIsPseudo,
                             -TransferLengthOfStay)
                  #=== Update Progress Bar ===
                  try(ProgressBar$tick())



#===============================================================================
# Obtain Discharge Events and add them to ADS$Events
#===============================================================================

  Aux.DischargeEvents <- ADS$Events %>%
                              group_by(PatientPseudonym, CasePseudonym) %>%
                                  filter(TransferNumber == max(TransferNumber)) %>%
                                  mutate(EventDate = DepartmentDischargeDate,
                                         EventClass = "Administration",
                                         EventSubclass = "Discharge",
                                         EventSpecification.A = DischargeCategory,
                                         EventSpecification.B = NA,
                                         EventSpecification.C = NA,
                                         EventLevelOfCare = case_when(DepartmentAdmissionToICU == TRUE ~ "ICU",
                                                                      DepartmentAdmissionToICU == FALSE ~ "NonICU",
                                                                      is.na(DepartmentAdmissionToICU) ~ "Unknown"))
                              #=== Update Progress Bar ===
                              try(ProgressBar$tick())


  ADS$Events <- ADS$Events %>%
                    ungroup() %>%
                    add_row(Aux.DischargeEvents) %>%
                    arrange(PatientPseudonym, CasePseudonym, EventDate)
                    #=== Update Progress Bar ===
                    try(ProgressBar$tick())


  # ---------- Temporary
  AuxSize <- ADS$Events %>% ungroup() %>% summarize(Step = "After discharge events", SampleSize = n_distinct(PatientPseudonym))
  Aux.SampleSize <- rbind(Aux.SampleSize, AuxSize)



#===============================================================================
# Reconfigure ADS$Events
#===============================================================================

  ADS$Events <- ADS$Events %>%
                    select(PatientPseudonym,
                           YearOfBirth,
                           Sex,
                           CaseNumber,
                           CasePseudonym,
                           AdmissionAge,
                           DepartmentCode,
                           Department,
                           OperatingSpecialty,
                           EventDate,
                           EventClass,
                           EventSubclass,
                           EventSpecification.A,
                           EventSpecification.B,
                           EventSpecification.C,
                           EventLevelOfCare)



#===============================================================================
# ADS$Diagnosis: Diagnosis-oriented Data Frame
#===============================================================================
# Create data frame of Case-related ICD codes
# 1) Attach Case-related ICD Codes
# 2) Use join to identify HIV Status codes from meta data
# 3) Use join to identify HIV Disease codes from meta data
#-------------------------------------------------------------------------------

  ADS$Diagnosis <- ADS$Case %>%
                        left_join(CDS$DiagnosisICD, by = join_by(CasePseudonym)) %>%
                        left_join(dsFredaP21::Res.HIVCoding.Status, by = join_by(ICD10Code == PrimaryICD10Code,
                                                                                 SecondaryICD10Code == SecondaryICD10Code)) %>%
                        left_join(dsFredaP21::Res.HIVCoding.Diseases, by = join_by(ICD10Code == PrimaryICD10Code,
                                                                                   SecondaryICD10Code == SecondaryICD10Code))
                        #=== Update Progress Bar ===
                        try(ProgressBar$tick())



#===============================================================================
# ADS$Diagnosis
#===============================================================================
# Add code-specific variables:
#     - ICD10Code.Short: Short form of full ICD Code to get more general (cancer) diagnoses for easier subgrouping
#     - Is...Code: Identify ICD codes to later construct subgroups of interest
#     - IsCancerCode: True, if code is a cancer code, but not a metastasis code
#     - IsPotentialMainCancer: True, if the cancer code is a "main diagnosis" ("HD") and no code of anamnestic or follow-up character (Z08, Z85, Z92.6)
#     - IsADCode: True, if coded disease is considered AIDS defining. Not equivalent to AIDS code!
#     - IsChronicRenalFailure
#     - IsAIDSCode: True, if code is an AIDS code by itself or if it codes for an AIDS defining disease that occurred after an HIV code
#-------------------------------------------------------------------------------

  ADS$Diagnosis <- ADS$Diagnosis %>%
                          mutate(ICD10Code.Short = str_sub(ICD10Code, end = 3),      # Get first 3 chars of full ICD code to get more general diagnostic entity
                                 IsCancerCode = if_else(str_starts(ICD10Code, paste(Aux.MetastasisCode, collapse = "|")),
                                                        FALSE,
                                                        str_starts(ICD10Code, paste(Aux.CancerCodes, collapse = "|"))),
                                 IsPotentialMainCancer = (IsCancerCode == TRUE &
                                                            DiagnosisType == "HD" &
                                                            str_starts(ICD10Code, "Z08|Z85|Z92.6") == FALSE),
                                 PotentialMainCancerCode = ifelse(any(IsPotentialMainCancer == TRUE),
                                                                  ICD10Code.Short[IsPotentialMainCancer == TRUE],
                                                                  NA),
                                 IsMetastasisCode = str_starts(ICD10Code, paste(Aux.MetastasisCode, collapse = "|")),
                                 IsHIVCode = str_starts(ICD10Code, paste(Aux.HIVCodes, collapse = "|")),
                                 IsADCode = case_when(HIVAssociation == "AIDS defining" ~ TRUE,
                                                      TRUE ~ FALSE),
                                 IsADCodeCancer = case_when((HIVAssociation == "AIDS defining" & HIVDiseaseClass == "Cancer") ~ TRUE,
                                                            TRUE ~ FALSE),
                                 IsADCodeNonCancerous = case_when((HIVAssociation == "AIDS defining" & HIVDiseaseClass != "Cancer") ~ TRUE,
                                                                  TRUE ~ FALSE),
                                 IsHIVNonADCodeCancer = case_when((HIVAssociation == "HIV associated" & HIVDiseaseClass == "Cancer") ~ TRUE,
                                                                  TRUE ~ FALSE),
                                 IsChronicRenalFailure = str_starts(ICD10Code, "N18")) %>%
                          arrange(PatientPseudonym, AdmissionDate, IsADCode) %>%      # HIV codes have to be listed before AIDS defining codes if occurring on the same date
                          group_by(PatientPseudonym) %>%
                              mutate(IsAIDSCode = case_when(HIVStatusInterpretation == "AIDS" ~ TRUE,
                                                            (cumsum(IsHIVCode) > 0 & IsADCode == TRUE) ~ TRUE,      # Check if HIV code already occurred. If so and if the code stands for an AIDS defining disease, interpret as AIDS code.
                                                            TRUE ~ FALSE),      # In "case_when" logic "TRUE ~ " is equivalent to "else"-statement
                                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                     # HIV- and non-cancerous AD and AIDS codes should not be shortened in order to be distinguishable later on
                                     ICD10Code.Short = case_when(IsHIVCode == TRUE ~ ICD10Code,
                                                                 IsADCode == TRUE & IsADCodeCancer == FALSE ~ ICD10Code,
                                                                 IsAIDSCode == TRUE & IsADCodeCancer == FALSE  ~ ICD10Code,
                                                                 TRUE ~ ICD10Code.Short))      # Only evaluated if code is neither HIV nor AD nor AIDS code
                          #=== Update Progress Bar ===
                          try(ProgressBar$tick())



#===============================================================================
# ADS$Diagnosis
#===============================================================================
# Obtain coded diagnosis labels in two forms:
#     - DiagnosisGeneral: More general diagnostic entity (except for AIDS codes)
#     - DiagnosisDetail: Detailed diagnostic entity
#-------------------------------------------------------------------------------
# Obtain additional Cancer Classification Information from Meta Data
#-------------------------------------------------------------------------------

  ADS$Diagnosis <- ADS$Diagnosis %>%
                        left_join(dsFredaP21::Res.ICD10Codes, by = join_by(ICDVersion == ICDVersion,
                                                                           ICD10Code.Short == ICD10Code)) %>%
                        left_join(dsFredaP21::Res.ICD10Codes, by = join_by(ICDVersion == ICDVersion,
                                                                           ICD10Code == ICD10Code)) %>%
                        rename(DiagnosisGeneral = Diagnosis.x,
                               DiagnosisDetail = Diagnosis.y) %>%
                        mutate(DiagnosisGeneral = coalesce(DiagnosisGeneral, DiagnosisDetail)) %>%      # If value in DiagnosisGeneral is NA, take value from DiagnosisDetail
                        left_join(dsFredaP21::Res.CancerGrouping, by = join_by(ICD10Code.Short)) %>%
                        select(-CancerTopographyDetail.German,
                               -CancerTopographyGroup.German,
                               -CancerTopographySpecification.German,
                               -CancerSpecification.German)
                        #=== Update Progress Bar ===
                        try(ProgressBar$tick())



#===============================================================================
# ADS$Diagnosis
#===============================================================================
# Reorder columns
# Omit unneeded variables:
#     - ICD Version
#-------------------------------------------------------------------------------
# Sort by patient pseudonym and admission date
#-------------------------------------------------------------------------------

  ADS$Diagnosis <- ADS$Diagnosis %>%
                        select(#--- Patient-specific data ------------------
                               PatientPseudonym,
                               YearOfBirth,
                               Sex,
                               #--- Case-specific data -------------------------
                               CasePseudonym,
                               CaseNumber,
                               PostalCode,
                               AdmissionDate,
                               AdmissionYear,
                               AdmissionAge,
                               AdmissionCauseCode,
                               AdmissionCause,
                               AdmittingDepartmentCode,
                               AdmittingDepartment,
                               TimeInICU,
                               CalculatedTimeInICU,
                               CountTransfersWithinCase,
                               CountTransfersICU,
                               CountTransfersPseudo,
                               DischargeDate,
                               DischargeCategory,
                               LengthOfStay,
                               #--- Code-specific data -------------------------
                               DiagnosisType,
                               ICD10Code.Short,
                               DiagnosisGeneral,
                               ICD10Code,
                               SecondaryICD10Code,
                               DiagnosisDetail,
                               IsCancerCode,
                               IsPotentialMainCancer,
                               IsMetastasisCode,
                               IsHIVCode,
                               IsAIDSCode,
                               IsADCode,
                               IsADCodeCancer,
                               IsADCodeNonCancerous,
                               IsHIVNonADCodeCancer,
                               IsChronicRenalFailure,
                               HIVDisease.German,
                               HIVDiseaseClass,
                               HIVInformationClass,
                               HIVInformationValue,
                               HIVCodingPlausibility,
                               HIVStatusInterpretation,
                               HIVStadiumClinical,
                               HIVStadiumCellcount,
                               HIVAssociation,
                               CancerTopographyGroup.ICD10,
                               CancerTopographyGroup.ZFKD,
                               CancerTopographySpecification,
                               CancerSpecification,
                               CancerIsLikelyToMetastasize,
                               CancerIsCarcinomaInSitu,
                               CancerIsNeoplasmOfUncertainBehavior) %>%
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        arrange(PatientPseudonym, AdmissionDate, DiagnosisType)



#===============================================================================
# ADS$Diagnosis
#===============================================================================
# Define main cancer disease of patients
#     - IsPresumedMainCancer
# If the context of the code meets certain criteria, presume that it is the first diagnosis of this cancer
#     - IsPresumedMainCancerFirstDiagnosis: Only presumed if there is a potential main cancer code (as defined in df_ADM_CaseDiagnoses) and no other Cancer Code (especially Z85 / Z92.6 or "ND" diagnoses) occurred before or at the same time
# Add information about patient-specific first occurrences of disease codes
#     - IsFirstMetastasisCode: Only presumed if Cancer code occurred before or at the same time
#     - IsFirstHIVCode
#     - IsFirstAIDSCode: Only presumed if HIV code occurred before or at the same time
#-------------------------------------------------------------------------------

  ADS$Diagnosis <- ADS$Diagnosis %>%
                        group_by(PatientPseudonym) %>%
                            arrange(AdmissionDate, .by_group = TRUE) %>%      # Sort all coded diagnoses of each patient by admission date
                        #--- Presumed Cancer Diagnosis -------------------
                        # Step 1: Identify first occurring potential main cancer
                        group_by(PatientPseudonym, IsPotentialMainCancer) %>%
                            mutate(IsPresumedMainCancer = (IsPotentialMainCancer == TRUE & row_number() == 1)) %>%
                        # Step 2: If there is no other Cancer Code that occurred before or at the same time (especially Z85 / Z92.6 or "ND" diagnoses), assume First Diagnosis of Cancer for code that is TRUE in IsPresumedMainCancer
                        group_by(PatientPseudonym) %>%
                            arrange(AdmissionDate, .by_group = TRUE) %>%
                            mutate(IsPresumedMainCancerFirstDiagnosis = IsPresumedMainCancer == TRUE &
                                                                          (any(AdmissionDate[IsCancerCode == TRUE & IsPotentialMainCancer == FALSE] <= AdmissionDate[IsPresumedMainCancer == TRUE]) == FALSE)) %>%
                        #--- Presumed Metastasis Diagnosis ---------------
                        group_by(PatientPseudonym) %>%
                            arrange(AdmissionDate, IsMetastasisCode, .by_group = TRUE) %>%      # Primary Cancer codes have to be listed before Metastasis codes if occurring on the same date. Here FALSE values for IsMetastasisCode appear on top of sorted "list".
                            mutate(IsFirstMetastasisCode = (cumsum(IsPresumedMainCancer) > 0 & IsMetastasisCode == TRUE)) %>%      # Check if Primary Cancer code already occurred
                        group_by(IsMetastasisCode, .add = TRUE) %>%
                            mutate(IsFirstMetastasisCode = (IsFirstMetastasisCode == TRUE & row_number() == 1)) %>%
                        #--- First HIV Code ----------------------
                        group_by(PatientPseudonym, IsHIVCode) %>%
                            arrange(AdmissionDate, .by_group = TRUE) %>%
                            mutate(IsFirstHIVCode = (IsHIVCode == TRUE & row_number() == 1)) %>%
                        #--- First AIDS Code ---------------------
                        group_by(PatientPseudonym, IsAIDSCode) %>%
                            mutate(IsFirstAIDSCode = (IsAIDSCode == TRUE & row_number() == 1)) %>%
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        ungroup()
                        #=== Update Progress Bar ===
                        try(ProgressBar$tick())



#===============================================================================
# Obtain Patient-specific diagnostic events and add them to ADS$Events
#===============================================================================

  Aux.DiagnosisEvents <- ADS$Diagnosis %>%
                              group_by(PatientPseudonym) %>%
                                  mutate(EventDate = AdmissionDate,
                                         #EventDate = AdmissionDate + days(LengthOfStay / 2),
                                         EventClass = "Diagnosis",
                                         EventSubclass = case_when(IsPresumedMainCancer == TRUE | IsFirstMetastasisCode == TRUE ~ "Cancer-related Diagnosis",
                                                                   IsFirstHIVCode == TRUE | IsFirstAIDSCode == TRUE ~ "HIV-related Diagnosis",
                                                                   IsChronicRenalFailure == TRUE ~ "Special Comorbidity Diagnosis",
                                                                   TRUE ~ NA),
                                         EventSpecification.A = case_when(IsPresumedMainCancer == TRUE ~ "Main Cancer Diagnosis",
                                                                          IsFirstMetastasisCode == TRUE ~ "Metastasis Diagnosis",
                                                                          IsFirstHIVCode == TRUE ~ "HIV Diagnosis",
                                                                          IsFirstAIDSCode == TRUE ~ "AIDS Diagnosis",
                                                                          IsChronicRenalFailure == TRUE ~ "Chronic Renal Failure",
                                                                          TRUE ~ NA),
                                         EventSpecification.B = paste0(ICD10Code.Short, ": ", DiagnosisGeneral),
                                         EventSpecification.C = ifelse(IsPresumedMainCancerFirstDiagnosis == TRUE,
                                                                       "Likely First Diagnosis",
                                                                       NA),
                                         EventLevelOfCare = NA) %>%
                                  filter(is.na(EventSpecification.A) == FALSE) %>%
                                  select(PatientPseudonym,
                                         CasePseudonym,
                                         EventDate,
                                         EventClass,
                                         EventSubclass,
                                         EventSpecification.A,
                                         EventSpecification.B,
                                         EventSpecification.C,
                                         EventLevelOfCare) %>%
                              ungroup()
                              #=== Update Progress Bar ===
                              try(ProgressBar$tick())


  ADS$Events <- ADS$Events %>%
                    ungroup() %>%
                    add_row(Aux.DiagnosisEvents) %>%
                    group_by(PatientPseudonym, CasePseudonym) %>%
                        fill(c(YearOfBirth,
                               Sex,
                               AdmissionAge,
                               DepartmentCode,
                               Department,
                               OperatingSpecialty)) %>%
                        mutate(EventDate = lubridate::as_date(ifelse(EventClass == "Diagnosis",
                                                          EventDate[EventSubclass == "Admission"],
                                                          EventDate))) %>%
                        arrange(PatientPseudonym, CasePseudonym, EventDate)
                    #=== Update Progress Bar ===
                    try(ProgressBar$tick())


  # ------------ Temporary
  AuxSize <- ADS$Events %>% ungroup() %>% summarize(Step = "After diagnosis events", SampleSize = n_distinct(PatientPseudonym))
  Aux.SampleSize <- rbind(Aux.SampleSize, AuxSize)


#===============================================================================
# ADS$Case
#===============================================================================
# Obtain Presumed Main Cancer Diagnosis and all Potential Main Cancer Diagnoses for every case and add that information to ADS$Case
#-------------------------------------------------------------------------------

  Aux.CaseSummaries.Diagnosis <- ADS$Diagnosis %>%
                                      group_by(PatientPseudonym, CasePseudonym) %>%
                                          summarize(CaseHoldsCancerCodes = any(IsCancerCode == TRUE),
                                                    CaseHoldsMetastasisCodes = any(IsFirstMetastasisCode == TRUE),
                                                    CaseHoldsHIVCodes = any(IsHIVCode == TRUE),
                                                    CaseHoldsAIDSCodes = any(IsFirstAIDSCode == TRUE),
                                                    #---------------------------
                                                    AllPotentialMainCancers.Case = ifelse(any(IsPotentialMainCancer == TRUE),
                                                                                          paste(unique(ICD10Code.Short[IsPotentialMainCancer == TRUE]), collapse = ", "),
                                                                                          NA),
                                                    CancerousComorbidities.Case = ifelse(any(IsCancerCode == TRUE),
                                                                                         paste(unique(ICD10Code.Short[IsCancerCode == TRUE & DiagnosisType == "ND"]), collapse = ", "),
                                                                                         NA),
                                                    AllDistinctCancerCodes.Case = ifelse(any(IsCancerCode == TRUE),
                                                                                         paste(unique(ICD10Code.Short[IsCancerCode == TRUE]), collapse = ", "),
                                                                                         NA),
                                                    SpecialComorbidities.Case = ifelse(any(IsChronicRenalFailure == TRUE),
                                                                                       ICD10Code.Short[IsChronicRenalFailure == TRUE],
                                                                                       NA),
                                                    #---------------------------
                                                    CaseHoldsMainCancerDiagnosis = any(IsPresumedMainCancer == TRUE),
                                                    CaseHoldsMainCancerFirstDiagnosis = any(IsPresumedMainCancerFirstDiagnosis == TRUE),
                                                    #---------------------------
                                                    HIVStatus.Case = NA) %>%
                                      ungroup()
                                      #=== Update Progress Bar ===
                                      try(ProgressBar$tick())



#   - DistinctCodeCount: Count of all distinct codes
#   - DistinctCodeCountCancer: Count of all "real" cancer codes (no anamnestic codes)
#   - DistinctCodeCountMainCancer: Count of all distinct MAIN cancer codes
#   - DistinctCodeCountHIV: Count of all distinct HIV codes
#   - DistinctCodeCountAIDS: Count of all distinct AIDS codes

  Aux.PatientSummaries.Diagnosis <- ADS$Diagnosis %>%
                                        group_by(PatientPseudonym) %>%
                                            summarize(DistinctCodeCount = n_distinct(ICD10Code.Short),
                                                      DistinctCodeCountCancer = n_distinct(ICD10Code.Short[IsCancerCode == TRUE & str_starts(ICD10Code, "Z08|Z85|Z92.6") == FALSE]),      # Only count "real" cancer codes
                                                      DistinctCodeCountMainCancer = n_distinct(ICD10Code.Short[IsPotentialMainCancer == TRUE]),
                                                      DistinctCodeCountHIV = n_distinct(ICD10Code.Short[IsHIVCode == TRUE]),
                                                      DistinctCodeCountAIDS = n_distinct(ICD10Code.Short[IsAIDSCode == TRUE]),
                                                      #-----------------------
                                                      PatientHoldsCancerCodes = any(IsCancerCode == TRUE),
                                                      PatientHoldsMainCancerDiagnosis = any(IsPresumedMainCancer == TRUE),
                                                      PatientHoldsMainCancerFirstDiagnosis = any(IsPresumedMainCancerFirstDiagnosis == TRUE),
                                                      PatientHoldsMetastasisCodes = any(IsFirstMetastasisCode == TRUE),
                                                      PatientHoldsHIVCodes = any(IsHIVCode == TRUE),
                                                      PatientHoldsAIDSCodes = any(IsFirstAIDSCode == TRUE),
                                                      #-----------------------
                                                      MainCancerCode = ifelse(any(IsPresumedMainCancer == TRUE),
                                                                              ICD10Code[IsPresumedMainCancer == TRUE],
                                                                              NA),
                                                      MainCancerDiagnosis = ifelse(any(IsPresumedMainCancer == TRUE),
                                                                                   DiagnosisDetail[IsPresumedMainCancer == TRUE],
                                                                                   NA),
                                                      MainCancerCodeShort = case_when(str_starts(MainCancerCode, "C") == TRUE ~ str_sub(MainCancerCode, end = 3),
                                                                                      str_starts(MainCancerCode, "D") == TRUE ~ MainCancerCode,
                                                                                      TRUE ~ NA),
                                                      MainCancerTopographyGroup = ifelse(any(IsPresumedMainCancer == TRUE),
                                                                                         CancerTopographyGroup.ICD10[IsPresumedMainCancer == TRUE],
                                                                                         NA),
                                                      # MainCancerTopographyDetail = ifelse(any(IsPresumedMainCancer == TRUE),
                                                      #                                     CancerTopographyDetail_ICD10[IsPresumedMainCancer == TRUE],
                                                      #                                     NA),
                                                      MainCancerIsLikelyToMetastasize = ifelse(any(IsPresumedMainCancer == TRUE),
                                                                                               CancerIsLikelyToMetastasize[IsPresumedMainCancer == TRUE],
                                                                                               NA),
                                                      MainCancerIsCarcinomaInSitu = ifelse(any(IsPresumedMainCancer == TRUE),
                                                                                           CancerIsCarcinomaInSitu[IsPresumedMainCancer == TRUE],
                                                                                           NA),
                                                      MainCancerIsNeoplasmOfUncertainBehavior = ifelse(any(IsPresumedMainCancer == TRUE),
                                                                                                       CancerIsNeoplasmOfUncertainBehavior[IsPresumedMainCancer == TRUE],
                                                                                                       NA),
                                                      MainCancerIsAD = ifelse(any(IsPresumedMainCancer == TRUE),
                                                                              IsADCodeCancer[IsPresumedMainCancer == TRUE],
                                                                              NA),
                                                      MainCancerIsHIVNonAD = ifelse(any(IsPresumedMainCancer == TRUE),
                                                                                    IsHIVNonADCodeCancer[IsPresumedMainCancer == TRUE],
                                                                                    NA),
                                                      #-----------------------
                                                      Patient.AllPotentialMainCancers = ifelse(any(IsPotentialMainCancer == TRUE),
                                                                                               paste(unique(ICD10Code.Short[IsPotentialMainCancer == TRUE]), collapse = ", "),
                                                                                               NA),
                                                      Patient.CancerousComorbidities = ifelse(any(IsCancerCode == TRUE),
                                                                                              paste(unique(ICD10Code.Short[IsCancerCode == TRUE & DiagnosisType == "ND"]), collapse = ", "),
                                                                                              NA),
                                                      Patient.AllDistinctCancerCodes = ifelse(any(IsCancerCode == TRUE),
                                                                                              paste(unique(ICD10Code.Short[IsCancerCode == TRUE]), collapse = ", "),
                                                                                              NA),
                                                      Patient.SpecialComorbidities = ifelse(any(IsChronicRenalFailure == TRUE),
                                                                                            ICD10Code.Short[IsChronicRenalFailure == TRUE],
                                                                                            NA)) %>%
                                        ungroup()
                                        #=== Update Progress Bar ===
                                        try(ProgressBar$tick())



  # Add Information about Potential Main Cancer Diagnoses to Case Data
  ADS$Case <- ADS$Case %>%
                  left_join(Aux.PatientSummaries.Diagnosis, by = join_by(PatientPseudonym)) %>%
                  left_join(Aux.CaseSummaries.Diagnosis, by = join_by(PatientPseudonym, CasePseudonym))
                  #=== Update Progress Bar ===
                  try(ProgressBar$tick())



#===============================================================================
# Calculation of Comorbidity Scores (Charlson and Elixhauser) for every Case
#===============================================================================
# Using weights put forth by Quan et al. for Charlson and Van Walraven et al. for Elixhauser
#-------------------------------------------------------------------------------

  # Get score points for Charlson comorbidity categories
  Aux.ComorbiditiesCharlson <- ADS$Diagnosis %>%
                                    group_by(CasePseudonym) %>%
                                        distinct(ICD10Code) %>%
                                        comorbidity::comorbidity(id = "CasePseudonym",
                                                                 code = "ICD10Code",
                                                                 map = "charlson_icd10_quan",
                                                                 assign0 = TRUE)
                                        # select(-canc,
                                        #        -metacanc,
                                        #        -aids)

  # Calculation of Charlson Comorbidity score
  Aux.ScoreCharlson <- Aux.ComorbiditiesCharlson %>%
                              select(CasePseudonym) %>%
                              mutate(ComorbidityScore.Charlson = comorbidity::score(Aux.ComorbiditiesCharlson,
                                                                                    weights = "quan",
                                                                                    assign0 = TRUE))


  # Get score points for Elixhauser comorbidity categories
  Aux.ComorbiditiesElixhauser <- ADS$Diagnosis %>%
                                      group_by(CasePseudonym) %>%
                                          distinct(ICD10Code) %>%
                                          comorbidity::comorbidity(id = "CasePseudonym",
                                                                   code = "ICD10Code",
                                                                   map = "elixhauser_icd10_quan",
                                                                   assign0 = TRUE)

  # Calculation of score using Van Walraven weights for Elixhauser comorbidity groups
  # AIDS/HIV has weight 0, so we don't have to exclude this comorbidity prior to avoid induced bias in later matching
  # Cancerous comorbidity are included for now
  Aux.ScoreElixhauser <- Aux.ComorbiditiesElixhauser %>%
                              select(CasePseudonym) %>%
                              mutate(ComorbidityScore.Elixhauser = comorbidity::score(Aux.ComorbiditiesElixhauser,
                                                                                      weights = "vw",      # 'swiss' would be a more recently published alternative
                                                                                      assign0 = TRUE))

  # Add Comorbidity Scores to Case Data
  ADS$Case <- ADS$Case %>%
                  left_join(Aux.ScoreCharlson, by = join_by(CasePseudonym)) %>%
                  left_join(Aux.ScoreElixhauser, by = join_by(CasePseudonym))
                  #=== Update Progress Bar ===
                  try(ProgressBar$tick())



#===============================================================================
# ADS$Procedures: Data Frame containing information about Procedures coded for each case
#===============================================================================

  ADS$Procedures <- ADS$Case %>%
                        left_join(CDS$Procedure, by = join_by(CasePseudonym)) %>%
                        mutate(OPSCode.Short = str_sub(OPSCode, end = 4)) %>%
                        left_join(select(dsFredaP21::Res.OPSCodes, c(OPSCode, Procedure)), by = join_by(OPSCode.Short == OPSCode)) %>%
                        mutate(ProcedureType = case_when(str_starts(OPSCode.Short, "5") & OPSCode.Short %notin% c("5411", "5936") ~ "Surgery",
                                                         str_starts(OPSCode.Short, "854") & OPSCode.Short %notin% c("8547", "8548") ~ "Chemotherapy",
                                                         OPSCode.Short == "8547" ~ "Immunotherapy",
                                                         str_starts(OPSCode.Short, "852") ~ "Radiotherapy",
                                                         str_starts(OPSCode.Short, "853") ~ "Nuclear Medicine Therapy",
                                                         OPSCode.Short == "8805" ~ "Stem Cell Therapy",
                                                         OPSCode.Short == "5411" ~ "Bone Marrow Transplant",
                                                         OPSCode.Short == "5936" ~ "Potential CAR-T-Cell Therapy",
                                                         OPSCode.Short == "8548" ~ "Antiretroviral Therapy",
                                                         OPSCode.Short %in% c("8701", "8704", "8706", "8712", "8713", "8714") ~ "Ventilation",
                                                         OPSCode.Short %in% c("8853", "8854", "8855", "8857") ~ "Dialysis")) %>%
                        left_join(select(dsFredaP21::Res.CancerSurgery, c(ICD10Code.Short, OPSCode.Short, starts_with("IsLikely"))),
                                  by = join_by(MainCancerCodeShort == ICD10Code.Short,
                                               OPSCode.Short == OPSCode.Short))
                        #=== Update Progress Bar ===
                        try(ProgressBar$tick())


  Aux.ProcedureEvents <- ADS$Procedures %>%
                                group_by(PatientPseudonym) %>%
                                    mutate(EventDate = OPSDate,
                                           EventClass = "Procedure",
                                           EventSubclass = ProcedureType,
                                           EventSpecification.A = case_when(ProcedureType == "Surgery" & IsLikelyCurativeIntention.Primary == TRUE ~ "CancerSurgery.CurativeIntention.Primary",
                                                                            ProcedureType == "Surgery" & IsLikelyCurativeIntention.Primary == TRUE ~ "CancerSurgery.CurativeIntention.Secondary",
                                                                            ProcedureType == "Surgery" & IsLikelySupportive.Direct == TRUE ~ "CancerSurgery.Supportive",
                                                                            ProcedureType == "Surgery" & IsLikelyCancerRelated == TRUE ~ "CancerSurgery.Other",
                                                                            ProcedureType == "Surgery" & is.na(IsLikelyCancerRelated) ~ "OtherSurgery",
                                                                            TRUE ~ ProcedureType),
                                           EventSpecification.B = Procedure,
                                           EventSpecification.C = NA,
                                           EventLevelOfCare = NA) %>%
                                    filter(is.na(EventSubclass) == FALSE) %>%
                                    select(PatientPseudonym,
                                           CasePseudonym,
                                           EventDate,
                                           EventClass,
                                           EventSubclass,
                                           EventSpecification.A,
                                           EventSpecification.B,
                                           EventLevelOfCare) %>%
                                    distinct() %>%
                                ungroup()
                                #=== Update Progress Bar ===
                                try(ProgressBar$tick())


  ADS$Events <- ADS$Events %>%
                    ungroup() %>%
                    add_row(Aux.ProcedureEvents) %>%
                    #filter(is.na(EventSpecification.A) | EventSpecification.A != "OtherSurgery") %>%       # Disabled this filtering to make pipeline non-cancer-specific
                    group_by(PatientPseudonym, CasePseudonym) %>%
                        fill(c(YearOfBirth,
                               Sex,
                               AdmissionAge,
                               DepartmentCode,
                               Department,
                               OperatingSpecialty)) %>%
                        arrange(PatientPseudonym, CasePseudonym, EventDate) %>%
                        fill(CaseNumber,
                             EventLevelOfCare) %>%
                        mutate(EventClass = factor(EventClass, levels = c("Administration", "Diagnosis", "Procedure")),      # Factorize EventClass to establish order of Admission and Diagnosis events if they have the same EventDate
                               EventLevelOfCare = ifelse(EventClass == "Diagnosis",
                                                         "Not Specified",
                                                         EventLevelOfCare),
                               EventSubclass = case_when(EventSubclass == "Transfer" & EventLevelOfCare == "ICU" ~ "TransferICU",
                                                         EventSubclass == "Transfer" & EventLevelOfCare == "NonICU" ~ "TransferNonICU",
                                                         EventSubclass == "Transfer" & EventLevelOfCare == "Unknown" ~ "TransferNonICU",
                                                         TRUE ~ EventSubclass),
                               EventSpecification.A = case_when(EventSubclass == "Chemotherapy" & EventLevelOfCare == "ICU" ~ "ChemotherapyICU",
                                                                EventSubclass == "Chemotherapy" & EventLevelOfCare == "NonICU" ~ "ChemotherapyNonICU",
                                                                EventSubclass == "Chemotherapy" & EventLevelOfCare == "Unknown" ~ "ChemotherapyUnknown",
                                                                EventSubclass == "Dialysis" & EventLevelOfCare == "ICU" ~ "DialysisICU",
                                                                EventSubclass == "Dialysis" & EventLevelOfCare == "NonICU" ~ "DialysisNonICU",
                                                                EventSubclass == "Dialysis" & EventLevelOfCare == "Unknown" ~ "DialysisUnknown",
                                                                TRUE ~ EventSpecification.A)) %>%
                    group_by(CasePseudonym, EventSpecification.A) %>%
                        arrange(EventDate, .by_group = TRUE) %>%
                        mutate(IsFirstOfProcedure.WithinCase = (EventClass == "Procedure" & row_number() == 1)) %>%
                    group_by(PatientPseudonym, EventSpecification.A) %>%
                        arrange(EventDate, .by_group = TRUE) %>%
                        mutate(IsFirstOfProcedure.AcrossCases = (EventClass == "Procedure" & row_number() == 1)) %>%
                    ungroup() %>%
                    arrange(PatientPseudonym, EventDate, EventClass)
                    #=== Update Progress Bar ===
                    try(ProgressBar$tick())


  # ------------ Temporary
  AuxSize <- ADS$Events %>% ungroup() %>% summarize(Step = "After prodedure events", SampleSize = n_distinct(PatientPseudonym))
  Aux.SampleSize <- rbind(Aux.SampleSize, AuxSize)


  ADS$Events <- ADS$Events %>%
                    group_by(PatientPseudonym) %>%
                        arrange(EventDate, EventClass, .by_group = TRUE) %>%
                        mutate(LastSurgeryDate = lubridate::as_date(ifelse(EventSubclass == "Surgery",
                                                                    EventDate,
                                                                    NA)),
                               LastChemotherapyDate = lubridate::as_date(ifelse(EventSubclass == "Chemotherapy",
                                                                         EventDate,
                                                                         NA)),
                               LastChemotherapyDate.NonICU = lubridate::as_date(ifelse(EventSpecification.A %in% c("ChemotherapyNonICU", "ChemotherapyUnknown"),
                                                                                EventDate,
                                                                                NA))) %>%
                        fill(LastSurgeryDate,
                             LastChemotherapyDate,
                             LastChemotherapyDate.NonICU) %>%
                        mutate(DaysSinceLastSurgery = round(as.numeric(difftime(EventDate, LastSurgeryDate, units = "days")), digits = 1),
                               DaysSinceLastChemotherapy.NonICU = round(as.numeric(difftime(EventDate, LastChemotherapyDate.NonICU, units = "days")), digits = 1),
                               IsPotentialComplication.AcrossCases = (is.na(DaysSinceLastChemotherapy.NonICU) == FALSE & between(DaysSinceLastChemotherapy.NonICU, 0, 30))      # Check if there has been a (Non-ICU-) Chemotherapy event in the past 20 days
                                                                        & (is.na(DaysSinceLastSurgery) == TRUE | DaysSinceLastSurgery > 7)      # Ensure that there has not been a preceding Surgery event in the 7 days before potential complication (across cases)
                                                                        & EventSubclass %in% c("TransferICU",
                                                                                               "Dialysis",
                                                                                               "Ventilation")) %>%      # Admission to ICU, Dialysis or Ventilation event after Chemotherapy without recent surgery is assumed as potential complication occurrence
                    group_by(PatientPseudonym, IsPotentialComplication.AcrossCases) %>%
                        mutate(IsFirstPotentialComplication.AcrossCases = (IsPotentialComplication.AcrossCases == TRUE & row_number() == 1)) %>%
                    group_by(PatientPseudonym, CasePseudonym) %>%
                        arrange(EventDate, EventClass, .by_group = TRUE) %>%
                        mutate(IsPotentialComplication.WithinCase = cumsum(EventSpecification.A == "ChemotherapyNonICU") > 0      # Check if there has been a preceding (Non-ICU-) Chemotherapy event
                                                                      & (is.na(DaysSinceLastSurgery) == TRUE | DaysSinceLastSurgery > 7)      # Ensure that there has not been a Surgery event during the 7 days prior
                                                                      & EventSubclass %in% c("TransferICU",
                                                                                             "Dialysis",
                                                                                             "Ventilation")) %>%      # Admission to ICU, Dialysis or Ventilation event after Chemotherapy without recent surgery is assumed as potential complication occurrence
                    group_by(PatientPseudonym, CasePseudonym, IsPotentialComplication.WithinCase) %>%
                        mutate(IsFirstPotentialComplication.WithinCase = (IsPotentialComplication.WithinCase == TRUE & row_number() == 1)) %>%
                    group_by(PatientPseudonym) %>%
                        arrange(EventDate, EventClass, .by_group = TRUE) %>%
                        mutate(IsPotentialComplication.AcrossCases = ifelse(IsPotentialComplication.AcrossCases == TRUE
                                                                              & EventSubclass == "Dialysis"
                                                                              & cumsum(EventSpecification.A == "Chronic Renal Failure") > 0,
                                                                            FALSE,
                                                                            IsPotentialComplication.AcrossCases),
                               IsPotentialComplication.WithinCase = ifelse(IsPotentialComplication.WithinCase == TRUE
                                                                              & EventSubclass == "Dialysis"
                                                                              & cumsum(EventSpecification.A == "Chronic Renal Failure") > 0,
                                                                            FALSE,
                                                                            IsPotentialComplication.WithinCase),
                               TimeChemoToComplication.AcrossCases = ifelse(IsPotentialComplication.AcrossCases == TRUE,
                                                                            DaysSinceLastChemotherapy.NonICU[IsPotentialComplication.AcrossCases == TRUE],
                                                                            NA),
                               TimeChemoToComplication.WithinCase = ifelse(IsPotentialComplication.WithinCase == TRUE,
                                                                            DaysSinceLastChemotherapy.NonICU[IsPotentialComplication.WithinCase == TRUE],
                                                                            NA)) %>%
                    ungroup()
                    #=== Update Progress Bar ===
                    try(ProgressBar$tick())


  # ------------ Temporary
  AuxSize <- ADS$Events %>% ungroup() %>% summarize(Step = "After event processing", SampleSize = n_distinct(PatientPseudonym))
  Aux.SampleSize <- rbind(Aux.SampleSize, AuxSize)


#   - Presumed...DiagnosisDate
#   - FirstMainAdmissionDate: Date of main cancer diagnosis or HIV diagnosis, whichever is earlier. If there is no main cancer or HIV diagnosis, take earliest admission date.
#   - FirstMainAdmissionAge: Age at first main admission
#   - LastRecordedDischargeDate
#   - LastRecordedDischargeCategory: Taken as rough momentary "outcome" of medical care

  Aux.PatientSummaries.Events <- ADS$Events %>%
                                        group_by(PatientPseudonym) %>%
                                        summarize(PresumedMainCancerDiagnosisDate = lubridate::as_date(ifelse(any(EventSpecification.A == "Main Cancer Diagnosis"),
                                                                                                       EventDate[EventSpecification.A == "Main Cancer Diagnosis"],
                                                                                                       NA)),
                                                  PresumedMainCancerIsLikelyFirstDiagnosis = any(EventSpecification.C == "Likely First Diagnosis"),
                                                  PresumedMainCancerFirstDiagnosisDate = lubridate::as_date(ifelse(any(EventSpecification.C == "Likely First Diagnosis"),
                                                                                                            EventDate[EventSpecification.C == "Likely First Diagnosis"],
                                                                                                            NA)),
                                                  PresumedMetastasisDiagnosisDate = lubridate::as_date(ifelse(any(EventSpecification.A == "Metastasis Diagnosis"),
                                                                                                       EventDate[EventSpecification.A == "Metastasis Diagnosis"],
                                                                                                       NA)),
                                                  PresumedHIVDiagnosisDate = lubridate::as_date(ifelse(any(EventSpecification.A == "HIV Diagnosis"),
                                                                                                EventDate[EventSpecification.A == "HIV Diagnosis"],
                                                                                                NA)),
                                                  PresumedAIDSDiagnosisDate = lubridate::as_date(ifelse(any(EventSpecification.A == "AIDS Diagnosis"),
                                                                                                 EventDate[EventSpecification.A == "AIDS Diagnosis"],
                                                                                                 NA)),
                                                  #-------------------------------
                                                  FirstRelevantAdmissionDate = lubridate::as_date(ifelse(sum(EventSpecification.A == "Main Cancer Diagnosis") == 0 &
                                                                                                    sum(EventSpecification.A == "HIV Diagnosis") == 0,      # If patient holds no main cancer or HIV diagnosis
                                                                                                  min(EventDate[EventSubclass == "Admission"], na.rm = TRUE),      # Then take the first admission date as value
                                                                                                  min(PresumedMainCancerDiagnosisDate, PresumedHIVDiagnosisDate, na.rm = TRUE))),
                                                  FirstRelevantAdmissionYear = lubridate::year(FirstRelevantAdmissionDate),
                                                  FirstRelevantAdmissionAge = first(AdmissionAge[EventDate == FirstRelevantAdmissionDate]),
                                                  #-------------------------------
                                                  MainCancerDiagnosisYear = lubridate::year(PresumedMainCancerDiagnosisDate),
                                                  MainCancerDiagnosisAge = first(AdmissionAge[EventDate == PresumedMainCancerDiagnosisDate]),
                                                  MainCancerDocumentedTimeSpan = ceiling(as.numeric(difftime(max(EventDate), PresumedMainCancerDiagnosisDate, units = "days")) + 1),
                                                  #-------------------------------
                                                  TotalDocumentedTimeSpan = ceiling(as.numeric(difftime(max(EventDate), min(EventDate), units = "days")) + 1),
                                                  RelevantDocumentedTimeSpan = ceiling(as.numeric(difftime(max(EventDate), min(PresumedMainCancerDiagnosisDate, PresumedHIVDiagnosisDate), units = "days")) + 1),
                                                  #-------------------------------
                                                  MainCancerFirstStay.TimeToFirstNonSurgicalTherapy = round(min(c(100000, as.numeric(difftime(lubridate::as_date(EventDate[EventSubclass %in% c("Chemotherapy",
                                                                                                                                                                                         "Immunotherapy",
                                                                                                                                                                                         "Radiotherapy",
                                                                                                                                                                                         "Nuclear Medicine Therapy",
                                                                                                                                                                                         "Stem Cell Therapy",
                                                                                                                                                                                         "Bone Marrow Transplant",
                                                                                                                                                                                         "Potential CAR-T-Cell Therapy")]),
                                                                                                                                          PresumedMainCancerDiagnosisDate,
                                                                                                                                          units = "days"))), na.rm = TRUE), 1),
                                                  MainCancerFirstStay.TimeToFirstNonSurgicalTherapy = ifelse(MainCancerFirstStay.TimeToFirstNonSurgicalTherapy == 100000,
                                                                                                             NA,
                                                                                                             MainCancerFirstStay.TimeToFirstNonSurgicalTherapy),
                                                  #--- Calculate time intervals between significant disease diagnosis / progress
                                                  TimeHIVToCancer = ceiling(as.numeric(difftime(PresumedMainCancerDiagnosisDate, PresumedHIVDiagnosisDate, units = "days"))),
                                                  TimeHIVToAIDS = ceiling(as.numeric(difftime(PresumedAIDSDiagnosisDate, PresumedHIVDiagnosisDate, units = "days"))),
                                                  TimeAIDSToCancer = ceiling(as.numeric(difftime(PresumedMainCancerDiagnosisDate, PresumedAIDSDiagnosisDate, units = "days"))),
                                                  TimeCancerToMetastasis = ceiling(as.numeric(difftime(PresumedMetastasisDiagnosisDate, PresumedMainCancerDiagnosisDate, units = "days"))),
                                                  #--- Replace invalid time intervals with NA ---------
                                                  TimeHIVToCancer = replace(TimeHIVToCancer, which(TimeHIVToCancer < 0), NA),
                                                  TimeHIVToAIDS = replace(TimeHIVToAIDS, which(TimeHIVToAIDS < 0), NA),
                                                  TimeAIDSToCancer = replace(TimeAIDSToCancer, which(TimeAIDSToCancer < 0), NA),
                                                  TimeCancerToMetastasis = replace(TimeCancerToMetastasis, which(TimeCancerToMetastasis < 0), NA),
                                                  #-------------------------------
                                                  LastRecordedDischargeDate = max(EventDate[EventSubclass == "Discharge"], na.rm = TRUE),
                                                  LastRecordedDischargeCategory = EventSpecification.A[EventSubclass == "Discharge" & EventDate == LastRecordedDischargeDate]) %>%
                                        ungroup()
                                        #=== Update Progress Bar ===
                                        try(ProgressBar$tick())



#===============================================================================
# Aux.PatientSummaries.CaseInfo
#===============================================================================
#   - PrimaryPostalCode: Postal code taken from first admission
#   - CaseCount: Number of cases per patient
#   - MeanLengthOfStay: Per patient mean length of stay across cases
#         - Calculate mean length of stay per patient based on age at admission (so only cases of the corresponding age group are included in mean calculation)
#-------------------------------------------------------------------------------

  Aux.PatientSummaries.CaseInfo <- ADS$Case %>%
                                          group_by(PatientPseudonym) %>%
                                              arrange(AdmissionDate, .by_group = TRUE) %>%
                                              summarize(YearOfBirth = first(YearOfBirth),
                                                        Sex = first(Sex),
                                                        PrimaryPostalCode = first(PostalCode),
                                                        #-------------------------
                                                        CaseCount = n_distinct(CasePseudonym),
                                                        LengthOfStayTotal = sum(LengthOfStay),
                                                        #-------------------------
                                                        ICUTransfersAbsolute = sum(CountTransfersICU, na.rm = TRUE),
                                                        ICUTransfersRelative = round(ICUTransfersAbsolute / CaseCount, 2),
                                                        ICUTimeAbsolute = sum(CalculatedTimeInICU, na.rm = TRUE),
                                                        ICUTimeRelative = round(ICUTimeAbsolute / LengthOfStayTotal, 2),
                                                        #-------------------------
                                                        MeanLengthOfStay = round(mean(LengthOfStay, na.rm = TRUE), 2),
                                                        MeanLengthOfStay.Age0To19 = round(mean(LengthOfStay[AdmissionAge < 20], na.rm = TRUE), 2),
                                                        MeanLengthOfStay.Age20To39 = round(mean(LengthOfStay[between(AdmissionAge, 20, 39)], na.rm = TRUE), 2),
                                                        MeanLengthOfStay.Age40To59 = round(mean(LengthOfStay[between(AdmissionAge, 40, 59)], na.rm = TRUE), 2),
                                                        MeanLengthOfStay.Age60To79 = round(mean(LengthOfStay[between(AdmissionAge, 60, 79)], na.rm = TRUE), 2),
                                                        MeanLengthOfStay.Age80AndAbove = round(mean(LengthOfStay[AdmissionAge >= 80], na.rm = TRUE), 2),
                                                        #-------------------------
                                                        MainCancerFirstStay_Length = ifelse(any(CaseHoldsMainCancerDiagnosis == TRUE),
                                                                                                LengthOfStay[CaseHoldsMainCancerDiagnosis == TRUE],
                                                                                                NA),
                                                        ComorbidityScoreAtMainCancerDiagnosis.Charlson = ifelse(any(CaseHoldsMainCancerDiagnosis == TRUE),
                                                                                                                ComorbidityScore.Charlson[CaseHoldsMainCancerDiagnosis == TRUE],
                                                                                                                NA),
                                                        ComorbidityScoreAtMainCancerDiagnosis.Elixhauser = ifelse(any(CaseHoldsMainCancerDiagnosis == TRUE),
                                                                                                                  ComorbidityScore.Elixhauser[CaseHoldsMainCancerDiagnosis == TRUE],
                                                                                                                  NA)) %>%
                                              mutate(across(starts_with("MeanLengthOfStay"), ~ ifelse(is.nan(.x), NA, .x))) %>%
                                          ungroup()
                                          #=== Update Progress Bar ===
                                          try(ProgressBar$tick())


#===============================================================================
# ADS$Patient: Consolidate patient-specific data
#===============================================================================
#   - Join all previously created Patient Summaries
#-------------------------------------------------------------------------------

  ADS$Patient <- Aux.PatientSummaries.CaseInfo %>%
                      left_join(Aux.PatientSummaries.Diagnosis, by = join_by(PatientPseudonym)) %>%
                      left_join(Aux.PatientSummaries.Events, by = join_by(PatientPseudonym))


  # ------------ Temporary
  AuxSize <- ADS$Patient %>% ungroup() %>% summarize(Step = "After creation of ADM_Patients", SampleSize = n_distinct(PatientPseudonym))
  Aux.SampleSize <- rbind(Aux.SampleSize, AuxSize)




#===============================================================================
# PATIENT SUBSET ADS$PatientCancer: Only patients with cancer
#===============================================================================

if (Settings$CreateSubsets$Cancer == TRUE)
{

  ADS$PatientCancer <- ADS$Patient %>%
                            filter(PatientHoldsMainCancerDiagnosis == TRUE)


#===============================================================================
# Create auxiliary data frames with summarizing information about cancer patients
#===============================================================================

  Aux.CancerPatientSummaries.Progress <- ADS$Events %>%
                                              filter(PatientPseudonym %in% ADS$PatientCancer$PatientPseudonym) %>%
                                              group_by(PatientPseudonym) %>%
                                                  summarize(HadAnyCancerTherapy = any(str_starts(EventSpecification.A, "CancerSurgery")) |
                                                                                    any(EventSubclass %in% c("Chemotherapy",
                                                                                                             "Immunotherapy",
                                                                                                             "Radiotherapy",
                                                                                                             "Nuclear Medicine Therapy",
                                                                                                             "Stem Cell Therapy",
                                                                                                             "Bone Marrow Transplant",
                                                                                                             "Potential CAR-T-Cell Therapy")),
                                                            #-------------------
                                                            HadAnyCancerSurgery = any(str_starts(EventSpecification.A, "CancerSurgery")),
                                                            HadCancerSurgery_CurativeIntention = any(EventSpecification.A %in% c("CancerSurgery.CurativeIntention.Primary",
                                                                                                                               "CancerSurgery.CurativeIntention.Secondary")),
                                                            HadCancerSurgery.Supportive = any(EventSpecification.A == "CancerSurgery.Supportive"),
                                                            HadCancerSurgery.Other = any(EventSpecification.A == "CancerSurgery.Other"),
                                                            HadChemotherapy = any(EventSubclass == "Chemotherapy"),
                                                            HadImmunotherapy = any(EventSubclass == "Immunotherapy"),
                                                            HadRadiotherapy = any(EventSubclass == "Radiotherapy"),
                                                            HadNuclearmedTherapy = any(EventSubclass == "Nuclear Medicine Therapy"),
                                                            HadStemCellTherapy = any(EventSubclass == "Stem Cell Therapy"),
                                                            HadBoneMarrowTransplant = any(EventSubclass == "Bone Marrow Transplant"),
                                                            HadPotentialCARTCellTherapy = any(EventSubclass == "Potential CAR-T-Cell Therapy"),
                                                            #-------------------
                                                            DateFirstCancerSurgery = as.Date(lubridate::as_date(ifelse(any(EventSpecification.A %in% c("CancerSurgery.CurativeIntention.Primary",
                                                                                                                          "CancerSurgery.CurativeIntention.Secondary",
                                                                                                                          "CancerSurger_Supportive")),
                                                                                                         min(EventDate[str_starts(EventSpecification.A, "CancerSurgery")], na.rm = TRUE),
                                                                                                         NA))),
                                                            DateFirstChemotherapy = as.Date(lubridate::as_date(ifelse(HadChemotherapy == TRUE,
                                                                                                        min(EventDate[EventSubclass == "Chemotherapy"], na.rm = TRUE),
                                                                                                        NA))),
                                                            DateFirstImmunotherapy = as.Date(lubridate::as_date(ifelse(HadImmunotherapy == TRUE,
                                                                                                         min(EventDate[EventSubclass == "Immunotherapy"], na.rm = TRUE),
                                                                                                         NA))),
                                                            DateFirstRadiotherapy = as.Date(lubridate::as_date(ifelse(HadRadiotherapy == TRUE,
                                                                                                        min(EventDate[EventSubclass == "Radiotherapy"], na.rm = TRUE),
                                                                                                        NA))),
                                                            DateFirstNuclearmedTherapy = as.Date(lubridate::as_date(ifelse(HadNuclearmedTherapy == TRUE,
                                                                                                             min(EventDate[EventSubclass == "Nuclear Medicine Therapy"], na.rm = TRUE),
                                                                                                             NA))),
                                                            DateFirstStemCellTherapy = as.Date(lubridate::as_date(ifelse(HadStemCellTherapy == TRUE,
                                                                                                           min(EventDate[EventSubclass == "Stem Cell Therapy"], na.rm = TRUE),
                                                                                                           NA))),
                                                            DateFirstBoneMarrowTransplant = as.Date(lubridate::as_date(ifelse(HadBoneMarrowTransplant == TRUE,
                                                                                                                     min(EventDate[EventSubclass == "Bone Marrow Transplant"], na.rm = TRUE),
                                                                                                                     NA))),
                                                            DateFirstPotentialCARTCellTherapy = as.Date(lubridate::as_date(ifelse(HadPotentialCARTCellTherapy == TRUE,
                                                                                                                    min(EventDate[EventSubclass == "Potential CAR-T-Cell Therapy"], na.rm = TRUE),
                                                                                                                    NA))),
                                                            #-------------------
                                                            HadComplicationAfterChemo = any(IsPotentialComplication.WithinCase == TRUE) | any(IsPotentialComplication.AcrossCases == TRUE),
                                                            HadComplication_Ventilation = any(IsPotentialComplication.AcrossCases == TRUE & EventSubclass == "Ventilation"),
                                                            HadComplication_Dialysis = any(IsPotentialComplication.AcrossCases == TRUE & EventSubclass == "Dialysis"),
                                                            HadComplication_TransferICU = any(IsPotentialComplication.AcrossCases == TRUE & EventSubclass == "TransferICU"),
                                                            DateFirstComplicationAfterChemo = as.Date(lubridate::as_date(ifelse(any(IsFirstPotentialComplication.AcrossCases == TRUE),
                                                                                                                         EventDate[IsFirstPotentialComplication.AcrossCases == TRUE],
                                                                                                                         NA))),
                                                            TimeChemoToFirstComplication = ifelse(HadComplicationAfterChemo == TRUE,
                                                                                                  TimeChemoToComplication.AcrossCases[IsFirstPotentialComplication.AcrossCases],
                                                                                                  NA),
                                                            #-------------------
                                                            HadMetastasis = any(EventSpecification.A == "Metastasis Diagnosis")) %>%
                                              ungroup()
                                              #=== Update Progress Bar ===
                                              try(ProgressBar$tick())


  # ------------ Temporary
  AuxSize <- Aux.CancerPatientSummaries.Progress %>% ungroup() %>% summarize(Step = "After CancerPatientSummaries_Progress", SampleSize = n_distinct(PatientPseudonym))
  Aux.SampleSize <- rbind(Aux.SampleSize, AuxSize)


#===============================================================================
# Aux.CancerPatientSummaries.TherapySequence
#===============================================================================
# Data frame containing information about sequence of:
#     - Presumed onset of different therapy modalities
#     - Presumed first complication after chemotherapy
#-------------------------------------------------------------------------------

  Aux.CancerPatientSummaries.TherapySequence <- Aux.CancerPatientSummaries.Progress %>%
                                                    select(PatientPseudonym,
                                                           DateFirstCancerSurgery,
                                                           DateFirstChemotherapy,
                                                           DateFirstImmunotherapy,
                                                           DateFirstRadiotherapy,
                                                           DateFirstNuclearmedTherapy,
                                                           DateFirstStemCellTherapy,
                                                           DateFirstBoneMarrowTransplant,
                                                           DateFirstPotentialCARTCellTherapy,
                                                           DateFirstComplicationAfterChemo) %>%
                                                    filter(!if_all(c(everything(), -PatientPseudonym), ~ is.na(.x))) %>%
                                                    group_by(PatientPseudonym) %>%
                                                        pivot_longer(c(everything(), -PatientPseudonym),
                                                                     names_to = "Event",
                                                                     values_to = "TherapyOnsetDate") %>%
                                                        filter(!is.na(TherapyOnsetDate)) %>%
                                                        arrange(PatientPseudonym, TherapyOnsetDate) %>%
                                                    group_by(PatientPseudonym, TherapyOnsetDate) %>%
                                                        summarize(ColSurgery = ifelse(any(Event == "DateFirstCancerSurgery"), "Surgery", NA),
                                                                  ColChemotherapy = ifelse(any(Event == "DateFirstChemotherapy"), "Chemotherapy", NA),
                                                                  ColImmunotherapy = ifelse(any(Event == "DateFirstImmunotherapy"), "Immunotherapy", NA),
                                                                  ColRadiotherapy = ifelse(any(Event == "DateFirstRadiotherapy"), "Radiotherapy", NA),
                                                                  ColNuclearmedTherapy = ifelse(any(Event == "DateFirstNuclearmedTherapy"), "Nuclear Medicine Therapy", NA),
                                                                  ColStemCellTherapy = ifelse(any(Event == "DateFirstStemCellTherapy"), "Nuclear Medicine Therapy", NA),
                                                                  ColBoneMarrowTransplant = ifelse(any(Event == "DateFirstBoneMarrowTransplant"), "Bone Marrow Transplant", NA),
                                                                  ColCARTCellTherapy = ifelse(any(Event == "DateFirstPotentialCARTCellTherapy"), "CAR-T-Cell Therapy", NA),
                                                                  ColComplication = ifelse(any(Event == "DateFirstComplicationAfterChemo"), "Complication", NA)) %>%
                                                        unite(col = "TherapyOnset",
                                                              c("ColSurgery",
                                                                "ColChemotherapy",
                                                                "ColImmunotherapy",
                                                                "ColRadiotherapy",
                                                                "ColNuclearmedTherapy",
                                                                "ColStemCellTherapy",
                                                                "ColBoneMarrowTransplant",
                                                                "ColCARTCellTherapy",
                                                                "ColComplication"),
                                                              remove = TRUE, na.rm = TRUE, sep = " & ") %>%
                                                        mutate(Stage = row_number(), .after = PatientPseudonym) %>%
                                                    group_by(PatientPseudonym) %>%
                                                    pivot_wider(names_from = Stage, values_from = c(TherapyOnset, TherapyOnsetDate)) %>%
                                                    ungroup()
                                                    #=== Update Progress Bar ===
                                                    try(ProgressBar$tick())


  # ------------ Temporary
  AuxSize <- Aux.CancerPatientSummaries.TherapySequence %>% ungroup() %>% summarize(Step = "After CancerPatientSummaries.TherapySequence", SampleSize = n_distinct(PatientPseudonym))
  Aux.SampleSize <- rbind(Aux.SampleSize, AuxSize)


#===============================================================================
# ADS$PatientCancer
#===============================================================================
#   - Join all previously created Patient Summaries
#   - Add Subgrouping of relation between cancer entity and HIV, regardless of actual HIV status
#-------------------------------------------------------------------------------

  ADS$PatientCancer <- ADS$PatientCancer %>%
                            left_join(Aux.CancerPatientSummaries.Progress, by = join_by(PatientPseudonym)) %>%
                            left_join(Aux.CancerPatientSummaries.TherapySequence, by = join_by(PatientPseudonym)) %>%
                            mutate(PatientSubgroupHIVCancerCategory = case_when(MainCancerIsAD == TRUE ~ "HIV-associated AD cancer",
                                                                                MainCancerIsHIVNonAD == TRUE ~ "HIV-associated non-AD cancer",
                                                                                (MainCancerIsAD == FALSE & MainCancerIsHIVNonAD == FALSE) ~ "Non-HIV-associated cancer"))

if (Settings$CreateSubsets$HIVCancer)
{

#===============================================================================
# SUBSET ADS$PatientHIVCancer: Patients with presumed HIV and cancer
#===============================================================================
# Add variable regarding order of presumed HIV and cancer diagnosis:
#     - HIV before cancer
#     - Diagnosed simultaneously
#     - Cancer before HIV
#-------------------------------------------------------------------------------
# Add variable regarding cancer categories in patients with presumed HIV infection and cancer:
#     - Patients with AIDS-defining cancer, occurring in manifested AIDS
#     - Patients with AIDS-defining cancer, occurring before presumed HIV diagnosis, so can not be considered manifested AIDS
#     - Patients with HIV-associated, non-AIDS-defining cancer, occurring at or after presumed HIV diagnosis
#     - Patients with HIV-associated, non-AIDS-defining cancer, occurring before presumed HIV diagnosis
#     - Patients with Non-HIV-associated cancer, occurring at or after presumed HIV diagnosis
#     - Patients with Non-HIV-associated cancer, occurring before presumed HIV diagnosis
#-------------------------------------------------------------------------------
# Add variable regarding AIDS occurrence in patients with presumed HIV infection and cancer
#     - Patients with presumed AIDS diagnosis at or before presumed cancer diagnosis
#     - Patients with presumed AIDS diagnosis after presumed cancer diagnosis
#     - Patients with cancer without AIDS
#-------------------------------------------------------------------------------

  ADS$PatientHIVCancer <- ADS$PatientCancer %>%
                                  filter(PatientHoldsHIVCodes == TRUE) %>%
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  mutate(HIVCancerDiagnosisOrder = case_when(PresumedHIVDiagnosisDate < PresumedMainCancerDiagnosisDate ~ "HIV before cancer",
                                                                             PresumedHIVDiagnosisDate == PresumedMainCancerDiagnosisDate ~ "Diagnosed simultaneously",
                                                                             PresumedHIVDiagnosisDate > PresumedMainCancerDiagnosisDate ~ "Cancer before HIV"),
                                         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                         HIVCancerCategory = case_when((MainCancerIsAD == TRUE &
                                                                          PresumedMainCancerDiagnosisDate >= PresumedHIVDiagnosisDate) ~ "HIV-associated AD cancer",
                                                                       (MainCancerIsAD == TRUE &
                                                                          PresumedMainCancerDiagnosisDate < PresumedHIVDiagnosisDate) ~ "HIV-associated AD cancer before presumed HIV diagnosis",
                                                                       (MainCancerIsHIVNonAD == TRUE &
                                                                          PresumedMainCancerDiagnosisDate >= PresumedHIVDiagnosisDate) ~ "HIV-associated non-AD cancer",
                                                                       (MainCancerIsHIVNonAD == TRUE &
                                                                          PresumedMainCancerDiagnosisDate < PresumedHIVDiagnosisDate) ~ "HIV-associated non-AD cancer before presumed HIV diagnosis",
                                                                       (MainCancerIsAD == FALSE & MainCancerIsHIVNonAD == FALSE &
                                                                          PresumedMainCancerDiagnosisDate >= PresumedHIVDiagnosisDate) ~ "Non-HIV-associated cancer",
                                                                       (MainCancerIsAD == FALSE & MainCancerIsHIVNonAD == FALSE &
                                                                          PresumedMainCancerDiagnosisDate < PresumedHIVDiagnosisDate) ~ "Non-HIV-associated cancer before presumed HIV diagnosis"),
                                         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                         AIDSOccurrence = case_when((PatientHoldsAIDSCodes == TRUE &
                                                                        PresumedAIDSDiagnosisDate <= PresumedMainCancerDiagnosisDate) ~ "AIDS at or before cancer diagnosis",
                                                                    (PatientHoldsAIDSCodes == TRUE &
                                                                        PresumedAIDSDiagnosisDate > PresumedMainCancerDiagnosisDate) ~ "AIDS after cancer diagnosis",
                                                                     PatientHoldsAIDSCodes == FALSE ~ "Cancer and HIV without AIDS")) %>%      # Most general case is evaluated last, following case_when syntax
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  ungroup()
                                  #=== Update Progress Bar ===
                                  try(ProgressBar$tick())


  vc_HIVCancerCategory <- unique(ADS$PatientHIVCancer$HIVCancerCategory)
  names(vc_HIVCancerCategory) <- vc_HIVCancerCategory



#===============================================================================
# Aux.HIVCancerPatientSummaries.HIVCancerSequence
#===============================================================================
# Data frame containing information about sequence of:
#     - Cancer diagnosis
#     - Metastasis diagnosis
#     - HIV diagnosis
#     - AIDS diagnosis
# ... and all combinations
#-------------------------------------------------------------------------------

  Aux.HIVCancerPatientSummaries.HIVCancerSequence <- ADS$PatientHIVCancer %>%
                                                            select(PatientPseudonym,
                                                                   PresumedMainCancerDiagnosisDate,
                                                                   PresumedMetastasisDiagnosisDate,
                                                                   PresumedHIVDiagnosisDate,
                                                                   PresumedAIDSDiagnosisDate) %>%
                                                            filter(!if_all(c(everything(), -PatientPseudonym), ~ is.na(.x))) %>%
                                                            group_by(PatientPseudonym) %>%
                                                            pivot_longer(c(everything(), -PatientPseudonym),
                                                                         names_to = "Event",
                                                                         values_to = "DiagnosisSequenceDate") %>%
                                                            filter(!is.na(DiagnosisSequenceDate)) %>%
                                                            arrange(PatientPseudonym, DiagnosisSequenceDate) %>%
                                                        group_by(PatientPseudonym, DiagnosisSequenceDate) %>%
                                                            summarize(ColCancer = ifelse(any(Event == "PresumedMainCancerDiagnosisDate"), "Cancer", NA),
                                                                      ColMetastasis = ifelse(any(Event == "PresumedMetastasisDiagnosisDate"), "Metastasis", NA),
                                                                      ColHIV = ifelse(any(Event == "PresumedHIVDiagnosisDate"), "HIV", NA),
                                                                      ColAIDS = ifelse(any(Event == "PresumedAIDSDiagnosisDate"), "AIDS", NA)) %>%
                                                            unite(col = "DiagnosisSequence",
                                                                  c("ColCancer",
                                                                    "ColMetastasis",
                                                                    "ColHIV",
                                                                    "ColAIDS"),
                                                                  remove = TRUE, na.rm = TRUE, sep = " & ") %>%
                                                            mutate(Stage = row_number(), .after = PatientPseudonym) %>%
                                                        group_by(PatientPseudonym) %>%
                                                        pivot_wider(names_from = Stage, values_from = c(DiagnosisSequence, DiagnosisSequenceDate)) %>%
                                                        ungroup()
                                                        #=== Update Progress Bar ===
                                                        try(ProgressBar$tick())



#===============================================================================
# ADS$PatientHIVCancer
#===============================================================================
#   - Add Diagnosis Sequence
#-------------------------------------------------------------------------------

  ADS$PatientHIVCancer <- ADS$PatientHIVCancer %>%
                                left_join(df_Aux_HIVCancerPatientSummaries_HIVCancerSequence, by = join_by(PatientPseudonym)) %>%
                                ungroup()
                                #=== Update Progress Bar ===
                                try(ProgressBar$tick())



#===============================================================================
# PATIENT SELECTION df_ADM_PatientsCancer_Strict / df_ADM_PatientsHIVCancer_Strict
#===============================================================================
#   - Among cancer coded patients, keep only ones with strictly plausible cancer diagnosis onset documentation
#-------------------------------------------------------------------------------

  # df_ADM_PatientsCancer_Strict <- df_ADM_PatientsCancer %>%
  #                                     filter(MainCancerFirstStay_Length > 4
  #                                             & MainCancerFirstStay.TimeToFirstNonSurgicalTherapy > 2)
  #
  #
  # df_ADM_PatientsHIVCancer_Strict <- df_ADM_PatientsHIVCancer %>%
  #                                         filter(MainCancerFirstStay_Length > 4
  #                                                 & MainCancerFirstStay.TimeToFirstNonSurgicalTherapy > 2)

}}


  #--- Terminate PB ---
  try(ProgressBar$terminate())



#===============================================================================
# Final modifications
#===============================================================================

  # Conversion of ADS tables from tibble to data.frame, because DataSHIELD can handle data.frames better
  ADS <- ADS %>%
              map(\(Table) as.data.frame(Table))


#===============================================================================
# Define content of AugmentationReport
#===============================================================================

  ls_AugmentationReport <- list(Test = c("TestReport"))


  Messages$CheckAugmentationCompletion <- "green"
  Messages$FinalMessage <- "Augmentation performed successfully!"

  # },
  #
  # # In case of occurring warning:
  # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # warning = function(w)
  #           {
  #               Messages$CheckAugmentationCompletion <- "yellow"
  #               Messages$FinalMessage <- paste0("Completed Augmentation with following warning: \n", w)
  #           },
  #
  # # In case of occurring error:
  # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # error = function(e)
  #         {
  #             Messages$CheckAugmentationCompletion <- "red"
  #             Messages$FinalMessage <- paste0("An error occured: \n", e)
  #         },
  #
  # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # # RETURN STATEMENT
  # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # finally =
  # {
  #   # Return the Augmented Data Set (ADS), an Augmentation Report (defined above) and Messages
    return(list(AugmentedDataSet = ADS,
                AugmentationReport = ls_AugmentationReport,
                Messages = Messages))
  # })

}

