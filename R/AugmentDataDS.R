
#' AugmentDataDS
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Transforms Curated Data Set (CDS) into Augmented Data Set (ADS)
#'
#' Server-side ASSIGN method
#'
#' @param CuratedDataSetName.S \code{character} - Name of the Curated Data Set object on server - Default: 'CuratedDataSet'
#' @param Settings.S \code{list} - Settings passed to function
#'                   \itemize{\item EventFeatures \code{list}
#'                                \itemize{\item RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_EventFeatures}
#'                                         \item Profile \code{character} - Profile name defining rule set to be used for event feature engineering. Profile name must be stated in \code{EventFeatures$RuleSet} - Default: 'Default'}}
#'
#' @return A \code{list} containing the following objects:
#'         \itemize{\item AugmentedDataSet \code{list}
#'                      \itemize{\item Events
#'                               \item DiseaseCourse
#'                               \item Therapy
#'                               \item Diagnosis
#'                               \item Patient}
#'                  \item AugmentationReport \code{list}
#'                  \item AugmentationMessages \code{list}}
#' @export
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
AugmentDataDS <- function(CuratedDataSetName.S = "CuratedDataSet",
                          Settings.S = list(CutoffValues = list(DaysDiagnosisToInitialStaging = 30),
                                            EventFeatures = list(RuleSet = dsCCPhos::Meta_EventFeatures,
                                                                 Profile = "Default"),
                                            OverallSurvival = list(ReferenceEvent = c(EventClass = "Diagnosis",
                                                                                      EventSubclass = "InitialDiagnosis")),
                                            TherapyOfInterest = list(EventSubclass = "Surgery",
                                                                     EventSubclassRank = 1),
                                            TimeToEvent = list(ReferenceEvent = list(EventClass = "Diagnosis",
                                                                                     EventSubclass = "InitialDiagnosis"),
                                                               TargetEvent = list(EventClass = "VitalStatus",
                                                                                  EventSubclass = "Deceased"))))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OVERVIEW
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #   SETUP
  #     - Evaluation and parsing of input
  #     - Loading of required package namespaces
  #
  #
  #   MODULE A) Classify associations between diagnosis entries
  #
  #   MODULE B)  Creation of ADS$Events
  #       - Diagnosis-related
  #       - Patient-related
  #
  #   MODULE C)  Creation of ADS$DiseaseCourse
  #       - On basis of ADS$Events
  #
  #   MODULE D)  Creation of ADS$Therapy
  #       - Consolidate information from ADS$Events
  #
  #   MODULE E)  Creation of ADS$Diagnosis
  #       - Consolidate information from ADS$Events
  #
  #   MODULE F)  Creation of ADS$Patient
  #       - Consolidate information from ADS$Events, ADS$Therapy and ADS$Diagnosis
  #
  #   Return statement


  # --- For Testing Purposes ---
  # Settings.S <- list(CutoffValues = list(DaysDiagnosisToInitialStaging = 50),
  #                    DiagnosisAssociation = list(Check = TRUE,
  #                                                RuleSet = dsCCPhos::Meta_DiagnosisAssociation,
  #                                                Profile = "Default"),
  #                    EventFeatures = list(RuleSet = dsCCPhos::Meta_EventFeatures,
  #                                         Profile = "Default"),
  #                    TherapyOfInterest = list(EventSubclass = "Surgery",
  #                                             EventSubclassRank = 1),
  #                    TimeToEvent = list(ReferenceEvent = c(EventClass = "Diagnosis",
  #                                                          EventSubclass = "InitialDiagnosis"),
  #                                       TargetEvent = c(EventClass = "VitalStatus",
  #                                                       EventSubclass = "Deceased")))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # - Package requirements -
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Use require() to load package namespaces
  require(dplyr)
  require(lubridate)
  require(purrr)
  # require(slider)
  require(stringr)
  require(tidyr)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # - Equip 'Settings.S' with default values in case of missing arguments -
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Rename 'Settings.S' argument for better code readability
  Settings <- Settings.S

  # If list of 'Settings' passed to function is incomplete, complete it with default values
  if (is.null(Settings$DiagnosisAssociation$Check)) { Settings$DiagnosisAssociation$Check <- TRUE }
  if (is.null(Settings$DiagnosisAssociation$RuleSet)) { Settings$DiagnosisAssociation$RuleSet <- dsCCPhos::Meta_DiagnosisAssociation }
  if (is.null(Settings$DiagnosisAssociation$Profile)) { Settings$DiagnosisAssociation$Profile <- "Default" }
  if (is.null(Settings$EventFeatures$RuleSet)) { Settings$EventFeatures$RuleSet <- dsCCPhos::Meta_EventFeatures }
  if (is.null(Settings$EventFeatures$Profile)) { Settings$EventFeatures$Profile <- "Default" }
  if (is.null(Settings$TimeToEvent)) { Settings$TimeToEvent <- list(ReferenceEvent = c(EventClass = "Diagnosis",
                                                                                       EventSubclass = "InitialDiagnosis"),
                                                                    TargetEvent = c(EventClass = "VitalStatus",
                                                                                    EventSubclass = "Deceased")) }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # - Evaluate and parse input before proceeding -
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (is.character(CuratedDataSetName.S))
  {
      CDS <- eval(parse(text = CuratedDataSetName.S), envir = parent.frame())

  } else {

      ClientMessage <- "ERROR: 'CuratedDataSetName.S' must be specified as a character string"
      stop(ClientMessage, call. = FALSE)
  }

  # if (Settings$EventFeatures$Profile %in% names(Settings$EventFeatures$RuleSet) == FALSE)
  # {
  #     ClientMessage <- "ERROR: Value of settings argument 'EventFeatures$Profile' must be column name of data.frame passed in settings argument 'EventFeatures$RuleSet'."
  #     stop(ClientMessage, call. = FALSE)
  # }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # - Initial statements -
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


  # Use tryCatch to catch warnings and errors
  # Note: Warnings and errors must be defined and thrown explicitly for this to work. Unspecified errors will not be caught directly but will also not lead to harsh stops.
  # tryCatch({


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Augmented Data Model (ADM)
  # Augmented Data Set (ADS)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #       _______________         _______________
  #      / ADS_Diagnosis \       / ADS_Procedure \
  #      \_______________/       \_______________/
  #
  #                      ____________
  #                     / ADS_Events \
  #                     \____________/
  #
  #                      __________
  #                     / ADS_Case \
  #                     \__________/
  #
  #                      _____________
  #                     / ADS_Patient \
  #                     \_____________/




  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # MODULE B)  Generate ADS$Events
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #   1) Initiation
  #   2) Loop through CDS tables and generate event data
  #   3) Enhance data set with useful features
  #-------------------------------------------------------------------------------


  #--- Set up progress bar -------------------------------------------------------
  CountProgressItems <- 17
  ProgressBar <- progress_bar$new(format = "Generating diagnosis-related events [:bar] :percent in :elapsed  :spin",
                                  total = CountProgressItems, clear = FALSE, width = 100)
  try(ProgressBar$tick())
  #-------------------------------------------------------------------------------



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 1) Initiate ADS$Events, integrating patient-specific and initial diagnosis events
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Initiation 1: Initial diagnosis event
  ADS$Events <- CDS$Patient %>%
                    right_join(CDS$Diagnosis, join_by(PatientID)) %>%
                    filter(IsReferenceEntry == TRUE) %>%
                    group_by(PatientID, DiagnosisID) %>%
                        mutate(EventType = "Point",
                               EventDate = DiagnosisDate,
                               EventDateEnd = NULL,      # For events of type "Period"
                               EventDateIsAdjusted = FALSE,      # In case event date is adjusted later for plausibility reasons
                               EventClass = "Diagnosis",
                               EventSubclass = "InitialDiagnosis",
                               EventSubclassRank = row_number(),
                               EventOrderSignificance = NULL) %>%
                        nest(EventDetails = c(ICD10Code,
                                              ICDOTopographyCode,
                                              ICDOMorphologyCode,
                                              Grading)) %>%
                        select(PatientID,
                               DateOfBirth,
                               DiagnosisID,
                               DiagnosisDate,
                               starts_with("Event"))
                        #--- Update PB ---
                        try(ProgressBar$tick())


  # Initiation 2: Last known vital status
  df_Events_LastVitalStatus <- CDS$Patient %>%
                                    right_join(CDS$Diagnosis, join_by(PatientID)) %>%
                                    filter(IsReferenceEntry == TRUE) %>%
                                    group_by(PatientID, DiagnosisID) %>%
                                        mutate(EventType = "Point",
                                               EventDate = LastVitalStatusDate,
                                               EventDateIsAdjusted = FALSE,
                                               EventClass = "General",
                                               EventSubclass = "LastVitalStatus",
                                               LastVitalStatus = if_else(is.na(LastVitalStatus), "Unknown", LastVitalStatus)) %>%
                                        nest(EventDetails = c(LastVitalStatus,
                                                              DeathCancerRelated,
                                                              CausesOfDeath)) %>%
                                    ungroup() %>%
                                    select(PatientID,
                                           DateOfBirth,
                                           DiagnosisID,
                                           DiagnosisDate,
                                           starts_with("Event"))
                                    #--- Update PB ---
                                    try(ProgressBar$tick())


  # Initiation 3: Row-bind data frames from Initiation 1 and 2
  ADS$Events <- ADS$Events %>%
                    bind_rows(df_Events_LastVitalStatus)

  try(ProgressBar$tick())



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 2) Loop through CDS tables to generate event data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  EventData <- CDS[names(CDS) %in% c("Patient", "Diagnosis") == FALSE] %>%      # Deselect tables 'Patient' and 'Diagnosis' from CDS
                    imap(function(Table, tablename)
                         {
                            try(ProgressBar$tick())

                            GroupingFeature <- NULL
                            DateFeature <- NULL
                            EndDateFeature <- NULL
                            Val_EventType <- NA
                            Val_EventClass <- NA
                            Val_EventSubclass <- NA
                            EventDetailsFeatures <- NULL

                            if (tablename == "BioSampling")
                            {
                                GroupingFeature <- "PatientID"
                                DateFeature <- "BioSamplingDate"
                                Val_EventType <- "Point"
                                Val_EventClass <- "Diagnostics"
                                Val_EventSubclass <- "BioSampling"
                                EventDetailsFeatures <- c("Aliquot",
                                                          "Type",
                                                          "Status",
                                                          "Quantity",
                                                          "Unit",
                                                          "ProjectName")
                            }
                            if (tablename == "DiseaseStatus")
                            {
                                GroupingFeature <- c("PatientID", "DiagnosisID")
                                DateFeature <- "DiseaseStatusDate"
                                Val_EventType <- "Point"
                                Val_EventClass <- "Diagnosis"
                                Val_EventSubclass <- "DiseaseStatus"
                                EventDetailsFeatures = c("GlobalStatus",
                                                         "PrimarySiteStatus",
                                                         "LymphnodalStatus",
                                                         "MetastasisStatus")
                            }
                            if (tablename == "GeneralCondition")
                            {
                                GroupingFeature <- c("PatientID", "DiagnosisID")
                                DateFeature <- "GeneralConditionDate"
                                Val_EventType <- "Point"
                                Val_EventClass <- "Diagnostics"
                                Val_EventSubclass <- "GeneralCondition"
                                EventDetailsFeatures = c("ECOG")
                            }
                            if (tablename == "Histology")
                            {
                                GroupingFeature <- c("PatientID", "DiagnosisID")
                                DateFeature <- "HistologyDate"
                                Val_EventType <- "Point"
                                Val_EventClass <- "Diagnostics"
                                Val_EventSubclass <- "Histology"
                                EventDetailsFeatures = c("ICDOMorphologyCode",
                                                         "ICDOMorphologyVersion",
                                                         "Grading",
                                                         "NumberLymphnodesExamined",
                                                         "NumberLymphnodesAffected",
                                                         "NumberSentinelLymphnodesExamined",
                                                         "NumberSentinelLymphnodesAffected")
                            }
                            if (tablename == "Metastasis")
                            {
                                GroupingFeature <- c("PatientID", "DiagnosisID")
                                DateFeature <- "MetastasisDate"
                                Val_EventType <- "Point"
                                Val_EventClass <- "Diagnosis"
                                Val_EventSubclass <- "Metastasis"
                                EventDetailsFeatures = c("HasMetastasis",
                                                         "Localization")
                            }
                            if (tablename == "MolecularDiagnostics")
                            {
                                GroupingFeature <- c("PatientID", "DiagnosisID")
                                DateFeature <- "MolecularDiagnosticsDate"
                                Val_EventType <- "Point"
                                Val_EventClass <- "Diagnostics"
                                Val_EventSubclass <- "MolecularDiagnostics"
                                EventDetailsFeatures = c("MolecularMarker",
                                                         "MolecularMarkerStatus",
                                                         "Documentation")
                            }
                            if (tablename == "OtherClassification")
                            {
                                GroupingFeature <- c("PatientID", "DiagnosisID")
                                DateFeature <- "OtherClassificationDate"
                                Val_EventType <- "Point"
                                Val_EventClass <- "Diagnosis"
                                Val_EventSubclass <- "Staging"
                                EventDetailsFeatures = c("Classification",
                                                         "Class")
                            }
                            if (tablename == "RadiationTherapy")
                            {
                                GroupingFeature <- c("PatientID", "DiagnosisID")
                                DateFeature <- "RadiationTherapyStartDate"
                                EndDateFeature <- "RadiationTherapyEndDate"
                                Val_EventType <- "Period"
                                Val_EventClass <- "Therapy"
                                Val_EventSubclass <- "RadiationTherapy"
                                EventDetailsFeatures = c("Intention",
                                                         "RelationToSurgery",
                                                         "ApplicationType",
                                                         "RadiationType",
                                                         "TargetArea",
                                                         "TargetAreaSide",
                                                         "TotalDose",
                                                         "TotalDoseUnit",
                                                         "SingleDailyDose",
                                                         "SingleDailyDoseUnit",
                                                         "Boost",
                                                         "EndReason",
                                                         "AdverseEventGrade",
                                                         "AdverseEventType",
                                                         "AdverseEventVersion")
                            }
                            if (tablename == "Staging")
                            {
                                GroupingFeature <- c("PatientID", "DiagnosisID")
                                DateFeature <- "StagingDate"
                                Val_EventType <- "Point"
                                Val_EventClass <- "Diagnosis"
                                Val_EventSubclass <- "Staging"
                                EventDetailsFeatures = c("UICCStage",
                                                         "TNM_T_Prefix",
                                                         "TNM_T",
                                                         "TNM_N_Prefix",
                                                         "TNM_N",
                                                         "TNM_M_Prefix",
                                                         "TNM_M",
                                                         "TNM_mSymbol",
                                                         "TNM_rSymbol",
                                                         "TNM_ySymbol",
                                                         "TNMVersion",
                                                         "TNM_L",
                                                         "TNM_V",
                                                         "TNM_Pn",
                                                         "TNM_S")
                            }
                            if (tablename == "Surgery")
                            {
                                GroupingFeature <- c("PatientID", "DiagnosisID")
                                DateFeature <- "SurgeryDate"
                                Val_EventType <- "Point"
                                Val_EventClass <- "Therapy"
                                Val_EventSubclass <- "Surgery"
                                EventDetailsFeatures = c("Intention",
                                                         "OPSCode",
                                                         "OPSVersion",
                                                         "ResidualAssessmentLocal",
                                                         "ResidualAssessmentTotal",
                                                         "SurgeryComplicationsICD10",
                                                         "SurgeryComplicationsADT")
                            }
                            if (tablename == "SystemicTherapy")
                            {
                                GroupingFeature <- c("PatientID", "DiagnosisID")
                                DateFeature <- "SystemicTherapyStartDate"
                                EndDateFeature <- "SystemicTherapyEndDate"
                                Val_EventType <- "Period"
                                Val_EventClass <- "Therapy"
                                Val_EventSubclass <- "SystemicTherapy"
                                EventDetailsFeatures = c("Intention",
                                                         "RelationToSurgery",
                                                         "Type",
                                                         "IsChemotherapy",
                                                         "IsHormoneTherapy",
                                                         "IsImmunotherapy",
                                                         "IsBoneMarrowTransplant",
                                                         "IsObservantStrategy",
                                                         "Protocol",
                                                         "Substance",
                                                         "ATC",
                                                         "ATCVersion",
                                                         "CTCAEGrade",
                                                         "CTCAEType",
                                                         "CTCAEVersion")
                            }
                            if (tablename == "TherapyRecommendation")
                            {
                                GroupingFeature <- "PatientID"
                                DateFeature <- "TherapyRecommendationDate"
                                Val_EventType <- "Point"
                                Val_EventClass <- "Diagnosis"
                                Val_EventSubclass <- "TherapyRecommendation"
                                EventDetailsFeatures = c("Type",
                                                         "Deviation")
                            }

                            # Create table of events for every CDS table, incorporating values / features defined above
                            if (!(is.null(Table) | length(Table) == 0 | nrow(Table) == 0))
                            {
                                if (length(GroupingFeature) == 1) { TableEventData <- Table %>% group_by(PatientID) }
                                else { TableEventData <- Table %>% group_by(PatientID, DiagnosisID) }

                                TableEventData <- TableEventData %>%
                                                          arrange(!!sym(DateFeature), .by_group = TRUE) %>%
                                                          mutate(EventType = Val_EventType,
                                                                 EventDate = !!sym(DateFeature),
                                                                 EventDateIsAdjusted = FALSE,      # In case event date is adjusted later for plausibility reasons
                                                                 EventClass = Val_EventClass,
                                                                 EventSubclass = Val_EventSubclass,
                                                                 EventSubclassRank = row_number(),
                                                                 EventOrderSignificance = case_when(row_number() == 1 ~ paste("First", Val_EventSubclass),
                                                                                                    row_number() == n() ~ paste("Last", Val_EventSubclass),
                                                                                                    TRUE ~ NA_character_)) %>%
                                                          { if (!is.null(EndDateFeature))
                                                            { mutate(., EventDateEnd = !!sym(EndDateFeature), .after = EventDate) }      # For events of type "Period"
                                                            else {.}
                                                          } %>%
                                                          nest(EventDetails = all_of(EventDetailsFeatures)) %>%
                                                      ungroup() %>%
                                                      select(all_of(GroupingFeature),
                                                             starts_with("Event"))
                                return(TableEventData)
                            }

                            else { return(NULL) }

                         }) %>%
                    list_rbind()


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 3) Consolidate Event data from CDS tables in one coherent table
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Define order of events if they fall on the same date
  SubclassOrder <- c("BioSampling" = 1,
                     "Histology" = 2,
                     "MolecularDiagnostics" = 3,
                     "InitialDiagnosis" = 4,
                     "Staging" = 5,
                     "Metastasis" = 6,
                     "DiseaseStatus" = 7,
                     "GeneralCondition" = 8,
                     "TherapyRecommendation" = 9,
                     "Surgery" = 10,
                     "SystemicTherapy" = 11,
                     "RadiationTherapy" = 12,
                     "LastVitalStatus" = 13)

  ADS$Events <- ADS$Events %>%
                    bind_rows(EventData) %>%
                    mutate(SubclassOrder = SubclassOrder[EventSubclass]) %>%
                    group_by(PatientID) %>%
                        fill(DateOfBirth,
                             .direction = "downup") %>%
                    group_by(PatientID, DiagnosisID) %>%
                        filter(!is.na(EventDate)) %>%
                        arrange(EventDate, SubclassOrder, .by_group = TRUE) %>%
                        # Important adjustment!
                        mutate(FirstEventDate = min(EventDate, na.rm = TRUE),
                               LastEventDate = max(EventDate, na.rm = TRUE),
                               EventDateIsAdjusted = case_when(EventSubclass == "InitialDiagnosis" & EventDate > FirstEventDate ~ TRUE,
                                                               EventClass == "VitalStatus" & EventDate < LastEventDate ~ TRUE,
                                                               .default = FALSE),
                               EventDate = case_when(EventSubclass == "InitialDiagnosis" ~ FirstEventDate,
                                                     EventClass == "VitalStatus" ~ LastEventDate,
                                                     .default = EventDate),
                               InitialDiagnosisDate = FirstEventDate) %>%
                        arrange(EventDate, .by_group = TRUE) %>%      # Sort by date again after possible date adjustments
                        mutate(EventRank = row_number(),
                               EventDaysSinceDiagnosis = round(as.numeric(difftime(EventDate, InitialDiagnosisDate, units = "days")), digits = 1),
                               EventPatientAge = floor(time_length(difftime(EventDate, DateOfBirth), unit = "years"))) %>%
                    group_by(PatientID, DiagnosisID, EventClass) %>%
                        mutate(EventClassRank = row_number()) %>%
                    select(PatientID,
                           DateOfBirth,
                           DiagnosisID,
                           InitialDiagnosisDate,
                           EventRank,
                           EventType,
                           EventDate,
                           EventDateEnd,
                           EventClass,
                           EventClassRank,
                           EventSubclass,
                           EventSubclassRank,
                           EventOrderSignificance,
                           EventDateIsAdjusted,
                           EventPatientAge,
                           EventDaysSinceDiagnosis,
                           EventDetails,
                           SubclassOrder) %>%
                    ungroup()
                    #--- Update PB ---
                    try(ProgressBar$tick())

  #--- Terminate PB ---
  try(ProgressBar$terminate())



  #Temporary
  # UnnestedEvents <- df_ADS_Events %>%
  #                       unnest(cols = c(EventDetails), keep_empty = TRUE)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 4) Enhance ADS$Events with engineered features (customizable through function argument / meta data)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #--- Set up progress bar -------------------------------------------------------
  # CountProgressItems <- ADS$Events %>% select(PatientID, DiagnosisID) %>% n_distinct()
  # ProgressBar <- progress_bar$new(format = "Engineering event data [:bar] :percent in :elapsed  :spin",
  #                                 total = CountProgressItems, clear = FALSE, width= 100)
  # #-------------------------------------------------------------------------------




  # ADS$Events <- ADS$Events %>%
  #                   group_by(PatientID, DiagnosisID) %>%
  #                       unnest(cols = c(EventDetails), keep_empty = TRUE) %>%
  #                       group_modify( ~ CreateEventFeatures(EventData = .x,
  #                                                           RuleSet = Settings$EventFeatures$RuleSet,
  #                                                           Profile = Settings$EventFeatures$Profile,
  #                                                           ProgressBarObject = ProgressBar))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # MODULE C)  Generate ADS$DiseaseCourse
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  # Auxiliary function
  AssignComparatorCodes <- function(ValueVector, FeatureName)
                           {
                               # Get value to comparator code matches from meta data
                               ComparatorCodeData <- dsCCPhos::Meta_Values %>%
                                                        filter(Feature == FeatureName) %>%
                                                        select(Value_Curated, ComparatorCode)

                               # Create named vector
                               ComparatorCodes <- set_names(ComparatorCodeData$ComparatorCode,
                                                            ComparatorCodeData$Value_Curated)

                               # Use named vector to transform original values into comparator codes
                               return(suppressWarnings(as.integer(ComparatorCodes[ValueVector])))
                           }


  ColumnNamesStaging <- ADS$Events %>%
                            filter(EventSubclass == "Staging") %>%
                            unnest(cols = c(EventDetails), keep_empty = TRUE) %>%
                            colnames()


  InitialDiseaseStatus <- ADS$Events %>%
                              filter(EventClass == "Therapy" | (EventSubclass == "Staging" & EventSubclassRank == 1)) %>%
                              unnest(cols = c(EventDetails), keep_empty = TRUE) %>%
                              mutate(HadTherapy = case_when(EventClass == "Therapy" ~ TRUE,
                                                            .default = NA)) %>%
                              group_by(PatientID, DiagnosisID) %>%
                                  fill(HadTherapy, .direction = "down") %>%
                                  mutate(HadTherapy = replace_na(HadTherapy, FALSE)) %>%
                              ungroup() %>%
                              filter(EventSubclass == "Staging") %>%
                              mutate(IsLikelyInitialStaging = (HadTherapy == FALSE & EventDaysSinceDiagnosis < Settings$CutoffValues$DaysDiagnosisToInitialStaging),
                                     PrimarySiteStatus_Initial = case_when(TNM_T == 0 ~ 0,
                                                                           .default = 1),
                                     LymphnodalStatus_Initial = case_when(str_starts(TNM_N, "0") ~ 0,
                                                                          .default = 1),
                                     MetastasisStatus_Initial = case_when(str_starts(TNM_M, "0") ~ 0,
                                                                          .default = 1)) %>%
                              select(all_of(ColumnNamesStaging),
                                     IsLikelyInitialStaging,
                                     PrimarySiteStatus_Initial,
                                     LymphnodalStatus_Initial,
                                     MetastasisStatus_Initial)


  ColumnNamesDiseaseStatus <- ADS$Events %>%
                                  filter(EventSubclass == "DiseaseStatus") %>%
                                  unnest(cols = c(EventDetails), keep_empty = TRUE) %>%
                                  colnames()


  ADS$DiseaseCourse <- ADS$Events %>%
                            filter(EventSubclass == "DiseaseStatus") %>%
                            unnest(cols = c(EventDetails), keep_empty = TRUE) %>%
                            bind_rows(InitialDiseaseStatus) %>%
                            mutate(PrimarySiteStatus_Comp = case_when(EventSubclass == "Staging" ~ PrimarySiteStatus_Initial,
                                                                      .default = AssignComparatorCodes(PrimarySiteStatus, "PrimarySiteStatus")),
                                   LymphnodalStatus_Comp = case_when(EventSubclass == "Staging" ~ LymphnodalStatus_Initial,
                                                                     .default = AssignComparatorCodes(LymphnodalStatus, "LymphnodalStatus")),
                                   MetastasisStatus_Comp = case_when(EventSubclass == "Staging" ~ MetastasisStatus_Initial,
                                                                     .default = AssignComparatorCodes(MetastasisStatus, "MetastasisStatus"))) %>%
                            group_by(PatientID, DiagnosisID) %>%
                                arrange(EventDate, SubclassOrder, .by_group = TRUE) %>%
                                mutate(PrimarySiteStatus_Diff = PrimarySiteStatus_Comp - lag(PrimarySiteStatus_Comp),
                                       LymphnodalStatus_Diff = LymphnodalStatus_Comp - lag(LymphnodalStatus_Comp),
                                       MetastasisStatus_Diff = MetastasisStatus_Comp - lag(MetastasisStatus_Comp)) %>%
                            ungroup() %>%
                            mutate(PrimarySiteChange = case_when(PrimarySiteStatus_Diff == 0 ~ "Stable",
                                                                 PrimarySiteStatus_Diff < 0 ~ "Regression",
                                                                 PrimarySiteStatus_Diff > 0 ~ "Progression",
                                                                 .default = "Unclear"),
                                   LymphnodalChange = case_when(LymphnodalStatus_Diff == 0 ~ "Stable",
                                                                LymphnodalStatus_Diff < 0 ~ "Regression",
                                                                LymphnodalStatus_Diff > 0 ~ "Progression",
                                                                .default = "Unclear"),
                                   MetastasisChange = case_when(MetastasisStatus_Diff == 0 ~ "Stable",
                                                                MetastasisStatus_Diff < 0 ~ "Regression",
                                                                MetastasisStatus_Diff > 0 ~ "Progression",
                                                                .default = "Unclear"),
                                   IsProgression = case_when(GlobalStatus %in% c("D", "P") ~ TRUE,
                                                             PrimarySiteStatus %in% c("rpT", "R") | LymphnodalStatus %in% c("pL", "L") | MetastasisStatus %in% c("pM", "M") ~ TRUE,
                                                             PrimarySiteChange == "Progression" | LymphnodalChange == "Progression" | MetastasisChange == "Progression" ~ TRUE,
                                                             .default = FALSE),
                                   IsContradiction = GlobalStatus %in% c("CR", "CRr") & IsProgression == TRUE,
                                   IsResponse = case_when(GlobalStatus %in% c("CR", "CRr", "PartRemission", "MinResp") ~ TRUE,
                                                          IsProgression == TRUE ~ FALSE,
                                                          IsContradiction == TRUE ~ NA,
                                                          is.na(GlobalStatus) & (PrimarySiteChange == "Regression" |
                                                                                 LymphnodalChange == "Regression" |
                                                                                 MetastasisChange == "Regression") ~ TRUE,
                                                          .default = FALSE),
                                   IsStableDisease = case_when(GlobalStatus == "NC" ~ TRUE,
                                                               (is.na(GlobalStatus) &
                                                                  (PrimarySiteChange == "Stable" & LymphnodalChange == "Stable" & MetastasisChange == "Stable")) ~ TRUE,
                                                               .default = FALSE),
                                   IsInRemission = case_when(GlobalStatus %in% c("CR", "CRr", "PartRemission") ~ TRUE,
                                                             is.na(GlobalStatus) & (PrimarySiteStatus == "N" & LymphnodalStatus == "N" & MetastasisStatus == "N") ~ TRUE,
                                                             .default = FALSE),
                                   IsNewRemission = case_when(IsInRemission == TRUE & (PrimarySiteChange == "Regression" | LymphnodalChange == "Regression" | MetastasisChange == "Regression") ~ TRUE,
                                                              .default = FALSE),
                                   IsStableRemission = case_when(IsInRemission == TRUE & (PrimarySiteChange == "Stable" & LymphnodalChange == "Stable" & MetastasisChange == "Stable") ~ TRUE,
                                                                 .default = FALSE)) %>%
                              filter(EventSubclass == "DiseaseStatus") %>%
                              select(all_of(ColumnNamesDiseaseStatus),
                                     PrimarySiteChange,
                                     LymphnodalChange,
                                     MetastasisChange,
                                     IsProgression,
                                     IsContradiction,
                                     IsResponse,
                                     IsStableDisease,
                                     IsInRemission,
                                     IsNewRemission,
                                     IsStableRemission)



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # MODULE D)  Generate ADS$Therapy
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # TO DO:
  # - Group therapies (same chemotherapy, adjuvant/neoadjuvant therapy, time-wise distance, etc)
  # - Classify into lines of therapy


  ColumnNamesTherapy <- ADS$Events %>%
                            filter(EventClass == "Therapy") %>%
                            unnest(cols = c(EventDetails), keep_empty = TRUE) %>%
                            colnames()

  ADS$Therapy <- ADS$Events %>%
                      filter(EventClass == "Therapy") %>%
                      unnest(cols = c(EventDetails), keep_empty = TRUE) %>%
                      mutate(IsTherapyOfInterest = if_all(all_of(names(Settings$TherapyOfInterest)), ~ .x == Settings$TherapyOfInterest[[cur_column()]]),
                             AssociatedTherapyID = EventClassRank,
                             AssociatedTherapyDate = EventDate) %>%
                      bind_rows(ADS$DiseaseCourse) %>%
                      group_by(PatientID, DiagnosisID) %>%
                          arrange(EventDate, SubclassOrder, .by_group = TRUE) %>%      # Sort again by EventDate and SubclassOrder
                          fill(AssociatedTherapyID, .direction = "down") %>%
                          fill(AssociatedTherapyDate, .direction = "down") %>%
                          mutate(TimeSinceTherapy = round(as.numeric(difftime(EventDate, AssociatedTherapyDate, units = "days")), digits = 1)) %>%
                      group_by(PatientID, DiagnosisID, AssociatedTherapyID) %>%
                          mutate(HasResponse = any(IsResponse, na.rm = TRUE),
                                 TimeToResponse = TimeSinceTherapy[which(IsResponse == TRUE)[1]],
                                 TimeToRemission = TimeSinceTherapy[which(IsInRemission == TRUE)[1]],
                                 TimeToRelapse = TimeSinceTherapy[which(IsProgression == TRUE)[1]]) %>%
                      ungroup() %>%
                      filter(EventClass == "Therapy") %>%
                      select(all_of(ColumnNamesTherapy),
                             IsTherapyOfInterest,
                             HasResponse,
                             TimeToResponse,
                             TimeToRemission,
                             TimeToRelapse)


  # TherapySummary <- ADS$Therapy %>%
  #                       summarize()


  # Features in ADS$Therapy

  # FirstTherapyType
  # FirstTherapyLatency   (Abstand Diagnose - FirstTherapy)



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # MODULE E)  Generate ADS$Diagnosis
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  #--- Set up progress bar -------------------------------------------------------
  CountProgressItems <- ADS$Events %>% select(PatientID, DiagnosisID) %>% n_distinct()
  ProgressBar <- progress_bar$new(format = "Summarizing event data [:bar] :percent in :elapsed  :spin",
                                  total = CountProgressItems, clear = FALSE, width= 100)
  #-------------------------------------------------------------------------------

  # Summarize diagnosis-specific event data using dsCCPhos::SummarizeEventData()
  DiagnosisEventSummary <- ADS$Events %>%
                                unnest(cols = c(EventDetails), keep_empty = TRUE) %>%
                                group_by(PatientID, DiagnosisID) %>%
                                    group_modify(~ SummarizeEventData(EventData = .x,
                                                                      ProgressBarObject = ProgressBar)) %>%
                                ungroup()


  DiagnosisDataAugmentation <- CDS$Diagnosis %>%
                                    left_join(CDS$Staging, by = join_by(PatientID, DiagnosisID), relationship = "many-to-many") %>%
                                    group_by(DiagnosisID) %>%
                                        arrange(DiagnosisDate) %>%
                                        slice_head() %>%
                                    ungroup() %>%
                                    mutate(UICCStageCategory = case_match(UICCStage,
                                                                          c("0", "0is", "0a") ~ "0",
                                                                          c("I", "IA", "IA1", "IA2", "IA3", "IB", "IB1", "IB2", "IC", "IS") ~ "I",
                                                                          c("II", "IIA", "IIA1", "IIA2", "IIB", "IIC") ~ "II",
                                                                          c("III", "IIIA", "IIIB", "IIIC", "IIIC1", "IIIC2", "IIID") ~ "III",
                                                                          c("IV", "IVA", "IVB", "IVC") ~ "IV",
                                                                          .default = NA_character_),
                                           .after = UICCStage)
                                    #--- Update PB ---
                                    # try(ProgressBar$tick())


  ADS$Diagnosis <- DiagnosisDataAugmentation %>%
                        left_join(DiagnosisEventSummary, by = join_by(PatientID, DiagnosisID)) %>%
                        #filter(is.na(TimeDiagnosisToDeath) | TimeDiagnosisToDeath >= 0) %>%
                        ungroup()
                        #--- Update PB ---
                        # try(ProgressBar$tick())



  # Construct diagnosis-related endpoints for clinical outcome measures from event history
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #   - Overall survival (OS)
  #   -
  #   - Progression-free survival (PFS)
  #   - Disease-free survival (DFS)
  #
  #   - Overall response rate

  #   - Disease specific survival (Death from disease or from treatment)
  #      - TimeSurgeryToDeath
  #      - TimeLastTherapyToDeath
  #
  #   - TimeTherapyToProgression
  #   - TimeTherapyToDeath

  # df_DiagnosisSummary_Histology <- df_CDS_Histology %>%
  #                                       group_by(DiagnosisID) %>%
  #                                       summarize(CountSubdiagnoses = n_distinct(SubDiagnosisID),
  #                                                 CountHistologyReports = n_distinct(HistologyID))
  #
  #
  # df_ADS_Diagnoses <- df_CDS_Diagnosis %>%
  #                         left_join(df_DiagnosisSummary_Histology, by = join_by(DiagnosisID))
  #
  #

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # MODULE F)  Generate ADS$Patients
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #--- Set up progress bar -------------------------------------------------------
  CountProgressItems <- 3
  ProgressBar <- progress_bar$new(format = "Composing patient-specific data [:bar] :percent in :elapsed  :spin",
                                  total = CountProgressItems, clear = FALSE, width = 100)
  try(ProgressBar$tick())
  #-------------------------------------------------------------------------------


  df_Aux_PatientSummary_Diagnosis <- CDS$Diagnosis %>%
                                          group_by(PatientID) %>%
                                              summarize(CountDiagnoses = n_distinct(DiagnosisID)) %>%
                                          ungroup()
                                          #--- Update PB ---
                                          try(ProgressBar$tick())


  ADS$Patient <- CDS$Patient %>%
                      left_join(df_Aux_PatientSummary_Diagnosis, by = join_by(PatientID))
                      #--- Update PB ---
                      try(ProgressBar$tick())

  #--- Terminate PB ---
  try(ProgressBar$terminate())



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Final modifications
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Conversion of ADS tables from tibble to data.frame, because DataSHIELD can handle data.frames better
  ADS <- ADS %>%
              map(\(Table) as.data.frame(Table))



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Define content of AugmentationReport
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
                AugmentationMessages = Messages))
  # })

}


