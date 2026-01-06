
#' P21.LinkToCCPDS
#'
#' Classify suitable P21 reference cases and map them to DiagnosisIDs in CCP data
#'
#' Server-side ASSIGN method
#'
#' @param P21CDSName.S \code{string} - Name of P21 Curated Data Set on servers
#' @param CCPADSName.S \code{string} - Name of CCP Augmented Data Set on servers
#' @param CCPLinkPatIDFeature.S \code{string} - Name of feature that contains CCP Patient IDs that can be linked with P21 Patient IDs
#' @param Tolerance.DischargeToDiagnosis.S \code{integer}
#' @param Tolerance.DiagnosisToAdmission.S \code{integer}
#'
#' @return A \code{data.frame}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
P21.LinkToCCPDS <- function(P21CDSName.S = "P21.CuratedDataSet",
                            CCPADSName.S = "CCP.AugmentedDataSet",
                            CCPLinkPatIDFeature.S = "DKTKIDLocal",
                            Tolerance.DischargeToDiagnosis.S = 30,
                            Tolerance.DiagnosisToAdmission.S = 30)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # P21CDSName.S <- "P21.CuratedDataSet"
  # CCPADSName.S <- "CCP.AugmentedDataSet"
  # CCPLinkPatIDFeature.S <- "DKTKIDLocal"
  # Tolerance.DischargeToDiagnosis.S <- 30
  # Tolerance.DiagnosisToAdmission.S <- 30

  # --- Argument Validation ---
  assert_that(is.string(P21CDSName.S),
              is.string(CCPADSName.S),
              is.string(CCPLinkPatIDFeature.S),
              is.count(Tolerance.DischargeToDiagnosis.S),
              is.count(Tolerance.DiagnosisToAdmission.S))

#-------------------------------------------------------------------------------

  # Get local objects: Parse expression and evaluate
  P21.CDS <- eval(parse(text = P21CDSName.S), envir = parent.frame())
  CCP.ADS <- eval(parse(text = CCPADSName.S), envir = parent.frame())

#-------------------------------------------------------------------------------

  P21.CaseInfo <- P21.CDS$Case %>%
                      select(PatientID,
                             CaseID,
                             AdmissionDate,
                             DischargeDate)

  # Linkage of CCP PatientIDs with P21 PatientIDs and thus linkage of CCP PatientIDs to all associated P21 CaseIDs
  MapCCPPatToP21Cases <- CCP.ADS$Patient %>%
                            mutate(LinkPatientID = .data[[CCPLinkPatIDFeature.S]]) %>%
                            select(PatientID,
                                   LinkPatientID) %>%
                            left_join(P21.CaseInfo, by = join_by(LinkPatientID == PatientID)) %>%
                            rename(P21CaseID = "CaseID") %>%
                            select(-LinkPatientID)

  # Create data.frame that maps all CCP Diagnosis IDs to a P21 case in time-wise proximity to the cancer diagnosis date
  # 1) Linkage of all CCP DiagnosisIDs to ALL P21 CaseIDs
  # 2) Create hierarchy of reference case candidates (Class A, B, C)
  # 3) Sort by hierarchy
  # 4) Save most likely P21 case ID
  #-----------------------------------------------------------------------------
  Mapping <- CCP.ADS$Diagnosis %>%
                select(PatientID,
                       DiagnosisID,
                       DiagnosisDate) %>%
                left_join(MapCCPPatToP21Cases, by = join_by(PatientID), relationship = "many-to-many") %>%
                mutate(IsDiagnosisDateWithinCase = between(DiagnosisDate, AdmissionDate, DischargeDate),
                       IntervalDischargeToDiagnosis = as.numeric(DiagnosisDate - DischargeDate),      # If this number of days is positive, P21 case is located BEFORE cancer diagnosis
                       IntervalDiagnosisToAdmission = as.numeric(AdmissionDate - DiagnosisDate),      # If this number of days is positive, P21 case is located AFTER cancer diagnosis
                       ReferenceCaseLikelihood = case_when(IsDiagnosisDateWithinCase == TRUE ~ 0,      # "Class A" candidate
                                                           between(IntervalDischargeToDiagnosis, 0, Tolerance.DischargeToDiagnosis.S) ~ IntervalDischargeToDiagnosis,      # "Class B" candidate - case in time-wise proximity BEFORE cancer diagnosis
                                                           between(IntervalDiagnosisToAdmission, 0, Tolerance.DiagnosisToAdmission.S) ~ IntervalDiagnosisToAdmission + Tolerance.DischargeToDiagnosis.S,      # "Class C" candidate - case in time-wise proximity AFTER cancer diagnosis. To make cases in this class less likely to be picked as reference case, add 'Tolerance.DischargeToDiagnosis'.
                                                           .default = NA)) %>%
                group_by(PatientID,
                         DiagnosisID) %>%
                arrange(desc(ReferenceCaseLikelihood), .by_group = TRUE) %>%
                summarize(HasReferenceP21Case = any(!is.na(ReferenceCaseLikelihood)),
                          ReferenceP21CaseID = as.character(ifelse(HasReferenceP21Case == TRUE, first(P21CaseID), NA))) %>%
                ungroup()

#-------------------------------------------------------------------------------
  return(as.data.frame(Mapping))
}

