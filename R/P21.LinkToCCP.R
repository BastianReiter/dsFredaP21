
#' P21.LinkToCCPDS
#'
#' Map P21 cases to DiagnosisIDs in CCP data
#'
#' Server-side ASSIGN method
#'
#' @param P21CDSName.S \code{string} - Name of P21 Curated Data Set on servers
#' @param CCPADSName.S \code{string} - Name of CCP Augmented Data Set on servers
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
                            Tolerance.DischargeToDiagnosis.S = 30,
                            Tolerance.DiagnosisToAdmission.S = 30)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # P21CDSName.S <- "P21.CuratedDataSet"
  # CCPADSName.S <- "CCP.AugmentedDataSet"
  # Tolerance.DischargeToDiagnosis.S <- 30
  # Tolerance.DiagnosisToAdmission.S <- 30

  # --- Argument Validation ---
  assert_that(is.string(P21CDSName.S),
              is.string(CCPADSName.S),
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

  MapCCPPatToP21Cases <- CCP.ADS$Patient %>%
                            select(PatientID) %>%
                            left_join(P21.CaseInfo, by = join_by(PatientID)) %>%
                            rename(P21CaseID = "CaseID")

  Mapping <- CCP.ADS$Diagnosis %>%
                select(PatientID,
                       DiagnosisID,
                       DiagnosisDate) %>%
                left_join(MapCCPPatToP21Cases, by = join_by(PatientID), relationship = "many-to-many") %>%
                mutate(IsDiagnosisDateWithinCase = between(DiagnosisDate, AdmissionDate, DischargeDate),
                       IntervalDischargeToDiagnosis = as.numeric(DiagnosisDate - DischargeDate),
                       IntervalDiagnosisToAdmission = as.numeric(AdmissionDate - DiagnosisDate),
                       ReferenceCaseLikelihood = case_when(IsDiagnosisDateWithinCase == TRUE ~ 0,      # "Class A" candidate
                                                           between(IntervalDischargeToDiagnosis, 0, Tolerance.DischargeToDiagnosis) ~ IntervalDischargeToDiagnosis,      # "Class B" candidate - case in time-wise proximity BEFORE cancer diagnosis
                                                           between(IntervalDiagnosisToAdmission, 0, Tolerance.DiagnosisToAdmission) ~ IntervalDiagnosisToAdmission + Tolerance.DischargeToDiagnosis,      # "Class C" candidate - case in time-wise proximity AFTER cancer diagnosis. To make cases in this class less likely to be picked as reference case, add 'Tolerance.DischargeToDiagnosis'.
                                                           .default = NA)) %>%
                group_by(PatientID,
                         DiagnosisID) %>%
                arrange(desc(ReferenceCaseLikelihood), .by_group = TRUE) %>%
                summarize(HasReferenceP21Case = any(!is.na(ReferenceCaseLikelihood)),
                          ReferenceP21CaseID = ifelse(HasReferenceP21Case == TRUE, first(P21CaseID), NA)) %>%
                ungroup()

#-------------------------------------------------------------------------------
  return(as.data.frame(Mapping))
}

