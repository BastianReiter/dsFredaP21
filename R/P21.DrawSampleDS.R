
#' P21.DrawSampleDS
#'
#' Draws a sample (subset) from Raw Data Set
#'
#' Server-side ASSIGN method
#'
#' @param RawDataSetName.S \code{string} - Name of Raw Data Set object (list) on server - Default: 'P21.RawDataSet'
#' @param SampleSize.S \code{count} - Number of patients in sample
#'
#' @return A \code{list} containing a subset of Raw Data Set
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
P21.DrawSampleDS <- function(RawDataSetName.S = "P21.RawDataSet",
                             SampleSize.S = 100)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---
  assert_that(is.string(RawDataSetName.S),
              is.count(SampleSize.S))

#-------------------------------------------------------------------------------

  # Evaluate input argument
  RawDataSet <- eval(parse(text = RawDataSetName.S), envir = parent.frame())

#-------------------------------------------------------------------------------

  # Get all available PatientIDs
  AllPatientIDs <- unique(RawDataSet$Case$PatientID)
  AvailableNumberPatients <- length(unique(AllPatientIDs))

  # Reduce SampleSize if necessary
  if (SampleSize.S > AvailableNumberPatients) { SampleSize.S <- AvailableNumberPatients }

  # Get a random sample of PatientIDs
  SamplePatientIDs <- sample(AllPatientIDs,
                             size = SampleSize.S)

  # Get the corresponding CaseIDs
  SampleCaseIDs <- RawDataSet$Case %>%
                        filter(PatientID %in% SamplePatientIDs) %>%
                        pull(CaseID) %>%
                        unique()

  # Subset RDS tables with sampled PatientIDs
  RawDataSetSample <- RawDataSet %>%
                          imap(function(Table, tablename)
                               {
                                  if (length(Table) > 0 && nrow(Table) > 0)
                                  {
                                      return(Table %>% filter(CaseID %in% SampleCaseIDs))
                                  }
                                  else { return(NULL) }
                               })

#-------------------------------------------------------------------------------
  return(RawDataSetSample)

}
