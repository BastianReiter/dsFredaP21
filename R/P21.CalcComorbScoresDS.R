
#' P21.CalcComorbScoresDS
#'
#' Calculate comorbidity scores and add them
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
P21.CalcComorbScoresDS <- function(CuratedDataSetName.S = "P21.CuratedDataSet",
                              Settings.S = list(CreateSubsets = list(Cancer = TRUE,
                                                                     HIVCancer = FALSE)))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{





#===============================================================================
# Final modifications
#===============================================================================

  # Conversion of ADS tables from tibble to data.frame, because DataSHIELD can handle data.frames better
  # ADS <- ADS %>%
  #             map(\(Table) as.data.frame(Table))



}

