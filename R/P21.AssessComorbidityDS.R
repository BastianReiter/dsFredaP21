
#' P21.AssessComorbidityDS
#'
#' Using functionality in package \code{comorbidity}, map ICD codes to morbidity categories and calculate a comorbidity score.
#'
#' Server-side ASSIGN method
#'
#' @param DiagnosisData.S \code{string} - Name of the \code{data.frame} holding diagnostic code data and a grouping identifier feature
#' @param DiagnosticCodeFeature.S \code{string} - Name of the feature containing diagnostic codes
#' @param IDFeature.S \code{string} - Name of the feature containing patient- or case-specific identifier (or any other feature grouping the diagnostic codes)
#' @param IgnoredCategories.S Optional \code{character vector} - Containing morbidity categories that should be ignored in score calculation. See documentation on \code{comorbidity::comorbidity()} for reference.
#' @param ReturnScoreValueOnly.S \code{logical} - Whether the returned \code{data.frame} should only contain the feature with comorbidity score values and not the category-specific features
#' @param ScoreFeatureName.S \code{string} - The name of the feature that holds the comorbidity score values in the returned \code{data.frame}
#' @param Arg.map.S \code{string} - The value for argument 'map' in \code{comorbidity::comorbidity()}
#' @param Arg.assign0.S \code{logical} - The value for argument 'assign0' in \code{comorbidity::comorbidity()}
#' @param Arg.weights.S \code{string} - The value for argument 'weights' in \code{comorbidity::score()}
#'
#' @return A \code{data.frame}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
P21.AssessComorbidityDS <- function(DiagnosisData.S,
                                    DiagnosticCodeFeature.S,
                                    IDFeature.S,
                                    IgnoredCategories.S = NULL,
                                    ReturnScoreValueOnly.S = FALSE,
                                    ScoreFeatureName.S = "ComorbidityScore",
                                    Arg.map.S = "charlson_icd10_quan",
                                    Arg.assign0.S = TRUE,
                                    Arg.weights.S = "quan")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DiagnosisData.S <- "P21.CuratedDataSet$DiagnosisICD"
  # DiagnosticCodeFeature.S <- "ICD10Code"
  # IDFeature.S <- "CaseID"
  # IgnoredCategories.S <- c("canc", "metacanc")
  # ReturnScoreValueOnly.S <- FALSE
  # ScoreFeatureName.S <- "ComorbidityScore"
  # Arg.map.S <- "charlson_icd10_quan"
  # Arg.assign0.S <- TRUE
  # Arg.weights.S <- "quan"

  # --- Argument Validation ---
  assert_that(is.string(DiagnosisData.S),
              is.string(DiagnosticCodeFeature.S),
              is.string(IDFeature.S),
              is.flag(ReturnScoreValueOnly.S),
              is.string(ScoreFeatureName.S),
              is.string(Arg.map.S),
              is.flag(Arg.assign0.S),
              is.string(Arg.weights.S))
  if (!is.null(IgnoredCategories.S)) { assert_that(is.character(IgnoredCategories.S)) }

  # Special validation rules implemented with base::stopifnot() instead of assertthat::assert_that()
  if (str_starts(Arg.map.S, "charlson")) { stopifnot("To calculate Charlson Comorbidity Score, the value for argument 'Arg.weights.S' must be one of 'charlson' or 'quan'!" = (Arg.weights.S %in% c("charlson", "quan"))) }
  if (str_starts(Arg.map.S, "elixhauser")) { stopifnot("To calculate Elixhauser Comorbidity Score, the value for argument 'Arg.weights.S' must be one of 'vw' or 'swiss'!" = (Arg.weights.S %in% c("vw", "swiss"))) }

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  Data <- eval(parse(text = DiagnosisData.S), envir = parent.frame())

#-------------------------------------------------------------------------------

  # Map diagnostic codes to comorbidity categories
  ComorbidityAssessment <- Data %>%
                              select(all_of(c(IDFeature.S, DiagnosticCodeFeature.S))) %>%
                              comorbidity::comorbidity(id = IDFeature.S,
                                                       code = DiagnosticCodeFeature.S,
                                                       map = Arg.map.S,
                                                       assign0 = Arg.assign0.S)

  # Set score values to 0 for all categories specified in 'IgnoredCategories.S'
  # Note: comorbidity::comorbidity() returns a data.frame with additional attributes. Further processing with dplyr verbs deletes these attributes, which are needed in comorbidity::score().
  for (category in IgnoredCategories.S)
  {
      if (category %in% names(ComorbidityAssessment))
      {
          ComorbidityAssessment[category] <- 0
      } else {
          stop(paste0("In argument 'IgnoredCategories.S', the term '", category, "' is not a valid category name (see documentation on comorbidity::comorbidity())!"))
      }
  }

  # Calculation of Comorbidity Score based on previously mapped comorbidity category-specific score values and chosen weights
  ComorbidityAssessment <- ComorbidityAssessment %>%
                                mutate(!!ScoreFeatureName.S := comorbidity::score(x = ComorbidityAssessment,
                                                                             weights = Arg.weights.S,
                                                                             assign0 = Arg.assign0.S))

  # Optionally select only comorbidity score feature for return
  # Note: If this is not the case, set category-specific scores for categories in 'IgnoredCategories.S' NA for clarity in returned data.frame (see else-Statement)
  if (ReturnScoreValueOnly.S == TRUE)
  {
      ComorbidityAssessment <- ComorbidityAssessment %>%
                                    select(all_of(c(IDFeature.S,
                                                    ScoreFeatureName.S)))
  } else {

      ComorbidityAssessment <- ComorbidityAssessment %>%
                                    mutate(across(all_of(IgnoredCategories.S), ~ NA))
  }

#-------------------------------------------------------------------------------
  return(as.data.frame(ComorbidityAssessment))
}

