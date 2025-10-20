
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --- DOCUMENTATION of Package Data ---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta.ADS.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Meta data: Table names, feature names and eligible values in AugmentedDataSet (ADS)
#'
#' A \code{tibble} containing meta data
#'
#' @format ## `Meta.ADS`
#' \code{tibble}
#' \describe{
#'   \item{TableName}{Table name}
#'   \item{FeatureName}{Feature name}
#'   \item{ScaleLevel}{Scale level of feature}
#'   \item{Value}{Eligible value}
#'   \item{Label}{Label for values}
#'   \item{HasEligibleValueSet}{logical}
#'   \item{FeatureIsFromCDS}{logical}
#' }
#' @author Bastian Reiter
"Meta.ADS"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set.Privacy.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Privacy settings for FredaP21
#'
#' A \code{list} containing settings concerning data privacy and disclosure mitigation
#'
#' @format ## `Set.Privacy`
#' \code{list}
#' \describe{
#'    \item{Profile}{Can be 'strict' or 'loose'}
#'    \item{NThreshold}{The minimum sample size required for transmission of aggregated data to client}
#' }
"Set.Privacy"
