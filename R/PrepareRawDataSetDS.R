
#' PrepareRawDataSetDS
#'
#' Load
#'
#' Server-side ASSIGN method
#'
#' @param RawDataSetName.S \code{string} - Name of Raw Data Set object (list) on server - Default: 'RawDataSet'
#' @param SampleSize \code{string} - Number of patients in sample
#'
#' @return A \code{list} containing a subset of Raw Data Set
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PrepareRawDataSetDS <- function()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)
  require(purrr)


  DataSet


  # Try to harmonize raw feature names using fuzzy string matching
  #-------------------------------------------------------------------------------
  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          # Get expected raw feature names
                          EligibleFeatureNames <- filter(dsP21Curator::Meta_Features, TableName_Curated == tablename)$FeatureName_Raw

                          # Create tibble containing tracks of all feature names, including harmonization attempt with fuzzy string matching
                          FeatureNames <- tibble(Original = names(Table),
                                                 IsEligible = (Original %in% EligibleFeatureNames),
                                                 Harmonized = GetFuzzyStringMatches(Vector = Original,
                                                                                    EligibleStrings = EligibleFeatureNames,
                                                                                    PreferredMethod = "jw",
                                                                                    Tolerance = 0.3),
                                                 ChosenName = case_when(IsEligible == FALSE ~ Harmonized,
                                                                        .default = Original),
                                                 Changed = !(Original == ChosenName))

                          # Re-assign names to current 'Table', possibly changing original feature names
                          names(Table) <- FeatureNames$ChosenName

                          # Obtain changed feature names for messaging
                          ChangedNames <- FeatureNames %>%
                                              filter(Changed == TRUE)

                          if (length(ChangedNames) == 0 || nrow(ChangedNames) == 0)
                          {
                              Messages$RawFeatureNames <- "Raw feature names left unchanged."

                          } else {

                              for (i in 1:nrow(ChangedNames))
                              {
                                  Message <- paste0("Changed feature name '", ChangedNames$Original[i], "' to '", ChangedNames$ChosenName[i], "'.")
                                  cli::cat_bullet(Message, bullet = "info")

                                  # Save messages in output object
                                  Messages$RawFeatureNames <- c(Messages$RawFeatureNames,
                                                                info = Message)
                              }
                          }

                          return(Table)
                       })


#-------------------------------------------------------------------------------
# Return
#-------------------------------------------------------------------------------
  return(RawDataSetSample)

}
