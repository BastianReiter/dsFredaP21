
#' P21.PrepareRawDataSetDS
#'
#' Try to harmonize feature names in RDS tables (prior to data curation) employing Fuzzy String Matching and Dictionary look-up.
#'
#' Server-side ASSIGN method
#'
#' @param RawDataSetName.S \code{string} - Name of Raw Data Set object (list) on server - Default: 'P21.RawDataSet'
#' @param FeatureNameDictionary.S Optional \code{list} containing dictionary data for feature name harmonization (Form: \code{list(Department = c(FAB = "Fachabteilung"))})
#'
#' @return A \code{list} containing
#'          \itemize{ \item The input RawDataSet (\code{list}) with harmonized feature names
#'                    \item Messages (\code{character vector}) }
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
P21.PrepareRawDataSetDS <- function(RawDataSetName.S = "P21.RawDataSet",
                                    FeatureNameDictionary.S = list())
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes
  # RawDataSetName.S <- "P21.RawDataSet"
  # FeatureNameDictionary.S <- list(Department = c(FAB = "Fachabteilung"))

  # --- Argument Validation ---
  assert_that(is.string(RawDataSetName.S),
              is.list(FeatureNameDictionary.S))

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  RawDataSet <- eval(parse(text = RawDataSetName.S), envir = parent.frame())
  # Save a 'backup' of the input data set (this will be part of output so changes to feature names can be tracked)
  OriginalRawDataSet <- RawDataSet

#-------------------------------------------------------------------------------

  # Initiate output messaging object
  Messages <- list(RawFeatureNames = character())

# Try to harmonize raw feature names using fuzzy string matching and dictionary data
#-------------------------------------------------------------------------------
  Preparation <- RawDataSet %>%
                    imap(function(Table, tablename)
                         {
                            # Initiate messaging vector for current 'Table'
                            CurrentMessages <- character()

                            # Get expected raw feature names
                            EligibleFeatureNames <- dsFredaP21::Meta.Features %>%
                                                        filter(TableName.Curated == tablename) %>%
                                                        pull(FeatureName.Raw)

                            # Get Dictionary (character vector) from passed list
                            FeatureNameDictionary <- FeatureNameDictionary.S[[tablename]]

                            # First, try Fuzzy String Matching (also matching to Dictionary look up values if possible, because these will subsequentially turn into eligible feature names)
                            HarmonizedFeatureNames <- dsFreda::GetFuzzyStringMatches(Vector = names(Table),
                                                                                     EligibleStrings = c(EligibleFeatureNames, names(FeatureNameDictionary)),
                                                                                     PreferredMethod = "jw",
                                                                                     Tolerance = 0.2)

                            # Try Dictionary Look-up, if dictionary data is passed
                            if (length(FeatureNameDictionary) > 0)
                            {
                                HarmonizedFeatureNames <- if_else(is.na(FeatureNameDictionary[HarmonizedFeatureNames]),
                                                                  HarmonizedFeatureNames,
                                                                  FeatureNameDictionary[HarmonizedFeatureNames])
                            }

                            # Create tibble containing transformation tracks of all feature names
                            FeatureNames <- tibble(Original = names(Table),
                                                   IsEligible.Original = (Original %in% EligibleFeatureNames),
                                                   Harmonized = HarmonizedFeatureNames,
                                                   ChosenName = case_when(IsEligible.Original == FALSE ~ Harmonized,
                                                                          .default = Original),
                                                   IsEligible.ChosenName = (ChosenName %in% EligibleFeatureNames),
                                                   Changed = !(Original == ChosenName))

                            # Re-assign names to current 'Table', possibly changing original feature names
                            names(Table) <- FeatureNames$ChosenName

                            # Obtain changed feature names for messaging
                            ChangedNames <- FeatureNames %>%
                                                filter(Changed == TRUE)

                            if (length(ChangedNames) == 0 || nrow(ChangedNames) == 0)
                            {
                                Message <- paste0("Table '", tablename, "': No changes to raw feature names.")
                                cli::cat_bullet(Message, bullet = "info")
                                CurrentMessages <- c(CurrentMessages,
                                                     Info = Message)

                            } else {

                                for (i in 1:nrow(ChangedNames))
                                {
                                    Message <- paste0("Table '", tablename, "': Changed feature name '", ChangedNames$Original[i], "' to '", ChangedNames$ChosenName[i], "'.")
                                    cli::cat_bullet(Message, bullet = "info")
                                    CurrentMessages <- c(CurrentMessages,
                                                         Info = Message)
                                }
                            }

                            # Obtain remaining ineligible feature names
                            RemainingIneligibleNames <- FeatureNames %>%
                                                            filter(IsEligible.ChosenName == FALSE)

                            if (length(RemainingIneligibleNames) > 0 && nrow(RemainingIneligibleNames) > 0)
                            {
                                for (i in 1:nrow(ChangedNames))
                                {
                                    Message <- paste0("Table '", tablename, "': The feature name '", RemainingIneligibleNames$Original[i], "' is ineligible and could not be harmonized!")
                                    cli::cat_bullet(Message, bullet = "warning", bullet_col = "red")
                                    CurrentMessages <- c(CurrentMessages,
                                                         Warning = Message)
                                }
                            }

                            return(list(Table = Table,
                                        Messages = CurrentMessages))
                         })

  # Extract RawDataSet and Messages
  RawDataSet <- Preparation %>% map("Table")
  Messages <- Preparation %>%
                  map("Messages") %>%
                  unlist() %>%
                  set_names(sub(".*\\.", "", names(.)))

#-------------------------------------------------------------------------------
  return(list(RawDataSet = RawDataSet,
              OriginalRawDataSet = OriginalRawDataSet,
              Messages = Messages))
}
