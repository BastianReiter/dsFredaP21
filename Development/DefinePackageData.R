
library(dplyr)
library(readxl)
library(stringr)
library(usethis)


#===============================================================================
# Read in Package Data from xlsx-file
#===============================================================================

ExcelFilePath <- "./Development/Data/PackageData/PackageDataFredaP21.xlsx"

Sheetnames <- c("Meta.Tables",
                "Meta.Features",
                "Meta.Values",
                "Proc.EventFeatures",
                "Proc.TableNormalization",
                "Set.FeatureObligations",
                "Set.FeatureTracking",
                "Set.DataHarmonization",
                "Set.TransformativeExpressions",
                "Set.Dictionary",
                "Set.FuzzyStringMatching")

for (sheetname in Sheetnames)
{
    Table <- read_excel(path = ExcelFilePath,
                        sheet = sheetname,
                        skip = 2,
                        col_types = "text")

    ColumnTypes <- read_excel(path = ExcelFilePath,
                              sheet = sheetname,
                              range = cell_rows(2),
                              col_names = colnames(Table)) %>%
                      tidyr::pivot_longer(everything(),
                             names_to = "Column",
                             values_to = "Type")

    if (nrow(ColumnTypes) > 0)
    {
        for (i in 1:nrow(ColumnTypes))
        {
            Table <- Table %>%
                          mutate(across(all_of(ColumnTypes$Column[i]),
                                        ~ dsFreda::FormatData(.x, ColumnTypes$Type[i])))
        }
    }

    assign(x = sheetname,
           value = Table)

    # Save data in .rda-file and make it part of the package
    do.call(use_data, list(as.name(sheetname), overwrite = TRUE))
}


#===============================================================================
# Add EligibleValueSets for ADS tables
#===============================================================================

# Extract table and feature names from ADS tables
Meta.ADS <- ADS %>%
              imap(\(table, tablename) tibble(TableName = tablename,
                                              FeatureName = names(table))) %>%
              list_rbind()

# Get eligible values and labels from CDS features
CDSValues <- Meta.Values %>%
                  select(FeatureName.Curated,
                         ScaleLevel,
                         Value.Curated,
                         Label.Curated) %>%
                  rename(c("FeatureName" = "FeatureName.Curated",
                           "Value" = "Value.Curated",
                           "Label" = "Label.Curated")) %>%
                  mutate(HasEligibleValueSet = TRUE,
                         FeatureIsFromCDS = TRUE)

# Attach eligible values and labels to ADS features
Meta.ADS <- Meta.ADS %>%
                left_join(CDSValues,
                          by = join_by(FeatureName),
                          relationship = "many-to-many")

# Save data in .rda-file and make it part of package
use_data(Meta.ADS, overwrite = TRUE)



#===============================================================================
# Resource data from TinkerLab
#===============================================================================

Res.CancerGrouping <- TinkerLab::Res.CancerGrouping
use_data(Res.CancerGrouping, overwrite = TRUE)

Res.CancerSurgery <- TinkerLab::Res.CancerSurgery
use_data(Res.CancerSurgery, overwrite = TRUE)

Res.HIVCoding.Diseases <- TinkerLab::Res.HIVCoding.Diseases
use_data(Res.HIVCoding.Diseases, overwrite = TRUE)

Res.HIVCoding.Status <- TinkerLab::Res.HIVCoding.Status
use_data(Res.HIVCoding.Status, overwrite = TRUE)

Res.OPSCodes <- TinkerLab::Res.OPSCodes
use_data(Res.OPSCodes, overwrite = TRUE)

Res.P21.AdmissionCauses <- TinkerLab::Res.P21.AdmissionCauses
use_data(Res.P21.AdmissionCauses, overwrite = TRUE)

Res.P21.DischargeReasons <- TinkerLab::Res.P21.DischargeReasons
use_data(Res.P21.DischargeReasons, overwrite = TRUE)

Res.P21.Departments <- TinkerLab::Res.P21.Departments
use_data(Res.P21.Departments, overwrite = TRUE)



#===============================================================================
# Resource data from external package 'ICD10gm'
#===============================================================================

Res.ICD10Codes <- ICD10gm::icd_meta_codes %>%
                      select(year,
                             icd_code,
                             label) %>%
                      rename(c(ICDVersion = "year",
                               ICD10Code = "icd_code",
                               Diagnosis = "label")) %>%
                      mutate(ICDVersion = as.character(ICDVersion),
                             ICD10Code = str_remove_all(ICD10Code, "\\.-"))

use_data(Res.ICD10Codes, overwrite = TRUE)



#===============================================================================
# Privacy Settings
#===============================================================================

Set.Privacy <- list(Profile = "loose",     # Optional: 'strict', 'loose'
                    NThreshold = 5)

# Save data in .rda-file and make it part of package
use_data(Set.Privacy, overwrite = TRUE)


