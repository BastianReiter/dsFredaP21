
library(dplyr)
library(readxl)
library(usethis)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in Package Data from xlsx-file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Resource data from TinkerLab
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Res.DischargeReasons <- TinkerLab::Res.P21DischargeReasons
use_data(Res.DischargeReasons, overwrite = TRUE)

Res.Departments <- TinkerLab::Res.P21Departments
use_data(Res.Departments, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Disclosure Settings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DisclosureSettings.FredaP21 <- list(Profile = "loose",     # Optional: 'strict', 'loose'
                                    NThreshold = 5)

# Save data in .rda-file and make it part of package
use_data(DisclosureSettings.FredaP21, overwrite = TRUE)


