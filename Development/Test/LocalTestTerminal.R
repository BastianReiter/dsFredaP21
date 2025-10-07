

library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load P21 data as raw data set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

P21.RawDataSet <- readRDS(file = "./Development/Data/RealData/RawDataSet.rds")
#RawDataSetProc <- readRDS(file = "./Development/Data/RealData/RawDataSet_PreProcessed.rds")


P21.RawDataSet <- PrepareRawDataSetDS(RawDataSetName.S = "P21.RawDataSet",
                                      FeatureNameDictionary.S = list(Department = c(FAB = "Fachabteilung")))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check Tables for existence and completeness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSTableCheck <- GetDataSetCheckDS(DataSetName.S = "P21.RawDataSet",
                                   Module.S = "P21",
                                   Stage.S = "Raw")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OPTIONAL: Draw sample from Raw Data Set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

P21.RawDataSet <- DrawSampleDS(RawDataSetName.S = "P21.RawDataSet",
                               SampleSize.S = 5000)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Curate data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CurationOutput <- P21.CurateDataDS(RawDataSetName.S = "P21.RawDataSet",
                                   Settings.S = list(DataHarmonization = list(Run = TRUE,
                                                                              Profile = "Default"),
                                                     FeatureObligations = list(Profile = "Default"),
                                                     FeatureTracking = list(Profile = "Default"),
                                                     TableCleaning = list(Run = TRUE)))

P21.CuratedDataSet <- CurationOutput$CuratedDataSet

CDSTableCheck <- GetDataSetCheckDS(DataSetName.S = "P21.CuratedDataSet",
                                   Module.S = "P21",
                                   Stage.S = "Curated")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Augment data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AugmentationOutput <- dsFredaP21::AugmentDataDS(CuratedDataSetName.S = "P21.CuratedDataSet")

ADS <- AugmentationOutput$AugmentedDataSet

ADSTableCheck <- GetDataSetCheckDS(DataSetName.S = "ADS")










