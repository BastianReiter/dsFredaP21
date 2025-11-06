

library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load P21 data as raw data set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

P21.RawDataSet <- readRDS(file = "./Development/Data/RealData/RawDataSet.rds")
#RawDataSetProc <- readRDS(file = "./Development/Data/RealData/RawDataSet_PreProcessed.rds")


RDSPreparation <- dsFreda::PrepareRawDataDS(RawDataSetName.S = "P21.RawDataSet",
                                            Module.S = "P21",
                                            FeatureNameDictionary.S = list(Department = c(FAB = "Fachabteilung")),
                                            RunFuzzyStringMatching.S = TRUE,
                                            CurateFeatureNames.S = TRUE)

P21.RawDataSet <- RDSPreparation$RawDataSet

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check Tables for existence and completeness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSTableCheck <- dsFreda::GetDataSetCheckDS(DataSetName.S = "P21.RawDataSet",
                                            Module.S = "P21",
                                            Stage.S = "Raw")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OPTIONAL: Draw sample from Raw Data Set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

P21.RawDataSet <- P21.DrawSampleDS(RawDataSetName.S = "P21.RawDataSet",
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

AugmentationOutput <- P21.AugmentDataDS(CuratedDataSetName.S = "P21.CuratedDataSet")

ADS <- AugmentationOutput$AugmentedDataSet

ADSTableCheck <- GetDataSetCheckDS(DataSetName.S = "ADS",
                                   Module.S = "P21",
                                   Stage.S = "Augmented")

# saveRDS(ADS, file = "P21_TestADS.rds")
#
# encryptr::genkeys()
#
# encryptr::encrypt_file(.path = "P21_TestADS.rds",
#                        crypt_file_name = "P21_TestADS.rds.encryptr.bin",
#                        public_key_path = "id_rsa.pub")
#
# encryptr::decrypt_file(.path = "P21_TestADS.rds.encryptr.bin",
#                        file_name = "P21_TestADS.rds",
#                        private_key_path = "id_rsa")










