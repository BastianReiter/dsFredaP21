

library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load P21 data as raw data set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RawDataSet <- readRDS(file = "./Development/Data/RealData/RawDataSet.rds")
#RawDataSetProc <- readRDS(file = "./Development/Data/RealData/RawDataSet_PreProcessed.rds")


RawDataSet <- PrepareRawDataSetDS(RawDataSetName.S = "RawDataSet",
                                  FeatureNameDictionary.S = list(Department = c(FAB = "Fachabteilung")))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check Tables for existence and completeness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSTableCheck <- GetDataSetCheckDS(DataSetName.S = "RawDataSet",
                                   Module.S = "P21",
                                   TransformationStage.S = "Raw")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OPTIONAL: Draw sample from Raw Data Set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# RawDataSet <- DrawSampleDS(RawDataSetName.S = "RawDataSet",
#                            SampleSize.S = "1000")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Curate data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CurationOutput <- CurateDataDS(RawDataSetName.S = "RawDataSet",
                               Settings.S = list(DataHarmonization = list(Run = TRUE,
                                                                          Profile = "Default"),
                                                 FeatureObligations = list(Profile = "Default"),
                                                 FeatureTracking = list(Profile = "Default"),
                                                 TableCleaning = list(Run = TRUE)))



CuratedDataSet <- CurationOutput$CuratedDataSet

CDSTableCheck <- GetDataSetCheckDS(DataSetName.S = "CuratedDataSet",
                                AssumeCCPDataSet.S = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Augment data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AugmentationOutput <- dsCCPhos::AugmentDataDS(CuratedDataSetName.S = "CuratedDataSet")

ADS <- AugmentationOutput$AugmentedDataSet

ADSTableCheck <- GetDataSetCheckDS(DataSetName.S = "ADS")



ADS_Patient <- ADS$Patient
ADS_Diagnosis <- ADS$Diagnosis


ADS_Patient <- ADS_Patient %>%
                    filter(CountDiagnoses == 1)


Analysis <- JoinTablesDS(TableNameA.S = "ADS_Patient",
                         TableNameB.S = "ADS_Diagnosis",
                         ByStatement.S = "PatientID")

#
#
# SampleStatistics <- GetSampleStatisticsDS(TableName.S = "ADS$Patients",
#                                           FeatureName.S = "LastVitalStatus")
# SampleStatistics$Statistics





Test <- Analysis %>%
            filter(str_starts(ICD10Code, "C34") == TRUE)



str_starts(Analysis$ICD10Code, "C50")








