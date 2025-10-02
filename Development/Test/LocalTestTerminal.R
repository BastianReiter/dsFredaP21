

library(dsP21Curator)
library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load P21 data as raw data set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#RawDataSet <- readRDS(file = "./Development/Data/RealData/RawDataSet.rds")
RawDataSet <- readRDS(file = "./Development/Data/RealData/RawDataSet_PreProcessed.rds")





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rename tables of RawDataSet (the names are also changed when tables are being loaded into R server sessions)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# vc_Lookup <- paste0("RDS_", dsCCPhos::Meta_Tables$TableName_Curated)
# names(vc_Lookup) <- dsCCPhos::Meta_Tables$TableName_Raw
#
# names(RawDataSet) <- sapply(names(RawDataSet),
#                             function(TableName) { vc_Lookup[TableName] })


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check Tables for existence and completeness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSTableCheck <- GetDataSetCheckDS(DataSetName.S = "RawDataSet",
                                   AssumeCCPDataSet.S = TRUE)
                                   # RequiredTableNames.S = paste0("RDS_", dsCCPhos::Meta_Tables$TableName_Curated),
                                   # RequiredFeatureNames.S = RequiredTableNames.S %>%
                                   #                              map(\(tablename) filter(dsCCPhos::Meta_Features, TableName_Curated == str_remove(tablename, "RDS_"))$FeatureName_Raw) %>%
                                   #                              set_names(RequiredTableNames.S))


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








