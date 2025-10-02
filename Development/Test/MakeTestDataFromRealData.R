

#==============================================================================#
#------------------------------------------------------------------------------#
#   INPUT DATA Frankfurt: LOADING and HARMONIZATION                            #
#------------------------------------------------------------------------------#
#==============================================================================#


library(readr)


# Source:
# ~~~~~~~
#     Datenintegrationszentrum Universitätsklinikum Frankfurt

# Export Method:
# ~~~~~~~~~~~~~~
#     SQL-Query --> CSV-Export

# Exported files:
# ~~~~~~~~~~~~~~~
#     - data_Fall.csv
#     - data_FAB.csv
#     - data_ICD.csv
#     - data_OPS.csv

InputDataPath <- "./Development/Data/RealData"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Input Data Model (IDM)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_IDM_Cases
# df_IDM_CasesDepartment
# df_IDM_CasesICD
# df_IDM_CasesOPS

# ==> For details, see Project Documentation -> Harmonized Input Data Model



#===============================================================================
# df_IDM_Cases: Patient- and Case-related data
#===============================================================================

RDS_Case <- read.csv(file = "./Development/Data/RealData/Data_Fall.csv",
                     colClasses = "character") %>%
                mutate(Aufnahmedatum = as.Date(as.POSIXct(Aufnahmedatum, format = "%Y%m%d%H%M")),
                       Entlassungsdatum = as.Date(as.POSIXct(Entlassungsdatum, format = "%Y%m%d%H%M")),
                       Entlassungsgrund = str_sub(Entlassungsgrund, start = 1, end = 2))


RDS_Department <- read.csv(file = "./Development/Data/RealData/Data_FAB.csv",
                           colClasses = "character") %>%
                      mutate(ID = 1:n(), .before = 1) %>%
                      mutate(FAB.Aufnahmedatum = as.Date(as.POSIXct(FAB.Aufnahmedatum, format = "%Y%m%d%H%M")),
                             FAB.Entlassungsdatum = as.Date(as.POSIXct(FAB.Entlassungsdatum, format = "%Y%m%d%H%M")))

RDS_DiagnosisICD <- read.csv(file = "./Development/Data/RealData/Data_ICD.csv",
                             colClasses = "character") %>%
                        mutate(ID = 1:n(), .before = 1)

RDS_Procedure <-  read.csv(file = "./Development/Data/RealData/Data_OPS.csv",
                           colClasses = "character") %>%
                      mutate(ID = 1:n(), .before = 1) %>%
                      mutate(OPS.Datum = as.Date(as.POSIXct(OPS.Datum, format = "%Y%m%d%H%M")))


RawDataSet <- list(RDS_Case = RDS_Case,
                   RDS_Department = RDS_Department,
                   RDS_DiagnosisICD = RDS_DiagnosisICD,
                   RDS_Procedure = RDS_Procedure)

#saveRDS(RawDataSet, file = "./Development/Data/RealData/RawDataSet.rds")
saveRDS(RawDataSet, file = "./Development/Data/RealData/RawDataSet_PreProcessed.rds")


