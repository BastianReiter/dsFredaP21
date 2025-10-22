

#==============================================================================#
#------------------------------------------------------------------------------#
#   INPUT DATA Frankfurt: LOADING and HARMONIZATION                            #
#------------------------------------------------------------------------------#
#==============================================================================#


library(dplyr)
library(readr)
library(stringr)


# Source:
# ~~~~~~~
#     Datenintegrationszentrum Universitätsklinikum Frankfurt

# Export Method:
# ~~~~~~~~~~~~~~
#     SQL-Query --> CSV-Export


RDS.Case <- read.csv(file = "./Development/Data/RealData/Data_Fall.csv",
                     colClasses = "character",
                     check.names = FALSE) %>%
                mutate(Aufnahmedatum = as.Date(as.POSIXct(Aufnahmedatum, format = "%Y%m%d%H%M")),
                       Entlassungsdatum = as.Date(as.POSIXct(Entlassungsdatum, format = "%Y%m%d%H%M")),
                       Entlassungsgrund = str_sub(Entlassungsgrund, start = 1, end = 2))

RDS.Department <- read.csv(file = "./Development/Data/RealData/FAB.csv",
                           colClasses = "character",
                           check.names = FALSE) %>%
                      mutate(ID = 1:n(), .before = 1) %>%
                      mutate("FAB-Aufnahmedatum" = as.Date(as.POSIXct(.data[["FAB-Aufnahmedatum"]], format = "%Y%m%d%H%M")),
                             "FAB-Entlassungsdatum" = as.Date(as.POSIXct(.data[["FAB-Entlassungsdatum"]], format = "%Y%m%d%H%M")))

# Test <- RDS.Department %>%
#             mutate("FAB-Aufnahmedatum" = lubridate::parse_date_time(.data[["FAB-Aufnahmedatum"]], orders = c('YmdHM')))


RDS.DiagnosisICD <- read.csv(file = "./Development/Data/RealData/Data_ICD.csv",
                             colClasses = "character",
                             check.names = FALSE) %>%
                        mutate(ID = 1:n(), .before = 1)

RDS.Procedure <-  read.csv(file = "./Development/Data/RealData/Data_OPS.csv",
                           colClasses = "character",
                           check.names = FALSE) %>%
                      mutate(ID = 1:n(), .before = 1) %>%
                      mutate("OPS-Datum" = as.Date(as.POSIXct(.data[["OPS-Datum"]], format = "%Y%m%d%H%M")))


RawDataSet <- list(Case = RDS.Case,
                   Department = RDS.Department,
                   DiagnosisICD = RDS.DiagnosisICD,
                   Procedure = RDS.Procedure)

#saveRDS(RawDataSet, file = "./Development/Data/RealData/RawDataSet.rds")
saveRDS(RawDataSet, file = "./Development/Data/RealData/RawDataSet.rds")


