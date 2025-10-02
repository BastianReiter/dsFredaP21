
# --- DOCUMENTATION of package data ---


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_DataHarmonizationMethods.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Meta data on which methods to use for each feature during data harmonization
#'
#' A tibble
#'
#' @format ## `Meta_DataHarmonizationMethods`
#' \code{tibble}
#' \describe{
#'   \item{Profile}{}
#'   \item{FeatureID}{}
#'   \item{Table}{}
#'   \item{Feature}{}
#'   \item{HasEligibleValueSet}{}
#'   \item{TransformationRules}{}
#'   \item{HashTable}{}
#'   \item{FuzzyStringMatching}{}
#'   \item{Classification}{}
#' }
#' @source <https://github.com/BastianReiter/dsP21Curator/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_DataHarmonizationMethods"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_Departments.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Meta data on hospital department codes
#'
#' A tibble
#'
#' @format ## `Meta_Departments`
#' \code{tibble}
#' \describe{
#'   \item{DepartmentCode}{}
#'   \item{DepartmentOriginalLabel}{}
#'   \item{Department}{}
#'   \item{OperatingSpecialty}{}
#'   \item{Subspecialty}{}
#' }
#' @source <https://github.com/BastianReiter/dsP21Curator/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_Departments"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_Dictionary.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Dictionary data used in Data Harmonization
#'
#' A tibble containing look-up values and corresponding replacements
#'
#' @format ## `Meta_Dictionary`
#' \code{tibble}
#' \describe{
#'   \item{Profile}{}
#'   \item{FeatureID}{}
#'   \item{Table}{}
#'   \item{Feature}{}
#'   \item{LookupValue}{}
#'   \item{NewValue}{}
#'   \item{Comment}{}
#' }
#' @source <https://github.com/BastianReiter/dsP21Curator/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_Dictionary"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_DischargeReasons.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Meta data on discharge reason coding
#'
#' A tibble
#'
#' @format ## `Meta_DischargeReasons`
#' \code{tibble}
#' \describe{
#'   \item{DischargeReasonCode}{}
#'   \item{DischargeReasonOriginalLabel}{}
#'   \item{DischargeCategory}{}
#' }
#' @source <https://github.com/BastianReiter/dsP21Curator/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_DischargeReasons"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_EventFeatures.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Meta_EventFeatures
#'
#' Rules for engineering of informative features in event data
#'
#' @format ## `Meta_EventFeatures`
#' \code{tibble}
#' \describe{
#'   \item{Profile}{}
#'   \item{Feature}{}
#'   \item{Value}{}
#'   \item{ValueRank}{}
#'   \item{EvaluationOrder}{}
#'   \item{Condition}{}
#' }
#' @source <https://github.com/BastianReiter/dsP21Curator/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_EventFeatures"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_Features.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Meta data: Features
#'
#' A tibble containing meta data about corresponding feature names in Raw Data Model (RDM) and Curated Data Model (CDM)
#'
#' @format ## `Meta_Features`
#' \code{tibble}
#' \describe{
#'   \item{FeatureID}{}
#'   \item{TableName_Raw}{Name of table in Opal}
#'   \item{TableName_Curated}{Name of table after loading into R session}
#'   \item{FeaturePosition}{Position of Feature in Table}
#'   \item{FeatureName_Raw}{Feature name in Raw Data Model}
#'   \item{FeatureName_Curated}{Corresponding feature name in Curated Data Model}
#'   \item{IsPrimaryKey}{Indicating whether feature serves as primary key for corresponding table}
#'   \item{Type}{Data type}
#'   \item{Scale}{Scale of measure}
#'   \item{HasEligibleValueSet}{Indicating whether values of feature are part of a finite, discrete eligible value set}
#'   \item{IsDiscriminatory}{Indicating whether feature is used to strictly discriminate between different entries. Used for discrimination of table entries in redundancy classification.}
#'   \item{IsEssential}{Indicating whether feature holds essential information. Used for discrimination of table entries in redundancy classification.}
#' }
#' @source <https://github.com/BastianReiter/dsP21Curator/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_Features"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_FeatureObligations.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Containing preferences on which features in CDS should be considered obligatory
#'
#' A tibble
#'
#' @format ## `Meta_FeatureObligations`
#' \code{tibble}
#' \describe{
#'   \item{Table}{}
#'   \item{Feature}{}
#'   \item{Default}{}
#' }
#' @source <https://github.com/BastianReiter/dsP21Curator/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_FeatureObligations"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_FeatureTracking.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Containing preferences on which features should be tracked/monitored during curation process
#'
#' A tibble
#'
#' @format ## `Meta_FeatureTracking`
#' \code{tibble}
#' \describe{
#'   \item{Table}{}
#'   \item{Feature}{}
#'   \item{HasEligibleValueSet}{Indicating whether values of feature are part of a finite, discrete eligible value set}
#'   \item{Default}{}
#' }
#' @source <https://github.com/BastianReiter/dsP21Curator/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_FeatureTracking"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_FuzzyStringMatching.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Meta_FuzzyStringMatching
#'
#' Feature-specific settings for Fuzzy String Matching
#'
#' @format ## `Meta_FuzzyStringMatching`
#' \code{tibble}
#' \describe{
#'   \item{Profile}{}
#'   \item{FeatureID}{}
#'   \item{Table}{}
#'   \item{Feature}{}
#'   \item{HasEligibleValueSet}{}
#'   \item{PreferredMethod}{}
#'   \item{FindBestMethod}{}
#'   \item{Tolerance}{}
#'   \item{Preprocessing.FlattenCase}{}
#'   \item{Preprocessing.RemoveAllWhiteSpace}{}
#'   \item{Preprocessing.SquishWhiteSpace}{}
#'   \item{Stringdist.useBytes}{}
#'   \item{Stringdist.weight.d}{}
#'   \item{Stringdist.weight.i}{}
#'   \item{Stringdist.weight.s}{}
#'   \item{Stringdist.weight.t}{}
#'   \item{Stringdist.q}{}
#'   \item{Stringdist.p}{}
#'   \item{Stringdist.bt}{}
#' }
#' @source <https://github.com/BastianReiter/dsP21Curator/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_FuzzyStringMatching"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_Tables.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Meta Data: Tables
#'
#' A tibble containing meta data about corresponding table names in Raw Data Model (RDM) and Curated Data Model (CDM)
#'
#' @format ## `Meta_Tables`
#' \code{tibble}
#' \describe{
#'   \item{TableName_Raw}{Table name in Raw Data Model}
#'   \item{TableName_Curated}{Corresponding table name in Curated Data Model}
#' }
#' @source <https://github.com/BastianReiter/dsP21Curator/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_Tables"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_TableNormalization.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Containing preferences on table normalization operations
#'
#' A tibble
#'
#' @format ## `Meta_TableNormalization`
#' \code{tibble}
#' \describe{
#'   \item{Profile}{}
#'   \item{FeatureID}{}
#'   \item{Table}{}
#'   \item{Feature}{}
#'   \item{EvaluationOrder}{}
#'   \item{Operation}{}
#'   \item{Comment}{}
#' }
#' @source <https://github.com/BastianReiter/dsP21Curator/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_TableNormalization"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_TransformativeExpressions.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Transformative expressions used in Data Harmonization
#'
#' A tibble
#'
#' @format ## `Meta_TransformativeExpressions`
#' \code{tibble}
#' \describe{
#'   \item{Profile}{}
#'   \item{FeatureID}{}
#'   \item{Table}{}
#'   \item{Feature}{}
#'   \item{EvaluationOrder}{}
#'   \item{Expression}{}
#'   \item{Comment}{}
#' }
#' @source <https://github.com/BastianReiter/dsP21Curator/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_TransformativeExpressions"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_Values.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Meta Data: Values
#'
#' A tibble containing meta data about data values
#'
#' @format ## `Meta_Values`
#' \code{tibble}
#' \describe{
#'   \item{FeatureID}{}
#'   \item{Table}{Table name}
#'   \item{Feature}{Feature name}
#'   \item{ScaleLevel}{Scale level of feature}
#'   \item{Value_Raw}{Value in original / raw data}
#'   \item{Value_Curated}{Value as preferred}
#'   \item{Label_Curated}{Label for coded feature values}
#'   \item{Label_Raw}{Label in original data}
#'   \item{FactorRank}{Used to determine order in values}
#'   \item{ComparatorCode}{Assignment of numeric value to certain non-numeric values to enable comparison operations}
#' }
#' @source <https://github.com/BastianReiter/dsP21Curator/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_Values"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DisclosureSettings.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Disclosure settings
#'
#' A \code{list} containing settings concerning data privacy and disclosure mitigation
#'
#' @format ## `DisclosureSettings`
#' \code{list}
#' \describe{
#'    \item{Profile}{Can be 'strict' or 'loose'}
#'    \item{NThreshold}{The minimum sample size required for transmission of aggregated data to client}
#' }
#' @source <https://github.com/BastianReiter/dsP21Curator/blob/main/Development/MetaData>
#' @author Bastian Reiter
"DisclosureSettings"
