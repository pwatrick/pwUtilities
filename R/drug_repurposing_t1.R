#' Calculate demographic table information for drug repurposing cohorts
#'
#' @details
#' Last updated: 2021-03-27

#' @param r_drugs A tibble, with columns c('person_id','start_date','end_date','drug_exposure_start_date','drug_concept_id')
#' @param r_covariates A tibble, with columns c('person_id','dob','gender_concept_id','race_concept_id','drug_concept_id','first_drug_exposure','last_drug_exposure')
#' @param r_biomarkers A tibble, with columns c('person_id','start_date','measurement_date','end_date','measurement_concept_id','value_as_number')
#' @param drug A string, e.g., "simvastatin"
#' @param phenotype A string, e.g., "Hyperlipidemia"
#' @param biomarker A string, e.g., "LDL Cholesterol"
#' @param source A string, e.g., "Vanderbilt"
#' @param concept_id_exclusions A vector, e.g., c(2212451)
#' @param exptid A numeric, e.g., 20210123061054
#' @export

drug_repurposing_t1 <- function(r_covariates, r_drugs, r_biomarkers, drug, phenotype, biomarker, source, concept_id_exclusions = c(2212451), exptid) {

  if (phenotype == "Hyperlipidemia") {
    indication_drugs <- DrugRepurposingToolKit::drugsHyperlipidemia
  }
  if(phenotype == "Hypertension") {
    indication_drugs <- DrugRepurposingToolKit::drugsHypertension
  }

  demo_table <- pwUtilities::demographics_table(r_drugs, r_covariates, r_biomarkers, drug, phenotype, biomarker, indication_drugs, concept_id_exclusions, exptid)

  htn_t1 <- table1::table1(~ factor(is_f)+factor(is_w)+age+observation_period_length_days+treatment_period_length_days+drug | phenotype, data=demo_table, overall=F)

  return(htn_t1)
}
