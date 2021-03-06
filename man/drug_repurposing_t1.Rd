% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drug_repurposing_t1.R
\name{drug_repurposing_t1}
\alias{drug_repurposing_t1}
\title{Calculate demographic table information for drug repurposing cohorts}
\usage{
drug_repurposing_t1(
  r_covariates,
  r_drugs,
  r_biomarkers,
  drug,
  phenotype,
  biomarker,
  source,
  concept_id_exclusions = c(2212451),
  exptid
)
}
\arguments{
\item{r_covariates}{A tibble, with columns c('person_id','dob','gender_concept_id','race_concept_id','drug_concept_id','first_drug_exposure','last_drug_exposure')}

\item{r_drugs}{A tibble, with columns c('person_id','start_date','end_date','drug_exposure_start_date','drug_concept_id')}

\item{r_biomarkers}{A tibble, with columns c('person_id','start_date','measurement_date','end_date','measurement_concept_id','value_as_number')}

\item{drug}{A string, e.g., "simvastatin"}

\item{phenotype}{A string, e.g., "Hyperlipidemia"}

\item{biomarker}{A string, e.g., "LDL Cholesterol"}

\item{source}{A string, e.g., "Vanderbilt"}

\item{concept_id_exclusions}{A vector, e.g., c(2212451)}

\item{exptid}{A numeric, e.g., 20210123061054}
}
\description{
Calculate demographic table information for drug repurposing cohorts
}
\details{
Last updated: 2021-03-27
}
