#' Clean DBI output and save as RDA file
#' Last updated: 2021-03-08
#'
#' @param r_drugs A tibble,
#' @param r_covariates A tibble
#' @param r_biomarkers A tibble
#' @export

clean_save_dbi_output <- function(r_covariates, r_drugs, r_biomarkers) {

  names(r_covariates) <- tolower(names(r_covariates))
  names(r_drugs) <- tolower(names(r_drugs))
  names(r_biomarkers) <- tolower(names(r_biomarkers))

  r_biomarkers <- r_biomarkers %>%
    mutate(
      start_date = as_date(start_date),
      measurement_date = as_date(measurement_date),
      end_date = as_date(end_date)
    )
  r_drugs <- r_drugs %>%
    mutate(
      start_date = as_date(start_date),
      end_date = as_date(end_date),
      drug_exposure_start_date = as_date(drug_exposure_start_date)
    )
  r_covariates <- r_covariates %>%
    mutate(
      dob = as_date(dob),
      first_drug_exposure = as_date(first_drug_exposure),
      last_drug_exposure = as_date(last_drug_exposure)
    )
  r_list <- list(r_covariates, r_drugs, r_biomarkers)
  names(r_list) <- c("r_c", "r_d", "r_b")
  return(r_list)
}
