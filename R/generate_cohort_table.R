#' For drug repurposing EHR validation studies, generates demographics table
#' Last updated: 2020-12-25
#'
#' @param t_phenotype_variables A tibble
#' @param cohort_drug A string
#' @param drug_cohort_metadata A tibble
#' @param col_vector A vector
#' @export

generate_cohort_table <- function(t_phenotypes_variables, cohort_drug, drug_cohort_metadata, col_vector) {

  drug_variables <- t_phenotype_variables %>%
    filter(drug_name == cohort_drug)

  #Import covariates data
  ##Create variables
  covariates_file <- drug_variables$covariates_file[[1]]
  drug_name <- drug_variables$drug_name[[1]]
  exptid <- drug_variables$expt_id[[1]]
  cohort_type <- "cohort1"
  covariates <- vroom(glue(covariates_file), .name_repair = janitor::make_clean_names)

  ##Process covariates data
  ###Keep only patients in cohort1
  covariates <- covariates %>%
    filter(person_id %in% drug_cohort_metadata$person_id)
  ###Convert binary variables
  covariates$is_f <- if_else(covariates$gender == "F", 1, 0)
  covariates$is_w <- if_else(covariates$race == "W", 1, 0)
  ###Age
  covariates$age <- interval(covariates$dob, covariates$start_date)
  covariates$age <- time_length(covariates$age, "year")
  ###Calculate total observation period length
  covariates$obs_length <- interval(covariates$start_date, covariates$end_date)
  covariates$obs_length <- time_length(covariates$obs_length, "day")
  ###Calculate treatment period length
  covariates <- covariates %>%
    mutate(
      treatment_length = time_length(interval(.data[[col_vector[[1]]]], .data[[col_vector[[2]]]]), "day")
    )
  ###Keep columns relevant for characteristics table
  covariates <- covariates %>%
    select(person_id, is_f, is_w, age, obs_length, treatment_length)
  summary(covariates$obs_length)
  ##Incorporate cohort information
  c1_patients <- drug_cohort_metadata %>%
    filter(cohort1 == 1) %>%
    select(person_id)
  c1_table <- covariates %>%
    filter(person_id %in% c1_patients$person_id) %>%
    mutate(
      cohort = "entire_cohort"
    )

  c2_patients <- drug_cohort_metadata %>%
    filter(cohort2 == 1) %>%
    select(person_id)
  c2_table <- covariates %>%
    filter(person_id %in% c2_patients$person_id) %>%
    mutate(
      cohort = "subgroup3"
    )

  c3_patients <- drug_cohort_metadata %>%
    filter(cohort3 == 1) %>%
    select(person_id)
  c3_table <- covariates %>%
    filter(person_id %in% c3_patients$person_id) %>%
    mutate(
      cohort = "subgroup1"
    )

  c4_patients <- drug_cohort_metadata %>%
    filter(cohort4 == 1) %>%
    select(person_id)
  c4_table <- covariates %>%
    filter(person_id %in% c4_patients$person_id) %>%
    mutate(
      cohort = "subgroup2"
    )

  merged_cohort_table <- bind_rows(c1_table, c3_table, c4_table, c2_table)

  return(merged_cohort_table)
}
