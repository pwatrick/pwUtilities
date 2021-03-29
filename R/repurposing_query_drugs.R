#' Function to create SQL query for drugs data
#' Last updated: 2021-02-15
#'
#' @param drug_concept_id A string
#' @param biomarker_concept_id A string
#' @param db A string, either "netezza" or "bigquery"
#' @export

repurposing_query_drugs <- function(drug_concept_id, biomarker_concept_id, db = "netezza",...) {

  if (db == "netezza") {
    #Table variables
    drug_exposure <- "v_drug_exposure"
    concept <- "v_concept"
    concept_ancestor <- "v_concept_ancestor"
    drug_concept_id <- drug_concept_id
    biomarker_concept_id <- biomarker_concept_id
    visit_occurrence <- "v_visit_occurrence"
    start_date <- "add_months(s1.first_drug_exposure, -12) as start_date"
    end_date <- "add_months(s1.first_drug_exposure, 12) as end_date"
    measurement <- "v_measurement"
    person <- "v_person"
  }

  if (db == "bigquery") {
    #Table variables
    drug_exposure <- "`drug_exposure`"
    concept <- "`concept`"
    concept_ancestor <- "`concept_ancestor`"
    drug_concept_id <- drug_concept_id
    biomarker_concept_id <- biomarker_concept_id
    visit_occurrence <- "`visit_occurrence`"
    start_date <- "DATE_SUB(s1.first_drug_exposure, INTERVAL 12 MONTH) as start_date"
    end_date <- "DATE_ADD(s1.first_drug_exposure, INTERVAL 12 MONTH) as end_date"
    measurement <- "`measurement`"
    person <- "`person`"
  }

  #codesets
  codesets <- glue::glue("
  WITH pw_codesets AS
  (
    SELECT s3.codeset_id, s3.concept_id, s3.concept_name
    FROM (

    (SELECT 1 as codeset_id, s1.concept_id, s1.concept_name
      FROM (
        SELECT t1.concept_id, t1.concept_name
        FROM (SELECT * FROM {concept} WHERE domain_id = 'Drug') t1
        INNER JOIN (SELECT * FROM {concept_ancestor} WHERE ancestor_concept_id = {drug_concept_id}) t2
        ON (t1.concept_id = t2.DESCENDANT_CONCEPT_ID)
        GROUP BY t1.concept_id, t1.concept_name
        ) s1)

    UNION DISTINCT

    (SELECT 2 as codeset_id, s2.concept_id, s2.concept_name
      FROM (
        SELECT t1.concept_id, t1.concept_name
        FROM (SELECT * FROM {concept} WHERE domain_id = 'Measurement') t1
        INNER JOIN (SELECT * FROM {concept_ancestor} WHERE ancestor_concept_id = {biomarker_concept_id}) t2
        ON (t1.concept_id = t2.DESCENDANT_CONCEPT_ID)
        GROUP BY t1.concept_id, t1.concept_name
        ) s2)) s3
        ),
                         ")

  #Index date
  index_date <- glue::glue("
  pw_index_date AS
  (
    SELECT s1.person_id,
           {drug_concept_id} as drug_concept_id,
           s1.drug_exposure_start_date
    FROM (
      (SELECT s2.person_id, s2.drug_concept_id, s3.concept_name as drug_concept_name, s2.drug_exposure_start_date
        FROM (SELECT * FROM {drug_exposure}) s2
        INNER JOIN (SELECT concept_id, concept_name FROM pw_codesets WHERE codeset_id = 1) s3
        ON (s2.drug_concept_id = s3.concept_id))
        ) s1
        ),
                           ")

  #First and last lisinopril exposure dates
  pw_obsperiod <- glue::glue("
  pw_obsperiod AS
  (
    SELECT s1.person_id,
           s1.drug_concept_id,
           min(s1.drug_exposure_start_date) as first_drug_exposure,
           max(s1.drug_exposure_start_date) as last_drug_exposure
    FROM (
      (SELECT s2.person_id, s2.drug_concept_id, s2.drug_exposure_start_date
      FROM (SELECT * FROM pw_index_date) s2
      INNER JOIN (SELECT * FROM {visit_occurrence} WHERE visit_concept_id = 9202) s3
      ON (s2.person_id = s3.person_id AND s2.drug_exposure_start_date = s3.visit_start_date))
    ) s1
    GROUP BY s1.person_id, s1.drug_concept_id
    ),
                             ")

  pw_start_end <- glue::glue("
  pw_start_end AS
  (
    SELECT s1.person_id, s1.drug_concept_id, {start_date}, {end_date}
    FROM (SELECT * FROM pw_obsperiod) s1
  ),

  ")

    pw_drugs <- glue::glue("
    pw_drugs AS
    (
    SELECT s1.person_id, s1.start_date, s1.end_date, s1.drug_concept_id, s1.drug_exposure_start_date
    FROM (
      (SELECT s2.person_id, s2.start_date, s2.end_date, s3.drug_concept_id, s3.drug_exposure_start_date
      FROM (SELECT * FROM pw_start_end) s2
      INNER JOIN (SELECT person_id, drug_concept_id, drug_exposure_start_date
                  FROM {drug_exposure}
                  WHERE drug_concept_id != 0) s3
      ON (s2.person_id = s3.person_id))
    ) s1
    WHERE s1.drug_exposure_start_date >= s1.start_date
    AND s1.drug_exposure_start_date <= s1.end_date
    ),
                           ")

  #Filter for only outpatient drug exposure
  pw_drugs_out <- glue::glue("
  pw_drugs_out AS
  (
  SELECT s1.person_id, s1.start_date, s1.end_date, s1.drug_concept_id, s1.drug_exposure_start_date
  FROM (
    (SELECT s2.person_id, s2.start_date, s2.end_date, s2.drug_concept_id, s2.drug_exposure_start_date
    FROM (SELECT * FROM pw_drugs) s2
    INNER JOIN (SELECT * FROM {visit_occurrence} WHERE visit_concept_id = 9202) s3
    ON (s2.person_id = s3.person_id AND s2.drug_exposure_start_date = s3.visit_start_date))
  ) s1
  GROUP BY s1.person_id, s1.start_date, s1.end_date, s1.drug_concept_id, s1.drug_exposure_start_date
  )
                             ")

  drugs_select_query <- glue::glue("
  SELECT po.person_id,
         po.start_date,
         po.end_date,
         po.drug_exposure_start_date,
         po.drug_concept_id
   FROM (SELECT * FROM pw_drugs_out) po
                                   ")

  drugs_query <- paste(codesets, index_date, pw_obsperiod, pw_start_end, pw_drugs, pw_drugs_out, drugs_select_query, sep = "\n")

  return(drugs_query)

}
