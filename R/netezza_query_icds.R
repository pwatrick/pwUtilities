#' Function to get ICD data for calculating elixhauser scores
#' Updated: 2021-10-01
#' @param drug_concept_id A string
#' @param biomarker_concept_id A string
#' @param table_name A string
#' @param table_type A string, either c("covariates", "drugs", "biomarkers", "icds")
#' @param ancestor_table A string, eg, "antihypertensive_drugs_ancestor_tbl_20210308151236"
#' @param conn A variable
#' @export
netezza_query_icds <- function(drug_concept_id, biomarker_concept_id, table_name, table_type, ancestor_table, conn,...) {

  sql_query <- glue::glue("
    (
    WITH pw_codesets AS
    (
      SELECT s3.codeset_id, s3.concept_id, s3.concept_name
      FROM (

      (SELECT 1 as codeset_id, s1.concept_id, s1.concept_name
        FROM (
          SELECT t1.concept_id, t1.concept_name
          FROM (SELECT * FROM v_concept WHERE domain_id = 'Drug') t1
          INNER JOIN (SELECT * FROM v_concept_ancestor WHERE ancestor_concept_id = {drug_concept_id}) t2
          ON (t1.concept_id = t2.DESCENDANT_CONCEPT_ID)
          GROUP BY t1.concept_id, t1.concept_name
          ) s1)

      UNION DISTINCT

      (SELECT 2 as codeset_id, s2.concept_id, s2.concept_name
        FROM (
          SELECT t1.concept_id, t1.concept_name
          FROM (SELECT * FROM v_concept WHERE domain_id = 'Measurement') t1
          INNER JOIN (SELECT * FROM v_concept_ancestor WHERE ancestor_concept_id = {biomarker_concept_id}) t2
          ON (t1.concept_id = t2.DESCENDANT_CONCEPT_ID)
          GROUP BY t1.concept_id, t1.concept_name
          ) s2)) s3
          ),
    pw_index_date AS
    (
      SELECT s1.person_id,
             {drug_concept_id} as drug_concept_id,
             s1.drug_exposure_start_date
      FROM (
        (SELECT s2.person_id, s2.drug_concept_id, s3.concept_name as drug_concept_name, s2.drug_exposure_start_date
          FROM (SELECT * FROM v_drug_exposure) s2
          INNER JOIN (SELECT concept_id, concept_name FROM pw_codesets WHERE codeset_id = 1) s3
          ON (s2.drug_concept_id = s3.concept_id))
          ) s1
          ),
    pw_obsperiod AS
    (
      SELECT s1.person_id,
             s1.drug_concept_id,
             min(s1.drug_exposure_start_date) as first_drug_exposure,
             max(s1.drug_exposure_start_date) as last_drug_exposure
      FROM (
        (SELECT s2.person_id, s2.drug_concept_id, s2.drug_exposure_start_date
        FROM (SELECT * FROM pw_index_date) s2
        INNER JOIN (SELECT * FROM v_visit_occurrence WHERE visit_concept_id = 9202) s3
        ON (s2.person_id = s3.person_id AND s2.drug_exposure_start_date = s3.visit_start_date))
      ) s1
      GROUP BY s1.person_id, s1.drug_concept_id
      ),
    pw_start_end AS
    (
      SELECT s1.person_id,
             s1.drug_concept_id,
             add_months(s1.first_drug_exposure, -12) as start_date,
             add_months(s1.first_drug_exposure, 12) as end_date
      FROM (SELECT * FROM pw_obsperiod) s1
    ),
    pw_icds AS
    (
      SELECT s1.person_id, s2.concept_code, s1.condition_date, s2.vocabulary_id
      FROM (SELECT person_id,
                   condition_source_concept_id,
                   condition_source_value,
                   condition_start_date as condition_date
           FROM V_CONDITION_OCCURRENCE
           ) s1
      INNER JOIN (SELECT * FROM pw_elixhauser_codes) s2
      ON (s1.condition_source_concept_id = s2.concept_id)
      GROUP BY s1.person_id, s2.concept_code, s1.condition_date, s2.vocabulary_id
    ),
    pw_icds_selected AS
    (
        SELECT s1.person_id, s1.condition_date, s1.concept_code, s1.vocabulary_id
        FROM (SELECT * FROM pw_icds) s1
        INNER JOIN (SELECT * FROM pw_start_end) s2
        ON (s1.person_id = s2.person_id AND
            s1.condition_date >= s2.start_date AND
            s1.condition_date <= s2.end_date)
        GROUP BY s1.person_id, s1.condition_date, s1.concept_code, s1.vocabulary_id
    ),
    pw_icds_op AS
    (
        SELECT s1.person_id, s1.concept_code, s1.condition_date, s1.vocabulary_id
        FROM (
            SELECT s2.person_id, s2.concept_code, s2.condition_date, s2.vocabulary_id
            FROM (SELECT * FROM pw_icds_selected) s2
            INNER JOIN (SELECT * FROM v_visit_occurrence WHERE visit_concept_id = 9202) s3
            ON (s2.person_id = s3.person_id AND s2.condition_date = s3.visit_start_date)
            GROUP BY s2.person_id, s2.concept_code, s2.condition_date, s2.vocabulary_id
       ) s1
    )
    select * from pw_icds_op);", .con = conn)

  return(sql_query)
}
