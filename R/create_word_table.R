#' Use flextable package to create table for word docx
#' Last updated: 2021-03-02
#'
#' @param file_name A string, e.g., "ddiwas_supptable3_ppv_table_20200914085357.xlsx"
#' @param file_type A string, e.g., "excel" or "csv"
#' @param pvalue_col A boolean. Default = T. Set = F if there is not a pvalue column
#' @param df A tibble. Specify if using tibble instead of file name
#' @export

create_word_table <- function(file_name, df, file_type = "excel", pvalue_col = T) {

  if (missing(file_name)) {
    ft <- df
  }

  if (missing(df)) {
    if (file_type == "csv") {
      ft <- vroom::vroom(file_name, .name_repair = janitor::make_clean_names)
    }
    if (file_type == "excel") {
      ft <- readxl::read_excel(file_name)
    }
  }

  if (pvalue_col == T) {
      ft <- ft %>%
        mutate(
          pval = sprintf('%.2E', pval)
        )
    }

  ft <- flextable(ft)
  ft <- theme_vanilla(ft)
  return(ft)
}
