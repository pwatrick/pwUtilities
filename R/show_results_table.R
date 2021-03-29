#' Show results table
#'
#' @details
#' Last updated: 2021-03-29
#'
#' @param data_table A tibble
#' @param length A numeric
#' @export

show_results_table <- function(data_table, length = 100) {
  DT::datatable(data_table,
                filter = 'top',
                options = list(pageLength = length,
                               autoWidth = TRUE))
}
