#' @import stats
NULL

#' Creates a log of dataframe shape, and entry missingness
#'
#' @param data_to_log dataset to create a log message from
#'
#' @return a string summarising the dataframe shape and missing info
#' @export
#'
#' @examples
#' df_shape_log_message(cars)
df_shape_log_message <- function(data_to_log) {
  row_missing_entries <- complete.cases(data_to_log)
  column_missing_entries <- complete.cases(t(data_to_log))
  dataframe_shape_message <-
    glue::glue(
      "Dataframe has: {nrow(data_to_log)} rows, {ncol(data_to_log)} columns,
            {sum(row_missing_entries)} rows without missing entries,
            {sum(column_missing_entries)} columns without missing entries"
    )

  return(dataframe_shape_message)
}
