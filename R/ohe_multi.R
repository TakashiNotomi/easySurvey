#' @title One-Hot Encoding (Single Column)
#' @description \code{ohe_multi_simple} performs one-hot encoding (OHE) by splitting a specified column into unique values.
#' @param data A data frame containing the target data for one-hot encoding.
#' @param column_name A string specifying the column name to perform one-hot encoding on.
#' @return A data frame with the original data and additional one-hot encoded columns.
#' @importFrom stringr str_split str_detect
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom purrr map
#' @importFrom stats setNames
#' @export
#' @examples
#' data <- tibble::tibble(ID = 1:3, col = c("A,B", "B,C", "A"))
#' ohe_multi_simple(data, "col")

ohe_multi_simple <- function(data, column_name) {
  unique_values <- data[[column_name]] |>
    str_split(",\\s*|\uFF0C\\s*") |>
    unlist() |>
    unique()
  ohe_df <- as.data.frame(
    map(unique_values, ~ as.integer(str_detect(data[[column_name]], .x)))
  ) |>
    setNames(paste0(column_name, "_", unique_values))

  bind_cols(data, ohe_df)
}

#' @title One-Hot Encoding (Multiple Columns)
#' @description \code{ohe_multi_multiple} applies one-hot encoding (OHE) to multiple specified columns at once.
#' @param data A data frame containing the target data for one-hot encoding.
#' @param columns A vector of strings specifying the column names to perform one-hot encoding on.
#' @return A data frame with the original data and additional one-hot encoded columns.
#' @importFrom tibble tibble
#' @importFrom purrr reduce
#' @export
#' @examples
#' data <- tibble::tibble(ID = 1:3, col1 = c("A,B", "B,C", "A"), col2 = c("X,Y", "Y,Z", "X"))
#' ohe_multi_multiple(data, c("col1", "col2"))

ohe_multi_multiple <- function(data, columns) {
  reduce(columns, function(df, col) ohe_multi_simple(df, col), .init = data)
}
