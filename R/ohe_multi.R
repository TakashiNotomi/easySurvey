#' @title ワンホットエンコーディング（単一列）
#' @description \code{ohe_multi_simple} 指定した列をユニークな値ごとに分割し、ワンホットエンコーディング（OHE）を実施
#' @param data データフレーム（ワンホットエンコーディングを行う対象のデータ）。
#' @param column_name ワンホットエンコーディングを行う列の名前（文字列）。
#' @return 元のデータフレームにワンホットエンコーディングされた列を追加したデータフレーム。
#' @importFrom stringr str_split str_detect
#' @importFrom dplyr bind_cols
#' @importFrom purrr map
#' @importFrom stats setNames
#' @export
#' @examples
#' data <- tibble(ID = 1:3, col = c("A,B", "B,C", "A"))
#' ohe_multi_simple(data, "col")

ohe_multi_simple <- function(data, column_name) {
  unique_values <- data[[column_name]] |>
    str_split(",\\s*|，\\s*") |>
    unlist() |>
    unique()
  ohe_df <- as.data.frame(
    map(unique_values, ~ as.integer(str_detect(data[[column_name]], .x)))
  ) |>
    setNames(paste0(column_name, "_", unique_values))

  bind_cols(data, ohe_df)
}

#' @title ワンホットエンコーディング（複数列）
#' @description \code{ohe_multi_multiple} 指定した複数列に対してワンホットエンコーディングを一括で適用します。
#' @param data データフレーム（ワンホットエンコーディングを行う対象のデータ）。
#' @param columns ワンホットエンコーディングを行う列名のベクトル（文字列）。
#' @return 元のデータフレームにワンホットエンコーディングされた列を追加したデータフレーム。
#' @importFrom purrr reduce
#' @export
#' @examples
#' data <- tibble(ID = 1:3, col1 = c("A,B", "B,C", "A"), col2 = c("X,Y", "Y,Z", "X"))
#' ohe_multi_multiple(data, c("col1", "col2"))

ohe_multi_multiple <- function(data, columns) {
  reduce(columns, function(df, col) ohe_multi_simple(df, col), .init = data)
}
