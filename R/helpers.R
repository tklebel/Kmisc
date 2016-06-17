#' Find variable label
#'
#' @param .data A data.frame containig the labels
#' @param .var A character string of length one with the name of the variable
#'
#' @return The variable lable of the requested variable.
#' @export
find_label <- function(.data, .var) {
  .data %>%
    dplyr::filter(names == .var) %>%
    dplyr::select(variable.labels) %>%
    unlist() %>%
    unname()
}

#' Extract variable labels
#'
#' @param .data A \code{data.frame}
#'
#' @return A \code{data.frame} with the attributes \code{names} and
#'    \code{variable.labels} as columns.
#' @export
label_df <- function(.data) {
  .data %>%
    attributes() %>%
    .[c("names", "variable.labels")] %>%
    dplyr::bind_cols()
}




#' Reshape to long form ans split by var
#'
#' @param .data A \code{data.frame}.
#'
#' @return A list with as many elements as there were variables in the original
#'    data.
#' @keywords internal
#' @noRd
reshape_data <- function(.data) {
  .data %>%
    tidyr::gather(var, wert) %>%
    dplyr::filter(!is.na(wert)) %>%
    split(.$var)
}

#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
