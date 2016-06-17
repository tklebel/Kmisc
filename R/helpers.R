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
  labels <- .data %>%
    attributes() %>%
    .[c("names", "variable.labels")]
  
  if (length(labels$names) < length(labels$variable.labels)) {
    out <- lapply(labels, "[", c(1:length(labels$names)))
    message("Number of variables and variable.labels not identical. Truncating
            variable.labels to length of variables.")
  }
  
  dplyr::bind_cols(out)
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
  missing <- .data %>% 
    dplyr::summarise_all(funs(round(mean(is.na(.)), 2))) %>% 
    tidyr::gather(var, wert) %>% 
    split(.$var)
  
  data <- .data %>%
    tidyr::gather(var, wert) %>%
    dplyr::filter(!is.na(wert)) %>%
    split(.$var)
  
  list(.data = data, missing = missing)
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
