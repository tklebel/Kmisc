#' Autmated histograms
#'
#' This functions returns histograms for all numeric variables in a data set.
#'
#' @param .data A \code{data.frame}.
#' @param .labels A second \code{data.frame}, containing the variables with
#'  corresponding labels to set titles
#'
#' @return Plots as side effect.
#' @export
plot_numerics <- function(.data, .labels = NULL) {
  reshaped <- .data %>%
    dplyr::select_if(is.numeric) %>%
    reshape_data()
  
  purrr::map(reshaped, plot_hist, .labels = .labels)
}

#' Plot histogram for factors
#'
#' @param .data A \code{data.frame} with the variable of interest named
#'    \code{wert}.
#'
#' @keywords internal
#' @noRd
plot_hist <- function(.data, .labels) {
  title <- NULL
  if (!is.null(.labels)) {
    # find title for graph
    var <- .data[[1]][1]
    title <- find_label(.labels, var)
  }
  
  ggplot(.data, aes(wert)) +
    geom_histogram() +
    labs(x = NULL,
         y = NULL,
         title = title) +
    theme_bw()
}
