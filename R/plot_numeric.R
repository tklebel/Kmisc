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
plot_numerics <- function(.data, .labels = NULL, missingness = T) {
  reshaped <- .data %>%
    dplyr::select_if(is.numeric) %>%
    reshape_data()

  purrr::pmap(list(reshaped$.data, reshaped$missing), plot_hist,
              .labels = .labels, missingness)
}

#' Plot histogram for numeric data
#'
#' @param .data A \code{data.frame} with the variable of interest named
#'    \code{wert}.
#'
#' @keywords internal
#' @noRd
plot_hist <- function(.data, .missing, .labels, missingness) {
  title <- NULL
  if (!is.null(.labels)) {
    # find title for graph
    var <- .data[[1]][1]
    title <- find_label(.labels, var)
    title <- stringr::str_wrap(title, width = 70)
  }
  
  if (missingness) {
    missingness <- paste0("Missing proportion: ", .missing$wert)
  } else {
    missingness <- NULL
  }

  ggplot(.data, aes(wert)) +
    geom_histogram() +
    labs(x = NULL,
         title = title,
         caption = missingness) +
    theme_bw()
}
