#' Autmated bar plots
#'
#' This functions returns bar plots for all the factors in a data set.
#'
#' @param .data A \code{data.frame}.
#' @param .labels A second \code{data.frame}, containing the variables with
#'  corresponding labels to set titles
#'
#' @return Plots as side effect.
#' @export
plot_factors <- function(.data, .labels = NULL, missingness = T) {
  reshaped <- .data %>%
    dplyr::select_if(is.factor) %>%
    reshape_data()

  purrr::pmap(list(reshaped$.data, reshaped$missing), plot_bar,
              .labels = .labels, missingness)}

#' Plot bar for factors
#'
#' @param .data A \code{data.frame} with the variable of interest named
#'    \code{wert}.
#'
#' @keywords internal
#' @noRd
plot_bar <- function(.data, .labels) {
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
    geom_bar() +
    scale_x_discrete(drop = F) +
    labs(x = NULL,
         y = NULL,
         title = missingness) +
    theme_bw()
}
