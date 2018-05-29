#' Standard Error
#'
#' This function allows you to calculate standard error
#' @param x A single vector.
#' @keywords standard error
#' @export
#' @examples
#' se(x)

se <- function(x) {
  sd(x, na.rm = TRUE)/sqrt(length(x))
}

#' Cross-Validated Correlation
#'
#' This function allows you to run a correlation using leave-one-out cross-validation.
#' @param x A single vector.
#' @param y A single vector.
#' @return A single measure of an average correlation coefficient.
#' @keywords cats
#' @export
#' @examples
#' corr_cv(x, y)
#' @import ggplot2
#' @import dplyr

corr_cv <- function(x, y) {
  corval <- vector()
  for (i in 1:length(x)) {
    corval[i] <- cor(x[-i], y[-i], use = "pairwise.complete.obs")
  }
  mean_corr <- mean(corval)
  return(mean_corr)

  temp <- data.frame(x, y) %>%
    dplyr::filter(!is.na(x) &
             !is.na(y))
  plot_cor <- temp %>%
    ggplot2::ggplot(aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::annotate(geom = "text", x = max(x, na.rm = TRUE), y = max(y, na.rm = TRUE),
             label = sprintf("r = %.2f", mean_corr)) +
    ggplot2::theme_light()
  return(plot_cor)
}

