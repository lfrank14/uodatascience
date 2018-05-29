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
#' @keywords cross-validation, correlation
#' @export
#' @examples
#' corr_cv(got$`Book Intro Chapter`, got$`Death Chapter`)

corr_cv <- function(x, y) {
  corval <- vector()
  for (i in 1:length(x)) {
    corval[i] <- cor(x[-i], y[-i], use = "pairwise.complete.obs")
  }
  mean_corr <- mean(corval)
  return(mean_corr)
}

