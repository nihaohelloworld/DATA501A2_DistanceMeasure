#' Calculate Influence Measures
#'
#' This function calculates Cook's Distance, DFFITS, or Hadi's Influence Measure for a given linear model.
#'
#' @param data A data frame containing the data.
#' @param model An `lm` object representing the linear model.
#' @param method A character string specifying the influence measure to calculate.
#'        One of "cooks", "dffits", or "hadi".
#' @return A vector of influence measures.
#' @importFrom stats lm cooks.distance dfbetas hatvalues
#' @examples
#' lm_model <- lm(mpg ~ wt + hp, data = mtcars)
#' influence_measures(mtcars, lm_model, method = "cooks")
#' influence_measures(mtcars, lm_model, method = "dffits")
#' influence_measures(mtcars, lm_model, method = "hadi")
#' @export
influence_measures <- function(data, model, method = c("cooks", "dffits", "hadi")) {
  method <- match.arg(method)

  if (!inherits(model, "lm")) {
    stop("The model must be an object of class 'lm'.")
  }

  if (method == "cooks") {
    return(cooks.distance(model))
  }

  if (method == "dffits") {
    # Calculate DFFITS
    h <- hatvalues(model)
    residuals <- residuals(model)
    s <- summary(model)$sigma
    dffits_values <- residuals / (s * sqrt(1 - h)) * sqrt(h / (1 - h))
    return(dffits_values)
  }

  if (method == "hadi") {
    # Calculate Hadi's Influence Measure
    X <- model.matrix(model)
    h <- hatvalues(model)
    residuals <- residuals(model)
    s <- summary(model)$sigma
    p <- ncol(X)

    d <- residuals^2 / (1 - h)
    Hadi_measure <- sqrt(p * h / (1 - h) + d / s^2)
    return(Hadi_measure)
  }

  stop("Invalid method. Choose one of 'cooks', 'dffits', or 'hadi'.")
}

data("mtcars")
lm_model <- lm(mpg ~ wt + hp, data = mtcars)
result <- influence_measures(mtcars, lm_model, method = "cooks")
plot(result)
