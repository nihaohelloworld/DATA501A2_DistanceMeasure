#' Calculate Influence Measure and Plot
#'
#' This function calculates one of Cook's Distance, DFFITS, or Hadi's Influence Measure
#' for a given linear model and then creates a scatter plot of the values.
#'
#' @param data A data frame containing the data.
#' @param model An `lm` object representing the linear model.
#' @param method A character string specifying the influence measure to calculate.
#'        One of "cooks", "dffits", or "hadi".
#' @return A scatter plot of the selected influence measure.
#' @importFrom stats lm cooks.distance residuals
#' @importFrom graphics plot abline
#' @examples
#' lm_model <- lm(mpg ~ wt + hp, data = mtcars)
#' influence_measure_plot(mtcars, lm_model, method = "cooks")
#' influence_measure_plot(mtcars, lm_model, method = "dffits")
#' influence_measure_plot(mtcars, lm_model, method = "hadi")
#' @export
influence_measure_plot <- function(data, model, method = c("cooks", "dffits", "hadi")) {
  method <- match.arg(method)

  if (!inherits(model, "lm")) {
    stop("The model must be an object of class 'lm'.")
  }

  if (!is.data.frame(data)) {
    stop("The data must be a data frame.")
  }

  model_vars <- all.vars(formula(model))
  if (!all(model_vars %in% names(data))) {
    stop("The data does not contain all the variables used in the model.")
  }

  if (nrow(data) != nrow(model$model)) {
    stop("The number of observations in the data does not match the number used in the model.")
  }

  if (anyNA(data)) {
    stop("The data contains NA values. Please remove or impute them before proceeding.")
  }

  if (any(is.infinite(as.matrix(data)))) {
    stop("The data contains infinite values. Please remove or correct them before proceeding.")
  }

  if (ncol(data) < length(coef(model))) {
    stop("The number of predictors in the data is less than the number of coefficients in the model.")
  }

  influence_values <- switch(
    method,
    "cooks" = cook_distance_custom(model),
    "dffits" = dffits_custom(model),
    "hadi" = hadi_influence_custom(model)
  )

  plot(
    influence_values,
    # type = "h",
    main = paste("Influence Measure:", method),
    xlab = "Observation",
    ylab = paste(method, "value"),
    col = "blue",
    lwd = 2
  )

  return(influence_values)
}

#' Cook's Distance Calculation
#'
#' @inheritParams influence_measure_plot
#' @return A vector of Cook's Distance values
#' @export
cook_distance_custom <- function(model) {
  y_hat <- fitted(model)
  n <- length(y_hat)
  p <- length(coef(model))
  residuals <- residuals(model)
  sse <- sum(residuals^2)
  sigma_sq_hat <- sse / (n - p - 1)
  cooks_distances <- numeric(n)

  for (i in 1:n) {
    model_i <- lm(formula(model), data = model$model[-i, , drop = FALSE])
    y_hat_i <- predict(model_i, newdata = model$model)
    cooks_distances[i] <- sum((y_hat - y_hat_i)^2) / (sigma_sq_hat * (p + 1))
  }

  return(cooks_distances)
}


#' DFFITS Calculation
#'
#' @inheritParams influence_measure_plot
#' @return A vector of DFFITS values
#' @export
dffits_custom <- function(model) {
  y_hat <- fitted(model)
  n <- length(y_hat)
  p <- length(coef(model))
  X <- model.matrix(model)
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  dffits_values <- numeric(n)

  for (i in 1:n) {
    model_i <- lm(formula(model), data = model$model[-i, , drop = FALSE])
    y_hat_i_minus <- predict(model_i, newdata = model$model[i, , drop = FALSE])
    p_ii <- H[i, i]
    residuals_i <- residuals(model_i)
    sse_i <- sum(residuals_i^2)
    sigma_i <- sqrt(sse_i / (n - p - 2))
    dffits_values[i] <- (y_hat[i] - y_hat_i_minus) / (sigma_i * sqrt(p_ii))
  }

  return(dffits_values)
}


#' Hadi's Influence Measure Calculation
#'
#' @inheritParams influence_measure_plot
#' @return A vector of Hadi's Influence Measure values
#' @export
hadi_influence_custom <- function(model) {
  n <- length(model$fitted.values)
  p <- length(coef(model))
  X <- model.matrix(model)
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  residuals <- residuals(model)
  sse <- sum(residuals^2)
  hadi_values <- numeric(n)

  for (i in 1:n) {
    p_ii <- H[i, i]
    d_i <- residuals[i] / sqrt(sse)
    hadi_values[i] <- (p_ii / (1 - p_ii)) + ((p + 1) / (1 - p_ii)) * (d_i^2 / (1 - d_i^2))
  }

  return(hadi_values)
}


