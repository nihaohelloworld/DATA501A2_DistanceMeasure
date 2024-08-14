library(testthat)

test_that("plot_influence_measure catches non-lm model errors", {
  data <- mtcars
  model <- lm(mpg ~ wt + hp, data = data)

  expect_error(plot_influence_measure(data, "not_a_model", method = "cooks"),
               "The model must be an object of class 'lm'.")
})

test_that("plot_influence_measure catches non-data frame input", {
  model <- lm(mpg ~ wt + hp, data = mtcars)

  expect_error(plot_influence_measure("not_a_data_frame", model, method = "cooks"),
               "The data must be a data frame.")
})

test_that("plot_influence_measure catches missing variables in data", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  incomplete_data <- mtcars[, -which(names(mtcars) == "wt")]

  expect_error(plot_influence_measure(incomplete_data, model, method = "cooks"),
               "The data does not contain all the variables used in the model.")
})

test_that("plot_influence_measure catches mismatched data and model rows", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  extra_row <- rbind(mtcars, mtcars[1, ])

  expect_error(plot_influence_measure(extra_row, model, method = "cooks"),
               "The number of observations in the data does not match the number used in the model.")
})

test_that("plot_influence_measure catches NA values in data", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  mtcars_with_na <- mtcars
  mtcars_with_na[1, "mpg"] <- NA

  expect_error(plot_influence_measure(mtcars_with_na, model, method = "cooks"),
               "The data contains NA values. Please remove or impute them before proceeding.")
})

test_that("plot_influence_measure catches infinite values in data", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  mtcars_with_inf <- mtcars
  mtcars_with_inf[1, "mpg"] <- Inf

  expect_error(plot_influence_measure(mtcars_with_inf, model, method = "cooks"),
               "The data contains infinite values. Please remove or correct them before proceeding.")
})

test_that("plot_influence_measure catches fewer predictors than coefficients", {
  model <- lm(mpg ~ wt + hp + drat, data = mtcars)
  reduced_data <- mtcars[, c("mpg", "wt", "hp")]

  expect_error(plot_influence_measure(reduced_data, model, method = "cooks"),
               "The number of predictors in the data is less than the number of coefficients in the model.")
})

test_that("cook_distance function returns correct length", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  cooks_values <- cook_distance(model)

  expect_equal(length(cooks_values), nrow(mtcars))
})

test_that("dffits function returns correct length", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  dffits_values <- dffits(model)

  expect_equal(length(dffits_values), nrow(mtcars))
})

test_that("hadi_influence function returns correct length", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  hadi_values <- hadi_influence(model)

  expect_equal(length(hadi_values), nrow(mtcars))
})
