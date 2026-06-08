library(testthat)

test_that("extract_logistic_model extracts logistic regression results", {
  df <- data.frame(
    y = c(rep(0, 20), rep(1, 20)),
    marker = c(rnorm(20, 0), rnorm(20, 1))
  )
  model <- glm(y ~ marker, data = df, family = binomial())

  result <- extract_logistic_model(
    model = model,
    markers_name = c(marker = "Marker"),
    n_independent_metabolites = 1,
    digits = 2,
    effective_size = c(marker = 40),
    case_size = 20,
    control_size = 20,
    outcome_name = "y"
  )

  expect_named(result)
  expect_equal(unname(result[["Metabolite"]]), "Marker")
  expect_true(as.numeric(result[["OR.confint.lower"]]) <= as.numeric(result[["OR.confint.upper"]]))
})
