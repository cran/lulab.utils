#' @title extract_logistic_model
#' @description Extract results from a fitted model.
#' @param model The fitted logistic model object.
#' @param markers_name A character vector of marker names.
#' @param n_independent_metabolites The number of independent metabolites.
#' @param digits The number of decimal places to round to.
#' @param effective_size A numeric vector containing effective sample sizes.
#' @param case_size The number of cases.
#' @param control_size The number of controls.
#' @param outcome_name The name of the outcome variable.
#' @return A named vector containing the extracted results.
#' @details This function extracts relevant results from a fitted logistic model and formats them for reporting.
#' @rdname extract_logistic_model
#' @importFrom stats qnorm
#' @export
extract_logistic_model = function(model,
                           markers_name,
                           n_independent_metabolites,
                           digits,
                           effective_size,
                           case_size,
                           control_size,
                           outcome_name){
  x = summary(model)
  biomarker = markers_name[rev(dimnames(x$coefficients)[[1]])[1]]
  p.value = rev(x$coefficients[,"Pr(>|z|)"])[1]
  BonferroniSignificance = ifelse(p.value < 0.05/n_independent_metabolites, "Yes", "No")
  p_value_scientific = formatC(p.value, format = "e", digits = 2)
  estimate_raw = rev(x$coefficients[,"Estimate"])[1]
  se_raw = rev(x$coefficients[,"Std. Error"])[1]
  wald.test = round2(rev(x$coefficients[,"z value"])[1], digits= digits)
  BETA = round2(estimate_raw, digits= digits)
  SE = round2(se_raw, digits= digits)
  OR  = round2(exp(estimate_raw), digits= digits)
  OR.confint.lower = round2(exp(c(estimate_raw-qnorm(0.975)*se_raw)), digits= digits)
  OR.confint.upper = round2(exp(c(estimate_raw+qnorm(0.975)*se_raw)), digits= digits)
  OR_formatted_digits = paste("%." , digits, "f", sep="")
  OR_formatted = paste0(sprintf(OR_formatted_digits, OR), " (", sprintf(OR_formatted_digits, OR.confint.lower), ", ", sprintf(OR_formatted_digits, OR.confint.upper), ")")
  sample_size = effective_size[[rev(dimnames(x$coefficients)[[1]])[1]]]
  case_n = case_size
  control_n = control_size
  outcome_name= outcome_name
  res = c(rev(dimnames(x$coefficients)[[1]])[1], biomarker, outcome_name, sample_size, case_n, control_n, BETA, SE, OR_formatted, p_value_scientific, BonferroniSignificance, wald.test, p.value, OR, OR.confint.lower, OR.confint.upper)
  names(res) = c(
    "Raw_Metabolite", "Metabolite", "Outcome", "Sample size", "Cases", "Controls", "Beta", "SE", "Odds ratio (95% CI)", "P value",
    sprintf("Significant with a Bonferroni correction (0.05/%d=%s)", n_independent_metabolites, formatC(0.05/n_independent_metabolites, format = "e", digits = 2)),
    "Wald test",
    "Raw P value", "OR", "OR.confint.lower", "OR.confint.upper"
  )
  return(res)
}
