#' Dispersion Test for Poisson Regression Models
#'
#' Performs a dispersion test on a Poisson regression model to check for overdispersion.
#' The function fits a Poisson regression model to the data using the specified columns,
#' and then performs a dispersion test using the model.
#'
#' @param data A dataframe containing the observed data.
#' @param residue_col A string specifying the response column in the model.
#' @param cleaning_event_col A string specifying the explanatory variable in the model.
#'
#' @return A dataframe object with the results of the overdispersion test, including
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' the Z-value, P-value, and dispersion estimate.
#' @export
#' @importFrom stats glm
#' @importFrom AER dispersiontest
#' @examples
#' cv14_dispersion_test(data = Eq_Mic, residue_col = "Mic", cleaning_event_col = "CleaningEvent")
cv14_dispersion_test <- function(data, residue_col, cleaning_event_col) {
  formula <- as.formula(paste(residue_col, cleaning_event_col, sep = " ~ "))
  poisson_model <- glm(formula, data = data, family = "poisson")
  dispersion_test_results <- dispersiontest(poisson_model)
  dispersion_df <- data.frame(
    Test = "Overdispersion Test",
    Z_Value = dispersion_test_results$statistic,
    P_Value = dispersion_test_results$p.value,
    Dispersion = dispersion_test_results$estimate
  )
  return(dispersion_df)
}
