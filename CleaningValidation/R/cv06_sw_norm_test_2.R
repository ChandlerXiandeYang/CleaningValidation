#' Perform Shapiro-Wilk Normality Test on Two Variables
#'
#' This function performs the Shapiro-Wilk test for normality on two specified variables within a dataset.
#' It returns a data frame with the variables' names, Shapiro-Wilk statistics, p-values in scientific notation,
#' and indications of whether the p-values are less than 0.05.
#'
#' @param data A data frame containing the dataset.
#' @param residue_col The name of the first column to test for normality.
#' @param residue_pct_col The name of the second column to test for normality.
#' @return A data frame with Shapiro-Wilk test results for both variables.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @examples
#' # assuming Eq_DAR is a predefined dataset 
#' Eq_DAR <- cv03_usl_unification(data = Eq_DAR,  "CleaningEvent",  "DAR",  "USL")
#' cv06_sw_norm_test_2(data=Eq_DAR, residue_col="DAR", residue_pct_col="DAR_Pct")
#' @export
#' @importFrom stats shapiro.test
cv06_sw_norm_test_2 <- function(data, residue_col, residue_pct_col) {
  shapiro_test_res_col <- stats::shapiro.test(data[[residue_col]])
  shapiro_test_res_pct_col <- shapiro.test(data[[residue_pct_col]])
  
  results <- data.frame(
    Variable = c(residue_col, residue_pct_col),
    SW_Statistic = c(shapiro_test_res_col$statistic, shapiro_test_res_pct_col$statistic),
    P_value = format(c(shapiro_test_res_col$p.value, shapiro_test_res_pct_col$p.value), scientific = TRUE),
    P_less_than_0_05 = ifelse(c(shapiro_test_res_col$p.value, shapiro_test_res_pct_col$p.value) < 0.05, "Yes", "No")
  )
  return(results)
}
