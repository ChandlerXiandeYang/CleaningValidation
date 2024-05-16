#' Perform Shapiro-Wilk Normality Test
#'
#' This function performs the Shapiro-Wilk test for normality on a specified variable in a dataset.
#' It returns a data frame with the variable name, the Shapiro-Wilk statistic, the p-value in scientific notation,
#' and an indication of whether the p-value is less than 0.05.
#'
#' @param data A data frame containing the dataset.
#' @param residue_col The name of the column to test for normality.
#' @return A data frame with the Shapiro-Wilk test results.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @examples
#' data(Eq_Mic)
#' cv05_sw_norm_test_1(data=Eq_Mic, residue_col="Mic")
#' @export
#' @importFrom stats shapiro.test
cv05_sw_norm_test_1 <- function(data, residue_col) {
  shapiro_test <- stats::shapiro.test(data[[residue_col]])
  
  results <- data.frame(
    Variable = residue_col,
    SW_Statistic = shapiro_test$statistic,
    P_value = format(shapiro_test$p.value, scientific = TRUE),
    P_less_than_0_05 = ifelse(shapiro_test$p.value < 0.05, "Yes", "No")
  )
  
  return(results)
}
