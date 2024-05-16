#' Poisson Goodness-of-Fit Test
#'
#' Conducts a goodness-of-fit test to evaluate if the Mic data follows a Poisson distribution.
#'
#' @param data A dataframe containing the observed data.
#' @param residue_col A string specifying the column in `data` to be tested.
#'
#' @return A dataframe object representing the chi-squared statistic and the p-value from the goodness-of-fit test.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export 
#' @importFrom stats pchisq dpois
#' @examples
#' # Assuming Eq_Mic is your dataframe and Mic is the column to be tested
#'  cv13_poisson_test(data=Eq_Mic, residue_col="Mic")
cv13_poisson_test <- function(data, residue_col) {
  lambda <- mean(data[[residue_col]], na.rm = TRUE)
  obs_freq <- table(data[[residue_col]])
  exp_freq <- dpois(as.numeric(names(obs_freq)), lambda) * length(data[[residue_col]])
  gof <- sum((obs_freq - exp_freq)^2 / exp_freq, na.rm = TRUE)
  p_value <- pchisq(gof, df = length(obs_freq) - 1, lower.tail = FALSE)
  results_df <- data.frame(
    `Chi_Squared_Statistic` = round(gof, 3),
    P_Value = format.pval(p_value, digits = 3)
  )
  return(results_df)
}


