#' Poisson Fixed Effect Model Summary
#'
#' Fits a simple Poisson model to the data and returns a data frame containing
#' the model's term, estimate, standard error, z value, and p-value, formatted to
#' a fixed number of decimal places.
#'
#' @param data A data frame containing the data set for analysis.
#' @param residue_col The name of the column representing residue data.
#'
#' @return A data frame with the formatted summary of the Poisson regression model.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#'
#' @examples
#' # Assuming 'Eq_Mic' is a data frame and 'Mic' is the residue column of interest.
#' cv19_poisson_simple(data = Eq_Mic, residue_col = "Mic")
#' @importFrom stats glm as.formula
cv19_poisson_simple <- function(data, residue_col) {
  # Ensure residue_col is a character string
  if (!is.character(residue_col) || length(residue_col) != 1) {
    stop("residue_col must be a single column name as a character string.")
  }

  formula <- as.formula(paste0(residue_col, " ~ 1"))
  poisson_model <- glm(formula, family = "poisson", data = data)
  poisson_model_summary <- summary(poisson_model)

  model_table <- data.frame(
    `Term (log)` = rownames(poisson_model_summary$coefficients),
    Estimate = sprintf("%.6f", poisson_model_summary$coefficients[, "Estimate"]),
    `Std. Error` = sprintf("%.6f", poisson_model_summary$coefficients[, "Std. Error"]),
    `z value` = sprintf("%.6f", poisson_model_summary$coefficients[, "z value"]),
    `Pr(>|z|)` = sprintf("%.6f", poisson_model_summary$coefficients[, "Pr(>|z|)"])
  )
  
  names(model_table) <- c("Term(log)", "Estimate", "Std. Error", "z value", "Pr(>|z|)")
  
  return(model_table)
}
