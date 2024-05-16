#' Poisson Fixed Effect Model
#'
#' Fits a fixed effects Poisson model and returns a data frame with the summary. 
#' If the p-value is significant, then the corresponding cleaning event is significantly 
#' different from other cleaning events. For a stable cleaning process, we wish all  
#' p-values are not significant.
#'
#' @param data Data frame containing the data.
#' @param residue_col The name of the residue column.
#' @param cleaning_event_col The name of the cleaning event column.
#' @return A data frame output with the fixed effect summary.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#' @examples
#' fixed_effect_summary <- cv20_poisson_fixed(data = Eq_Mic, residue_col = "Mic", 
#' cleaning_event_col = "CleaningEvent")
#' @importFrom stats glm as.formula coef
cv20_poisson_fixed <- function(data, residue_col, cleaning_event_col) {
  if (!is.character(residue_col) || length(residue_col) != 1 ||
      !is.character(cleaning_event_col) || length(cleaning_event_col) != 1) {
    stop("residue_col and cleaning_event_col must be single column names as character strings.")
  }
  
  data[[cleaning_event_col]] <- factor(data[[cleaning_event_col]], ordered = FALSE)
  
  formula <- as.formula(paste0(residue_col, " ~ ", cleaning_event_col))
  
  poisson_fixed <- glm(formula, family = "poisson", data = data)
  
  fe_summary <- summary(poisson_fixed)$coefficients
  fe_df <- data.frame(
    Term = rownames(fe_summary),
    Estimate = as.numeric(fe_summary[, "Estimate"]),
    `Std. Error` = as.numeric(fe_summary[, "Std. Error"]),
    `z value` = as.numeric(fe_summary[, "z value"]),
    `Pr(>|z|)` = as.numeric(fe_summary[, "Pr(>|z|)"])
  )
  names(fe_df) <- c("Term(log)", "Estimate", "Std. Error", "z value", "Pr(>|z|)")
  
  return(fe_df)
}

