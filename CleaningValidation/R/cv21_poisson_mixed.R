#' Poisson Mixed Effect Model Summary
#'
#' Fits a mixed-effects Poisson model to the data and returns a data frame containing
#' the fixed effect part estimates, standard errors, z-values, and p-values.
#'
#' @param data A data frame containing the data set for analysis.
#' @param residue_col A string specifying the column in `data` that contains residue data.
#' @param cleaning_event_col A string specifying the column in `data` for random effects grouping.
#'
#' @return A data frame with the fixed effect summary of the mixed-effects Poisson regression model.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#'
#' @examples
#' # Assuming 'Eq_Mic' is a data frame, 'Mic' is the residue column of interest,
#' # and 'CleaningEvent' is the column for random effects grouping.
#' mixed_effect_summary <- cv21_poisson_mixed(data = Eq_Mic, residue_col = "Mic", 
#' cleaning_event_col = "CleaningEvent")
#' print(mixed_effect_summary)
#' @importFrom lme4 glmer
#' @importFrom stats as.formula coef
cv21_poisson_mixed <- function(data, residue_col, cleaning_event_col) {
  if (!is.character(residue_col) || length(residue_col) != 1 ||
      !is.character(cleaning_event_col) || length(cleaning_event_col) != 1) {
    stop("residue_col and cleaning_event_col must be single column names as character strings.")
  }
  data[[cleaning_event_col]] <- factor(data[[cleaning_event_col]], ordered = FALSE)
  formula <- as.formula(paste0(residue_col, " ~ 1 + (1|", cleaning_event_col, ")"))
  me_poisson <- lme4::glmer(formula, family = "poisson", data = data)
  fe_summary <- coef(summary(me_poisson))
  fe_summary[, "Pr(>|z|)"] <- format(fe_summary[, "Pr(>|z|)"], scientific = TRUE)
  fe_df <- data.frame(
    Term = rownames(fe_summary),
    Estimate = sprintf("%.6f", as.numeric(fe_summary[, "Estimate"])),
    `Std. Error` = sprintf("%.9f", as.numeric(fe_summary[, "Std. Error"])),
    `z value` = sprintf("%.6f", as.numeric(fe_summary[, "z value"])),
    `Pr(>|z|)` = sprintf("%.6f", as.numeric(fe_summary[, "Pr(>|z|)"]))
  )
  names(fe_df) <- c("Term (log)", "Estimate", "Std. Error", "z value", "Pr(\\textgreater|z|)")
  
  return(fe_df)
}

