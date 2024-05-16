#' Extract Variance of Random Effects
#'
#' This function fits a Poisson mixed-effects model with a specified random effect 
#' and extracts the variances and standard deviations of the random effects.
#'
#' @param data A data frame containing the data.
#' @param residue_col The name of the residue column.
#' @param cleaning_event_col The name of the column used for random effects grouping.
#'
#' @return A data frame with the variances and standard deviations of the random effects.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#'
#' @examples
#' ra_table <- cv22_var_random_effect(data=Eq_Mic, residue_col="Mic", 
#' cleaning_event_col="CleaningEvent")
#'
#' @importFrom lme4 glmer VarCorr
#' @importFrom stats as.formula
cv22_var_random_effect <- function(data, residue_col, cleaning_event_col) {
  data[[cleaning_event_col]] <- factor(data[[cleaning_event_col]], ordered = FALSE)
  
  formula <- as.formula(paste0(residue_col, " ~ 1 + (1|", cleaning_event_col, ")"))
  
  me_poisson <- lme4::glmer(formula, family = "poisson", data = data)
  
  re_summary <- lme4::VarCorr(me_poisson)
  
  group_name <- names(re_summary)
  
  variance <- sapply(re_summary, function(x) x@.Data[1])
  std_dev <- sqrt(variance)
  
  # Format the variance and standard deviation in scientific notation
  variance_sci <- format(variance, scientific = TRUE)
  std_dev_sci <- format(std_dev, scientific = TRUE)
  
  ra_table <- data.frame(
    Group = group_name,
    Variance = variance_sci,
    `Std. Dev.` = std_dev_sci
  )
  
  return(ra_table)
}

