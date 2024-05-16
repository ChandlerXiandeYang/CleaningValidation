#' Extract Random Effect Coefficients
#'
#' This function fits a Poisson mixed-effects model with a specified random effect 
#' and extracts the random effect coefficients and their standard deviations.
#'
#' @param data A data frame containing the data.
#' @param residue_col The name of the residue column.
#' @param cleaning_event_col The name of the column used for random effects grouping.
#'
#' @return A data frame with the random effect coefficients and standard deviations.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#'
#' @examples
#' re_coefs <- cv23_random_effect_coef(data=Eq_Mic, residue_col="Mic", 
#' cleaning_event_col="CleaningEvent")
#'
#' @importFrom lme4 glmer ranef
#' @importFrom stats as.formula
cv23_random_effect_coef <- function(data, residue_col, cleaning_event_col) {
  data[[cleaning_event_col]] <- factor(data[[cleaning_event_col]], ordered = FALSE)
  formula <- as.formula(paste0(residue_col, " ~ 1 + (1|", cleaning_event_col, ")"))
  me_poisson <- lme4::glmer(formula, family = "poisson", data = data)
  random_effects <- lme4::ranef(me_poisson, condVar = TRUE)
  std_devs <- sapply(1:length(random_effects[[cleaning_event_col]]), function(i) {
    sqrt(attr(random_effects[[cleaning_event_col]], "postVar")[1, 1, i])
  })
  level_names <- rownames(random_effects[[cleaning_event_col]])
  coefficients <- as.numeric(random_effects[[cleaning_event_col]][,1])
  
  # Apply scientific formatting only to non-NA values
  formatted_coefs <- ifelse(is.na(coefficients), NA, format(coefficients, scientific = TRUE))
  formatted_std_devs <- format(std_devs, scientific = TRUE)
  
  # Create a data frame with Coefficient column first
  re_table <- data.frame(
    CleaningEvent = level_names,
    Coefficient = formatted_coefs,
    StdDev = formatted_std_devs
  )
  
  return(re_table)
}
