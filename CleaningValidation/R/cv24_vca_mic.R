#' Variance Component Analysis for Microbial Counts
#'
#' This function performs a variance component analysis using a mixed-effects model
#' with a Poisson distribution to estimate within-group and between-group variance
#' for microbial counts data. Assumes data is grouped by cleaning events and 
#' evaluates the residue or microbial counts within these groups.
#'
#' @param data A data frame containing the dataset.
#' @param residue_col The name of the column in `data` that contains the residue or microbial counts.
#' @param cleaning_event_col The name of the column in `data` that contains the grouping factor for cleaning events.
#'
#' @return A data frame summarizing the variance components, including within-group variance,
#'         between-group variance, and total variance, along with their percentages and standard deviations.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#' @examples
#' # Assuming `Eq_Mic` is your dataframe, `Mic` is the microbial counts column, 
#' # and `CleaningEvent` is the cleaning event column:
#' cv24_vca_mic(Eq_Mic, "Mic", "CleaningEvent")
#'
#' @importFrom lme4 glmer VarCorr fixef
cv24_vca_mic <- function(data, residue_col, cleaning_event_col) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Please install the 'lme4' package.")
  }
  data[[cleaning_event_col]] <- as.factor(data[[cleaning_event_col]])
  formula <- as.formula(paste0(residue_col, " ~ (1|", cleaning_event_col, ")"))
  me_poisson <- lme4::glmer(formula, family = "poisson", data = data)
  var_comp <- lme4::VarCorr(me_poisson)
  random_effect_variance <- var_comp[[cleaning_event_col]][1] 
  fixed_effect_estimate <- fixef(me_poisson)[1]
  within_group_variance <- exp(fixed_effect_estimate)
  total_variance <- within_group_variance + random_effect_variance
  within_group_percentage <- (within_group_variance / total_variance) * 100
  between_group_percentage <- (random_effect_variance / total_variance) * 100
  within_group_sd <- sqrt(within_group_variance)
  between_group_sd <- sqrt(random_effect_variance)
  total_sd <- sqrt(total_variance)
  summary_table <- data.frame(
    Component = c("Within-Group", "Between-Group", "Total"),
    Variance = round(c(within_group_variance, random_effect_variance, total_variance), 3),
    Percentage = round(c(within_group_percentage, between_group_percentage, 100), 3),
    `Standard Deviation` = round(c(within_group_sd, between_group_sd, total_sd), 3)
  )
  
  return(summary_table)
}
