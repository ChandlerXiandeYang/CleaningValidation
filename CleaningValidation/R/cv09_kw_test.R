#' Kruskal-Wallis Test for Residue Percentages
#'
#' Perform Kruskal-Wallis test for residue percentages based on cleaning events.
#'
#' @param data A data frame containing the data.
#' @param residue_col The name of the column containing residue percentages.
#' @param cleaning_event_col The name of the column containing cleaning event identifiers.
#' @importFrom stats kruskal.test reformulate
#' @return A data frame of Kruskal-Wallis test results.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#' @examples
#' # Assuming 'Eq_DAR' is the data frame, 'DAR_Pct' is the residue column, 
#' # and 'CleaningEvent' is the cleaning event column.
#' Eq_DAR <- cv01_dfclean(data=Eq_DAR, cleaning_event_col="CleaningEvent", 
#' residue_col="DAR", usl_col="USL" ) 
#' Eq_DAR <- cv03_usl_unification(data=Eq_DAR, cleaning_event_col="CleaningEvent", 
#' residue_col="DAR", usl_col="USL")
#' kw_test_results <- cv09_kw_test(data = Eq_DAR, residue_col = "DAR_Pct", 
#'  cleaning_event_col = "CleaningEvent")
cv09_kw_test <- function(data, residue_col, cleaning_event_col) {
  kw_test <- stats::kruskal.test(formula = reformulate(residue_col, cleaning_event_col), data = data)
  kw_test_results_data <- data.frame(
    Statistic = kw_test$statistic,
    DF = kw_test$parameter,
    PValue = kw_test$p.value,
    Method = kw_test$method
  )
  
  return(kw_test_results_data)
}

