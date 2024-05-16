#' Dunn's Test for Residue
#'
#' Perform Dunn's test for residue based on cleaning events. Choose the control
#' group as the cleaning event whose median is closest to the grand median. This
#' function is for investigation purpose.
#'
#' @param data A data frame containing the data.
#' @param residue_col The name of the column containing residue.
#' @param cleaning_event_col The name of the column containing cleaning event identifiers.
#' @importFrom dunn.test dunn.test
#' @return A data frame of Dunn's test results with control group.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#' @examples
#' # 'Eq_DAR' is the  data frame, 'DAR_Pct' is the residue column, and
#' # 'CleaningEvent' is the cleaning event column.
#' Eq_DAR <- cv03_usl_unification(data = Eq_DAR, residue_col = "DAR",
#' cleaning_event_col = "CleaningEvent",  usl_col = "USL")
#' dunn_test_results_vs_control <- cv10_dunn_test_vs_control(data = Eq_DAR,
#' residue_col = "DAR_Pct", cleaning_event_col = "CleaningEvent")
cv10_dunn_test_vs_control <- function(data, residue_col, cleaning_event_col) {
  dunn_test_results <- dunn.test(x = data[[residue_col]], g = data[[cleaning_event_col]], method = "bonferroni")
  grand_median <- median(data[[residue_col]], na.rm = TRUE)
  group_medians <- tapply(data[[residue_col]], data[[cleaning_event_col]], median, na.rm = TRUE)
  differences <- abs(group_medians - grand_median)
  min_diff <- min(differences)
  closest_groups <- names(differences[differences == min_diff])
  control_group <- closest_groups[1]  # Select the first one alphabetically
  control_comparisons <- grep(paste0("\\b", control_group, "\\b"), dunn_test_results$comparisons)
  dunn_test_df <- data.frame(
    Comparison = dunn_test_results$comparisons,
    Z_Value = dunn_test_results$Z,
    P_Value = dunn_test_results$P,
    P_Adjusted = dunn_test_results$P.adjusted
  )
  dunn_test_filtered <- dunn_test_df[control_comparisons, ]
  return(dunn_test_filtered)
}

