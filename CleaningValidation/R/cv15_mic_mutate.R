#' Calculate Mic Statistics
#'
#' @param data A dataframe containing the data.
#' @param cleaning_event_col The name of the column that identifies the cleaning event.
#' @param residue_col The name of the column containing residue measurements.
#' @return A dataframe with new columns for mean, median, grand mean, and grand median of Mic values.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#' @examples
#' cv15_mic_mutate(data=Eq_Mic, cleaning_event_col="CleaningEvent", residue_col="Mic")
#' @importFrom dplyr group_by mutate ungroup
cv15_mic_mutate <- function(data, cleaning_event_col, residue_col) {
  mean_col_name <- paste0(residue_col, "_Mean")
  median_col_name <- paste0(residue_col, "_Median")
  grand_mean_col_name <- paste0(residue_col, "_Grand_Mean")
  grand_median_col_name <- paste0(residue_col, "_Grand_Median")

  grand_mean <- mean(data[[residue_col]], na.rm = TRUE)
  grand_median <- median(data[[residue_col]], na.rm = TRUE)

  data <- data %>%
    group_by(!!sym(cleaning_event_col)) %>%
    mutate(
      "{mean_col_name}" := mean(!!sym(residue_col), na.rm = TRUE),
      "{median_col_name}" := median(!!sym(residue_col), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      "{grand_mean_col_name}" := grand_mean,
      "{grand_median_col_name}" := grand_median
    )
  
  return(data)
}
