#' Unify USL Percentages for Specified Residue
#'
#' This function takes a dataset and computes the percentage of residue over USL for each event, as well as mean and median of these percentages for each cleaning event and overall.
#'
#' @param data A dataframe containing the relevant dataset.
#' @param cleaning_event_col Name of the column in `data` that contains the cleaning event identifiers as a string.
#' @param residue_col Name of the column in `data` that contains the residue measurements as a string.
#' @param usl_col Name of the column in `data` that contains the USL values as a string.
#' @return A dataframe with original data and additional columns for residue percentages, and their mean and median values per cleaning event and overall.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @importFrom dplyr mutate group_by ungroup %>%
#' @importFrom rlang sym :=
#' @importFrom stats median
#' @export
#' @examples
#' cv03_usl_unification(data = Eq_DAR, cleaning_event_col = "CleaningEvent", 
#' residue_col = "DAR", usl_col = "USL")
cv03_usl_unification <- function(data, cleaning_event_col, residue_col, usl_col) {
  pct_col_name <- paste0(residue_col, "_Pct")
  mean_col_name <- paste0(residue_col, "_Pct_Mean")
  median_col_name <- paste0(residue_col, "_Pct_Median")
  grand_mean_col_name <- paste0(residue_col, "_Pct_Grand_Mean")
  grand_median_col_name <- paste0(residue_col, "_Pct_Grand_Median")

  data <- data %>%
    dplyr::mutate(
      "{pct_col_name}" := (!!sym(residue_col) / !!sym(usl_col)) * 100,
      USL_Pct = 100
    ) %>%
    dplyr::group_by(!!sym(cleaning_event_col)) %>%
    dplyr::mutate(
      "{mean_col_name}" := mean(!!sym(pct_col_name), na.rm = TRUE),
      "{median_col_name}" := median(!!sym(pct_col_name), na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      "{grand_mean_col_name}" := mean(!!sym(pct_col_name), na.rm = TRUE),
      "{grand_median_col_name}" := median(!!sym(pct_col_name), na.rm = TRUE)
    )
  
  return(data)
}



