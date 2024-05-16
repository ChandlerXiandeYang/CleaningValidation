utils::globalVariables(c("Upper_Bound", "Status"))
#' Variability Chart for Cleaning Events
#'
#' This function generates a variability chart for cleaning events, showing data points, outliers,
#' and overall statistics like the grand mean and median.
#'
#' @param data A data frame containing the data to plot.
#' @param cleaning_event_col Name of the column representing cleaning events (as a string).
#' @param residue_pct_col Name of the column representing residue percentages (as a string).
#' @param usl_pct_col Name of the column representing the upper specification limit percentages (as a string).
#' @importFrom dplyr filter group_by mutate summarise ungroup case_when
#' @importFrom ggplot2 aes ggplot geom_boxplot geom_point geom_hline geom_text labs theme_minimal
#' @importFrom stats IQR quantile
#' @export
#'
#' @return A ggplot object representing the variability chart.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @examples
#' Eq_DAR <- cv01_dfclean(data=Eq_DAR, cleaning_event_col="CleaningEvent", 
#' residue_col="DAR", usl_col="USL" ) 
#' Eq_DAR <- cv03_usl_unification(data=Eq_DAR, cleaning_event_col="CleaningEvent", 
#' residue_col="DAR", usl_col="USL")
#' cv08_variability_chart(data=Eq_DAR, cleaning_event_col="CleaningEvent", 
#' residue_pct_col="DAR_Pct", usl_pct_col="USL_Pct")
cv08_variability_chart <- function(data, cleaning_event_col, residue_pct_col, usl_pct_col) {
  cleaning_event_sym <- rlang::sym(cleaning_event_col)
  residue_pct_sym <- rlang::sym(residue_pct_col)
  usl_pct_sym <- rlang::sym(usl_pct_col)
  
  grand_mean_value <- mean(data[[residue_pct_col]], na.rm = TRUE)
  grand_median_value <- median(data[[residue_pct_col]], na.rm = TRUE)
  
  data <- data %>%
    dplyr::group_by(!!cleaning_event_sym) %>%
    dplyr::mutate(
      IQR = stats::IQR(!!residue_pct_sym, na.rm = TRUE),
      Upper_Bound = stats::quantile(!!residue_pct_sym, 0.75, na.rm = TRUE) + (1.5 * IQR),
      Upper_Outlier = !!residue_pct_sym > Upper_Bound,
      Status = dplyr::case_when(
        !!residue_pct_sym >= !!usl_pct_sym ~ "OOS",
        Upper_Outlier ~ "Outlier",
        TRUE ~ "In Spec"
      )
    ) %>%
    dplyr::ungroup()
  
  last_event_data <- data %>% 
    dplyr::filter(!!cleaning_event_sym == max(!!cleaning_event_sym))
  
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!cleaning_event_sym, y = !!residue_pct_sym)) +
    ggplot2::geom_boxplot(ggplot2::aes(group = !!cleaning_event_sym), outlier.shape = NA) +
    ggplot2::geom_point(ggplot2::aes(color = Status), size = 1.5, alpha = 0.5) +
    ggplot2::stat_summary(fun = median, geom = "line", ggplot2::aes(group = 1), color = "blue", size = 0.5) +
    ggplot2::scale_color_manual(values = c("OOS" = "purple", "Outlier" = "red", "In Spec" = "green")) +
    ggplot2::geom_hline(yintercept = grand_mean_value, linetype = "dashed", color = "purple", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = grand_median_value, linetype = "dotdash", color = "dark green", linewidth = 0.5) +
    ggplot2::geom_text(data = last_event_data,
                       ggplot2::aes(label = paste("Grand Mean:", round(grand_mean_value, 2)), y = grand_mean_value), 
                       hjust = 1.5, vjust = -1, colour = "purple", size = 3, check_overlap = TRUE) +
    ggplot2::geom_text(data = last_event_data,
                       ggplot2::aes(label = paste("Grand Median:", round(grand_median_value, 2)), y = grand_median_value), 
                       hjust = 1.1, vjust = -0.5, colour = "dark green", size = 3, check_overlap = TRUE) +
    ggplot2::labs(title = paste(residue_pct_col, "Variability Chart"), x = "Cleaning Event", y = residue_pct_col) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(0.8), face = "bold", hjust = 0.5))
  
  return(plot)
}