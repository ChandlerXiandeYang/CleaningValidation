#' Median Control Chart
#'
#' This function creates a control chart for the median residue percentages based on kernel density estimation. The in put residue_pct_meidan_col 
#' can be median of non-USL_unified variable such as Mic_Median, DAR_Median, or CAR_Median
#'
#' @param data A data frame containing the data to plot.
#' @param cleaning_event_col The name of the column containing cleaning event identifiers.
#' @param residue_pct_median_col The name of the column containing the calculated median residue percentages.
#' @importFrom ggplot2 ggplot geom_rect aes geom_point geom_line geom_hline scale_color_manual labs theme_minimal theme element_text geom_text
#' @importFrom dplyr %>% mutate arrange
#' @importFrom stats density uniroot 
#' @return The meidan control chart.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#' @examples
#' # Assuming 'Eq_DAR' is a data frame with appropriate columns:
#' Eq_DAR <- cv03_usl_unification(data = Eq_DAR,  "CleaningEvent",  "DAR",  "USL")
#' cv07_median_control_chart(data = Eq_DAR, cleaning_event_col = "CleaningEvent", 
#' residue_pct_median_col="DAR_Pct_Median")
cv071_median_control_chart <- function(data,  cleaning_event_col, residue_pct_median_col) {
  
  stopifnot(is.data.frame(data))
  stopifnot(cleaning_event_col %in% names(data))
  stopifnot(residue_pct_median_col %in% names(data))
  
  kde <- density(data[[residue_pct_median_col]], na.rm = TRUE) 
  kde_data <- data.frame(x = kde$x, y = kde$y)
  kde_cdf <- approxfun(kde$x, cumsum(kde$y) / sum(kde$y))
  
  P0_5 <- uniroot(function(x) kde_cdf(x) - 0.5, lower = min(kde$x), upper = max(kde$x))$root
  UCL <- uniroot(function(x) kde_cdf(x) - 0.99865, lower = min(kde$x), upper = max(kde$x))$root
  P0_9772 <- uniroot(function(x) kde_cdf(x) - 0.97725, lower = min(kde$x), upper = max(kde$x))$root
  P0_84135 <- uniroot(function(x) kde_cdf(x) - 0.84135, lower = min(kde$x), upper = max(kde$x))$root
  median_value <- median(data[[residue_pct_median_col]], na.rm = TRUE)
  
  UCL_label <- sprintf("UCL=P0.99865: %.2f", UCL)
  P0_9772_label <- sprintf("P0.97725: %.2f", P0_9772)
  P0_84135_label <- sprintf("P0.84135: %.2f", P0_84135)
  median_label <- sprintf("Median: %.2f", median_value)
  
  median_chart <- ggplot(data, aes(x = .data[[cleaning_event_col]], y = .data[[residue_pct_median_col]])) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = UCL, ymax = UCL+2), fill = "pink", alpha = 0.2) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = P0_9772, ymax = UCL), fill = "yellow", alpha = 0.2) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = P0_84135, ymax = P0_9772), fill = "yellow", alpha = 0.2) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = P0_84135), fill = "palegreen", alpha = 0.2) +
    geom_point(size = 0.8) +
    geom_line(aes(group = 1), color = "black", size = 0.5) +
    geom_hline(yintercept = UCL, color = "red", linetype = "dashed") +
    geom_hline(yintercept = P0_9772, color = "darkgreen", linetype = "dashed") +
    geom_hline(yintercept = P0_84135, color = "blue", linetype = "dashed") +
    geom_hline(yintercept = median_value, color = "black", linetype = "dashed") +
    scale_color_manual(values = c("red" = "red", "black" = "black")) +
    labs(title = "Median Chart", x = cleaning_event_col, y = residue_pct_median_col) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10, face = "plain"),
          legend.position = "none",
          axis.text = element_text(size = 10, face = "plain"),
          axis.title = element_text(size = 11, face = "plain")) +
    geom_text(aes(label = UCL_label, x = Inf, y = UCL+1), color = "red", hjust = 1.2, vjust = -0.2, size = 2, nudge_y = 0.02) +
    geom_text(aes(label = P0_9772_label, x = Inf, y = P0_9772), color = "darkgreen", hjust = 1.2, vjust = -0.2, size = 2, nudge_y = 0.02) +
    geom_text(aes(label = P0_84135_label, x = Inf, y = P0_84135), color = "blue", hjust = 1.2, vjust = -0.2, size = 2, nudge_y = 0.02) +
    geom_text(aes(label = median_label, x = Inf, y = median_value), color = "black", hjust = 1.2, vjust = -0.2, size = 2, nudge_y = 0.02)
  
  return(median_chart)
}