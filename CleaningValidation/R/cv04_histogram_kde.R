utils::globalVariables(c("x", "y"))
#' Plot Histogram with Kernel Density Estimate Curve
#'
#' This function takes a dataset and a column representing the residue percentages
#' and generates a histogram overlaid with a KDE (Kernel Density Estimate) curve.
#' It calculates and marks quantiles P0.5, P0.8413, P0.9772, and the P0.99865, i.e., UCL (Upper Control Limit)
#' on the plot.
#'
#' @param data A dataframe containing the relevant dataset.
#' @param residue_pct_col The name of the column in `data` that contains the residue percentages.
#' @return A ggplot object representing the histogram with KDE curve.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @import ggplot2
#' @importFrom stats density approxfun uniroot
#' @export
#' @examples
#' Eq_DAR <- cv03_usl_unification(data=Eq_DAR,"CleaningEvent", "DAR", usl_col="USL")
#' cv04_histogram_kde(data = Eq_DAR, residue_pct_col = "DAR_Pct")
cv04_histogram_kde <- function(data, residue_pct_col) {
  kde_result <- stats::density(data[[residue_pct_col]], na.rm = TRUE)
  kde_data <- data.frame(x = kde_result$x, y = kde_result$y)
  kde_cdf <- stats::approxfun(kde_result$x, cumsum(kde_result$y) / sum(kde_result$y))

  P0_5 <- uniroot(function(x) kde_cdf(x) - 0.5, lower = min(kde_result$x), upper = max(kde_result$x))$root
  UCL_line <- uniroot(function(x) kde_cdf(x) - 0.99865, lower = min(kde_result$x), upper = max(kde_result$x))$root
  P0_9772 <- uniroot(function(x) kde_cdf(x) - 0.9772, lower = min(kde_result$x), upper = max(kde_result$x))$root
  P0_8413 <- uniroot(function(x) kde_cdf(x) - 0.8413, lower = min(kde_result$x), upper = max(kde_result$x))$root

  y_position_pct <- max(kde_data$y) * 1.2

  y_limit <- y_position_pct * 1.3

  HistDen_Plot <- ggplot(data, aes(x = .data[[residue_pct_col]])) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "purple", alpha = 0.5) +
    geom_line(data = kde_data, aes(x = x, y = y), color = "blue") +
    geom_vline(xintercept = P0_5, color = "black", linetype = "dashed") +
    geom_vline(xintercept = UCL_line, color = "red", linetype = "dashed") +
    geom_vline(xintercept = P0_9772, color = "dark green", linetype = "dashed") +
    geom_vline(xintercept = P0_8413, color = "blue", linetype = "dashed") +
    geom_text(label = "P0.5", x = P0_5, y = y_position_pct, hjust = 1.2, vjust = 1.2) +
    geom_text(label = "P0.8413", x = P0_8413, y = 0.9*y_position_pct, hjust = -0.1, vjust = 1.2) +
    geom_text(label = "P0.9772", x = P0_9772, y = 0.9*y_position_pct, hjust = -0.1, vjust = 1.2) +
    geom_text(label = "UCL", x = UCL_line, y = y_position_pct, hjust = 0, vjust = 1.2) +
    labs(title = paste(residue_pct_col, "Histogram with KDE Curve")) +
    theme_minimal() +
    theme(plot.title = element_text(size = rel(0.8), face = "bold", hjust = 0.5)) +
    ylim(0, y_limit)
  
  return(HistDen_Plot)
}