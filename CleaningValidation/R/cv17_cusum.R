#' Create a CUSUM Chart for Poisson-distributed Data
#'
#' This function computes the cumulative sum (CUSUM) for the mean values of 
#' a specified residue column aggregated by a cleaning event column. It then 
#' generates a CUSUM chart for visualizing the stability of a process based on 
#' a Poisson distribution. The reference value `k` can be provided; if not,
#' it defaults to half of the process average lambda.
#'
#' @param data A data frame containing the dataset for analysis.
#' @param residue_col The name of the column representing residue data.
#' @param cleaning_event_col The name of the column representing cleaning events.
#' @param k The reference value used in calculating CUSUM, by default it is set to half of lambda.
#'
#' @return A ggplot object representing the CUSUM chart.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#'
#' @examples
#' # To create a CUSUM chart with default k value
#' cv17_cusum(data = Eq_Mic, residue_col = "Mic", cleaning_event_col = "CleaningEvent")
#'
#' # To create a CUSUM chart with a specified k value
#' cv17_cusum(data = Eq_Mic, residue_col = "Mic", cleaning_event_col = "CleaningEvent", k = 0.75)
#' @importFrom dplyr group_by_at summarise mutate
#' @importFrom ggplot2 ggplot geom_line geom_point geom_hline geom_text labs theme_minimal theme
#' @importFrom rlang sym .data
cv17_cusum <- function(data, residue_col, cleaning_event_col, k=NULL) {
  mean_counts <- data %>%
    group_by_at(vars(cleaning_event_col)) %>%
    summarise(Mean_Residue = mean(!!sym(residue_col), na.rm = TRUE), .groups = 'drop')
  names(mean_counts)[names(mean_counts) == "Mean_Residue"] <- paste0("Mean_", residue_col)
  lambda <- mean(mean_counts[[paste0("Mean_", residue_col)]])
  if (is.null(k)) { k <- lambda / 2 }
  cusum <- numeric(nrow(mean_counts))
  cusum[1] <- 0  
  for (i in 2:length(cusum)) {
    cusum[i] <- max(0, cusum[i-1] + mean_counts[[paste0("Mean_", residue_col)]][i] - lambda - k)
  }
  h <- round(3 * sqrt(lambda), 3)  
  cusum_df <- data.frame(CleaningEvent = mean_counts[[cleaning_event_col]], CUSUM = cusum)
  cusum_plot <- ggplot(cusum_df,  aes(x = .data[[cleaning_event_col]], y = .data[["CUSUM"]])) +
    geom_line(group=1) +
    geom_point() +
    geom_hline(yintercept = h, color = "red", linetype = "dashed", linewidth=0.5) +
    geom_text(aes(x = Inf, y = h, label = paste("UCL =", h)), color = "red", hjust = 1.2, vjust = 1.3) +
    labs(title = paste("Poisson CUSUM Chart for Mean", residue_col), x = cleaning_event_col, y = "CUSUM") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  return(cusum_plot)
}
