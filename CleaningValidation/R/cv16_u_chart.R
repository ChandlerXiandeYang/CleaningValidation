#' Create a u-Chart for Poisson-distributed Data
#'
#' This function generates a u-chart for visualizing the stability and
#' capability of a process based on a Poisson distribution.
#'
#' @param data A data frame containing the data for plotting.
#' @param residue_col The name of the column representing residue data (numeric).
#' @param cleaning_event_col The name of the column representing cleaning events (factor or character).
#'
#' @return A ggplot object representing the u-chart.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#'
#' @examples
#' cv16_u_chart(data = Eq_Mic, residue_col = "Mic", cleaning_event_col = "CleaningEvent")
#' @import ggplot2
#' @importFrom dplyr group_by summarise mutate
#' @importFrom rlang sym as_label
#' @importFrom graphics text
cv16_u_chart <- function(data, residue_col, cleaning_event_col) {
  residue_sym <- sym(residue_col)
  cleaning_event_sym <- sym(cleaning_event_col)
  residue_mean_sym <- sym(paste0(residue_col, "_Mean"))

  event_stats <- data %>%
    group_by(!!cleaning_event_sym) %>%
    summarise(!!residue_mean_sym := mean(!!residue_sym, na.rm = TRUE), .groups = 'drop')

  grand_mean <- mean(data[[residue_col]], na.rm = TRUE)
  lambda <- grand_mean
  n <- 1  # Assuming each unit is a single observation
  ucl <- lambda + 3 * sqrt(lambda / n)
  
  # Adding UCL to the event statistics
  event_stats <- mutate(event_stats, UCL = ucl)
  
  u_chart <- ggplot(event_stats, aes(x = !!cleaning_event_sym, y = !!residue_mean_sym)) +
    geom_line(group = 1) +
    geom_point() +
    geom_hline(yintercept = ucl, color = "red", linetype = "dashed") +
    geom_hline(yintercept = lambda, color = "green") +
    annotate("text", x = Inf, y = ucl, label = paste("UCL =", round(ucl, 3)), hjust = 1.1, vjust = 1.3, color = "red", size = 3.5) +
    annotate("text", x = Inf, y = lambda, label = paste("Mean =", round(lambda, 3)), hjust = 1.1, vjust = -0.2, color = "green", size = 3.5) +
    labs(title = paste("Poisson u-Chart for Mean", residue_col),
         x = as_label(cleaning_event_sym),
         y = paste("Mean", residue_col, "per Swab")) +
    theme_minimal() +
    theme(
      plot.margin = unit(c(1, 3, 1, 1), "lines"),
      plot.title = element_text(hjust = 0.5)
    )

  y_range <- range(c(0, max(event_stats[[paste0(residue_col, "_Mean")]], na.rm = TRUE), ucl))
  u_chart <- u_chart + scale_y_continuous(limits = y_range)
  
  return(u_chart)
}
