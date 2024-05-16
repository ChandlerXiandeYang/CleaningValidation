utils::globalVariables(c("EWMA"))
#' Exponentially Weighted Moving Average (EWMA) Chart
#'
#' Generates an EWMA chart for a specified residue column grouped by cleaning events.
#'
#' @param data A data frame containing the data set for analysis.
#' @param residue_col The name of the column representing residue data.
#' @param cleaning_event_col The name of the column representing cleaning events.
#' @param alpha The smoothing parameter for the EWMA calculation, default is 0.2.
#'
#' @return A ggplot object representing the EWMA chart.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#' @examples
#' # Assuming 'Eq_Mic' is a data frame, 'Mic' is the residue column of interest,
#' # and 'CleaningEvent' is the column representing cleaning events.
#' ewma_plot <- cv18_ewma(data = Eq_Mic, residue_col = "Mic", cleaning_event_col = "CleaningEvent")
#' print(ewma_plot)
#' @importFrom dplyr group_by summarise ungroup mutate
#' @importFrom ggplot2 ggplot geom_line geom_point geom_hline geom_text labs theme
#' @importFrom rlang sym .data
cv18_ewma <- function(data, residue_col, cleaning_event_col, alpha = 0.2) {

  if (!is.character(residue_col) || !is.character(cleaning_event_col)) {
    stop("residue_col and cleaning_event_col must be character strings.")
  }
  mean_counts <- data %>%
    dplyr::group_by(!!rlang::sym(cleaning_event_col)) %>%
    dplyr::summarise(Mean_Residue = mean(!!rlang::sym(residue_col), na.rm = TRUE)) %>%
    dplyr::ungroup()

  col_name <- paste0("Mean_", residue_col)
  names(mean_counts)[names(mean_counts) == "Mean_Residue"] <- col_name

  ewma <- numeric(length(mean_counts[[col_name]]))
  ewma[1] <- mean_counts[[col_name]][1]

  for (i in 2:length(ewma)) {
    ewma[i] <- alpha * mean_counts[[col_name]][i] + (1 - alpha) * ewma[i - 1]
  }

  ewma_df <- data.frame(CleaningEvent = mean_counts[[cleaning_event_col]], EWMA = ewma)

  ewma_plot <- ggplot(ewma_df, aes(x = .data[[cleaning_event_col]], y = EWMA)) +
    geom_line(group=1) +
    geom_point() +
    geom_hline(yintercept = max(ewma, na.rm = TRUE), linetype = "dashed", color = "red") +
    geom_text(aes(x = Inf, y = max(ewma, na.rm = TRUE), label = paste("UCL =", round(max(ewma, na.rm = TRUE), 3))), color = "red", hjust = 1.1, vjust = 1.3) +
    labs(title = paste("EWMA Chart for Mean", residue_col), x = cleaning_event_col, y = "EWMA") +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5)) # Center the plot title
  
  return(ewma_plot)
}

