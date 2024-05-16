utils::globalVariables(c("n_i", "x_i", "N_i"))

#' Binomial Process Performance Calculation
#'
#' Performs a process performance calculation using binomial distribution. This includes
#' a bootstrap procedure to estimate the confidence interval of the Process Performance Index (Ppu).
#'
#' @param data A data frame containing the dataset.
#' @param residue_col Name of the column in `data` that contains the residue or defect counts.
#' @param cleaning_event_col Name of the column in `data` that groups data by cleaning event.
#' @param usl_col Name of the column in `data` that contains the Upper Specification Limit (USL) for each group.
#'
#' @return A data frame with the calculated Ppu and its 95% confidence interval (CI_Lower, CI_Upper),
#'         along with the method used ("Q-Binomial").
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#' @examples
#' # Assuming `data` is the dataframe with columns "Residue", "CleaningEvent", and "USL":
#' cv25_qbinom_ppu(Eq_Mic, "Mic", "CleaningEvent", "USL")
#'
#' @importFrom dplyr group_by summarise ungroup mutate n
#' @importFrom stats pbinom qnorm sd
#' @importFrom boot boot boot.ci
cv25_qbinom_ppu <- function(data, residue_col, cleaning_event_col, usl_col) {
  # Binomial process performance calculation
  Ppu_Binom <- function(data_subset) {
    USL <- data_subset[[usl_col]][1]
    data_P <- data_subset %>%
      group_by(.data[[cleaning_event_col]]) %>%
      summarise(n_i = n(), x_i = sum(.data[[residue_col]]), .groups = 'drop') %>%
      ungroup()
    
    data_P <- data_P %>%
      mutate(N_i = cumsum(n_i),
             X_i = cumsum(x_i),
             p_i = x_i / N_i)
    
    size <- sum(data_P$n_i)
    happen <- sum(data_P$x_i)
    p <- happen / size
    x_USL <- data_subset[[usl_col]][1]
    P_USL_B <- pbinom(x_USL, size, p)
    
    if (P_USL_B == 1) {
      Ppu_P_B <- 100
    } else {
      P_x <- pbinom(data_P$X_i, size = data_P$N_i, prob = p)
      q_P_x_USL_B <- qnorm(P_USL_B)
      q_P_x <- qnorm(P_x)
      q_mean <- mean(q_P_x[-1])
      q_sigma <- sd(q_P_x[-1])
      Ppu_P_B <- (q_P_x_USL_B - q_mean) / (3 * q_sigma)
    }
    
    return(Ppu_P_B)
  }
  
  boot_stat_function_Binom <- function(data, indices) {
    resampled_data <- data[indices, ]
    ppu_ft_resampled <- Ppu_Binom(resampled_data)
    return(ppu_ft_resampled)
  }
  
  boot_results_Binom <- boot(data = data, statistic = boot_stat_function_Binom, R = 1000)
  
  ci_Binom <- tryCatch({
    suppressMessages(boot.ci(boot_results_Binom, type = "bca"))
  }, error = function(e) {
    message("An error occurred in boot.ci")
    return(NULL)
  })
  
  ci_95_Binom <- if(!is.null(ci_Binom) && !is.null(ci_Binom$bca)) ci_Binom$bca[4:5] else c(NA, NA)
  results_df <- data.frame(
    Method = "Q-Binomial",
    Ppu = round(Ppu_Binom(data), 3),
    Lower_CI = ci_95_Binom[1],
    Upper_CI = ci_95_Binom[2]
  )
  
  return(results_df)
}