#' Calculate Process Performance Index using Poisson Distribution
#'
#' This function calculates the Process Performance Index (Ppu) for data 
#' assumed to follow a Poisson distribution. It includes a bootstrap method 
#' for estimating the confidence interval of the Ppu.
#'
#' @param data A data frame containing the dataset.
#' @param residue_col Name of the column in `data` containing residue counts.
#' @param cleaning_event_col Name of the column in `data` used to group data by cleaning event.
#' @param usl_col Name of the column in `data` that contains the Upper Specification Limit (USL) for each group.
#' @return A data frame with columns Method, Ppu, CI_Lower, and CI_Upper.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#' @examples
#' cv26_qpoisson_ppu(Eq_Mic, "Mic", "CleaningEvent", "USL")
#' @importFrom dplyr group_by summarise ungroup
#' @importFrom stats ppois qnorm sd
#' @importFrom boot boot.ci 
cv26_qpoisson_ppu <- function(data, residue_col, cleaning_event_col, usl_col) {
  Ppu_Poisson <- function(data_subset) {
      data_P <- data_subset %>%
      group_by(!!sym(cleaning_event_col)) %>%
      summarise(n_i = n(), x_i = sum(!!sym(residue_col))) %>%
      ungroup()

    lambda <- mean(data_P$x_i)

    x <- data_subset[[usl_col]][1]

    P_USL <- ppois(x, lambda)

    if(P_USL == 1) {
      Ppu_P <- 100
    } else {
      quan_P_USL <- qnorm(P_USL)
      P_x_i <- ppois(data_P$x_i, lambda)
      quan_x_i <- qnorm(P_x_i)
      quan_mean <- mean(quan_x_i)
      quan_sigma <- sd(quan_x_i)
      Ppu_P <- (quan_P_USL - quan_mean) / (3 * quan_sigma)
    }
    
    return(Ppu_P)
  }
  
  Ppu_P <- round(Ppu_Poisson(data), 3)

  boot_stat_function_Poisson <- function(data, indices) {
    resampled_data <- data[indices, , drop = FALSE]
    ppu_ft_resampled <- Ppu_Poisson(resampled_data)
    return(ppu_ft_resampled)
  }

  boot_results_Poisson <- boot(data = data, statistic = boot_stat_function_Poisson, R = 1000)
  
  ci_Poisson <- suppressMessages(boot.ci(boot_results_Poisson, type = "bca"))
  ci_95_Poisson <- if(!is.null(ci_Poisson$bca)) ci_Poisson$bca[4:5] else c(NA, NA)

  results_df <- data.frame(
    Method = "Q-Poisson",
    Ppu = Ppu_P,
    CI_Lower = ci_95_Poisson[1],
    CI_Upper = ci_95_Poisson[2]
  )
  
  return(results_df)
}

