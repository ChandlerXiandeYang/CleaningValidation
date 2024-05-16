#' Calculate Ppu using Freeman's Transformation
#'
#' This function calculates the Process Performance Index (Ppu) using Freeman's transformation,
#' including a bootstrap method to estimate the confidence interval of Ppu.
#'
#' @param data A data frame containing the dataset.
#' @param residue_col The name of the column in `data` containing residue or defect counts.
#' @param cleaning_event_col The name of the column in `data` used for grouping data by cleaning event.
#' @param usl_col The name of the column in `data` that contains the Upper Specification Limit (USL).
#' @return A data frame with columns for the Method, Ppu, CI_Lower, and CI_Upper.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#' @examples
#' cv28_freeman_ppu(Eq_Mic, "Mic", "CleaningEvent", "USL")
#' @importFrom dplyr mutate
#' @importFrom boot boot boot.ci
cv28_freeman_ppu <- function(data, residue_col, cleaning_event_col, usl_col) {
  
  Ppu_FT <- function(data_subset) {
    data_subset <- mutate(data_subset, y_i_FT = sqrt(!!sym(residue_col)) + sqrt(!!sym(residue_col) + 1))
    USL_FT <- sqrt(data_subset[[usl_col]][1]) + sqrt(data_subset[[usl_col]][1] + 1)
    
    mean_FT <- 2 * mean(data_subset[[residue_col]])
    sigma_FT <- 1 # Based on the provided formula
    
    Ppu_FT <- (USL_FT - mean_FT) / (3 * sigma_FT)
    
    return(Ppu_FT)
  }
  
  Ppu_F <- round(Ppu_FT(data), 3)

  boot_stat_function_FT <- function(data, indices) {
    resampled_data <- data[indices, , drop = FALSE]
    ppu_ft_resampled <- Ppu_FT(resampled_data)
    
    return(ppu_ft_resampled)
  }

  boot_results_FT <- boot(data = data, statistic = boot_stat_function_FT, R = 1000, sim = "ordinary")

  ci_FT <- suppressMessages(boot.ci(boot_results_FT, type = "bca"))
  ci_95_FT <- if(!is.null(ci_FT$bca)) ci_FT$bca[4:5] else c(NA, NA)

  results_df <- data.frame(
    Method = "Freeman",
    Ppu = Ppu_F,
    CI_Lower = ci_95_FT[1],
    CI_Upper = ci_95_FT[2]
  )
  
  return(results_df)
}
