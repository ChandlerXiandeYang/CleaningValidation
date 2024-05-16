#' Process Performance Calculation using Anscombe's Transformation
#'
#' Calculates the Process Performance Index (Ppu) using Anscombe's transformation. 
#' This function also performs a bootstrap to estimate the confidence interval of Ppu.
#'
#' @param data A data frame containing the dataset.
#' @param residue_col Name of the column in `data` containing residue or defect counts.
#' @param cleaning_event_col Name of the column in `data` used for grouping by cleaning event.
#' @param usl_col Name of the column in `data` that contains the Upper Specification Limit (USL).
#' @return A data frame with columns for the Method, Ppu, CI_Lower, and CI_Upper.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#' @examples
#' cv27_anscombe_ppu(Eq_Mic, "Mic", "CleaningEvent", "USL")
#' @importFrom dplyr mutate
#' @importFrom boot boot boot.ci
cv27_anscombe_ppu <- function(data, residue_col, cleaning_event_col, usl_col) {

  Ppu_Ans <- function(data_subset) {
    data_subset <- mutate(data_subset, y_i = sqrt(!!sym(residue_col) + 3/8))
    USL_Ans <- sqrt(data_subset[[usl_col]][1] + 3/8)
    
    mean_Ans <- mean(data_subset$y_i) - 1 / (8 * mean(data_subset[[residue_col]]))
    sigma_Ans <- sqrt(0.25) # Based on transformation
    
    Ppu_Ans <- (USL_Ans - mean_Ans) / (3 * sigma_Ans)
    
    return(Ppu_Ans)
  }
  
  Ppu_A <- round(Ppu_Ans(data), 3)
  boot_stat_function_A <- function(data, indices) {
    resampled_data <- data[indices, , drop = FALSE]
    ppu_ans_resampled <- Ppu_Ans(resampled_data)
    return(ppu_ans_resampled)
  }
  
  boot_results_A <- boot(data = data, statistic = boot_stat_function_A, R = 1000)

  ci_A <- suppressMessages(boot.ci(boot_results_A, type = "bca"))
  ci_95_A <- if(!is.null(ci_A$bca)) ci_A$bca[4:5] else c(NA, NA)

  results_df <- data.frame(
    Method = "Anscombe",
    Ppu = Ppu_A,
    CI_Lower = ci_95_A[1],
    CI_Upper = ci_95_A[2]
  )
  
  return(results_df)
}

