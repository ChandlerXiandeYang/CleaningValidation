#' Calculate Mic Ppu with Five Methods
#'
#' This function calculates the process performance index (Ppu) for Mic using five different methods,
#' including Q-Binomial, Q-Poisson, Anscombe, Freeman, and KDE. It returns a dataframe with the Ppu
#' values, lower and upper confidence intervals for each method, and appends a row for the method
#' with the minimum Ppu value.
#'
#' @param data A dataframe containing the dataset.
#' @param residue_col The name of the column in `data` that contains the residue values.
#' @param cleaning_event_col The name of the column in `data` that contains the cleaning event identifiers.
#' @param usl_col The name of the column in `data` that contains the Upper Specification Limit values.
#' @return A dataframe with the Ppu calculations for each method and the minimum Ppu method.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @importFrom utils head
#' @export
#' @examples
#' \donttest{
#'   MicPPU <- cv29_mic_ppu(Eq_Mic, "Mic", "CleaningEvent", "USL")
#' }
cv29_mic_ppu <- function(data, residue_col, cleaning_event_col, usl_col) {
  method_functions <- list(
    cv25_qbinom_ppu,
    cv26_qpoisson_ppu,
    cv27_anscombe_ppu,
    cv28_freeman_ppu,
    cv12_kde_ppu
  )
  combined_results <- data.frame(Method = character(), 
                                 Ppu = numeric(), 
                                 Lower_CI = numeric(), 
                                 Upper_CI = numeric(), 
                                 stringsAsFactors = FALSE)
  
  for (func in method_functions) {
    method_result <- func(data, residue_col, cleaning_event_col, usl_col)
    colnames(method_result) <- c("Method", "Ppu", "Lower_CI", "Upper_CI")
    combined_results <- rbind(combined_results, method_result)
  }
  combined_results$Ppu <- round(as.numeric(combined_results$Ppu), 3)
  combined_results$Lower_CI <- round(as.numeric(combined_results$Lower_CI), 3)
  combined_results$Upper_CI <- round(as.numeric(combined_results$Upper_CI), 3)
  Ppu_min <- min(combined_results$Ppu, na.rm = TRUE)
  min_method_row <- combined_results[which.min(combined_results$Ppu), ]
  min_method_row$Method <- "Min"
  combined_results <- rbind(combined_results, min_method_row)
  return(combined_results)
}
