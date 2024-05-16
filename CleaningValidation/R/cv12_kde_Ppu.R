#' Calculate PPU using KDE density estimation
#'
#' @param data The dataset containing the columns specified in other parameters.
#' @param residue_col The name of the column containing residue data.
#' @param cleaning_event_col The name of the column containing cleaning event data (unused).
#' @param usl_col The name of the column containing USL values.
#' @param n_bootstrap The number of bootstrap samples to use.
#' @return A dataframe with the estimated PPU and its 95% confidence interval.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#' @importFrom stats density approxfun uniroot
#' @importFrom boot boot boot.ci
#' @examples
#'  Eq_DAR <- cv03_usl_unification(data = Eq_DAR, cleaning_event_col = "CleaningEvent", 
#' residue_col = "DAR", usl_col = "USL")
#' cv12_kde_ppu(data = Eq_DAR, residue_col = "DAR_Pct", cleaning_event_col = "CleaningEvent",
#'  usl_col = "USL_Pct", n_bootstrap = 1000)
cv12_kde_ppu <- function(data, residue_col, cleaning_event_col, usl_col, n_bootstrap = 1000) {
  
  calculate_ppu_kde_density <- function(sample_data, USL, Median_value) {
    kde_result <- density(sample_data, na.rm = TRUE, bw = "nrd0")
    cdf <- approxfun(kde_result$x, cumsum(kde_result$y) / sum(kde_result$y), method = "linear", yleft = 0, yright = 1)
    P_50_i <- uniroot(function(x) cdf(x) - 0.5, lower = min(kde_result$x), upper = max(kde_result$x))$root
    P_99865_i <- uniroot(function(x) cdf(x) - 0.99865, lower = min(kde_result$x), upper = max(kde_result$x))$root
    Ppu <- (USL - Median_value) / (P_99865_i - P_50_i)
    return(Ppu)
  }
  
  calculate_ppu_kde_bootstrap <- function(data, indices, residue_col, USL, Median_value) {
    sample_data <- data[indices, residue_col, drop = FALSE][[1]] 
    Ppu_99865 <- calculate_ppu_kde_density(sample_data, USL, Median_value)
    return(Ppu_99865)
  }
  USL <- data[[usl_col]][1]
  Median_value <- median(data[[residue_col]], na.rm = TRUE)
  bootstrap_results <- boot(data = data, 
                            statistic = function(data, indices) calculate_ppu_kde_bootstrap(data, indices, residue_col, USL, Median_value), 
                            R = n_bootstrap)
  CI <- boot.ci(bootstrap_results, type = "perc")
  results_Ppu <- data.frame(
    Method="KDE",
    Ppu = round(bootstrap_results$t0, 3),
    Lower_CI = round(CI$perc[4], 3),
    Upper_CI = round(CI$perc[5], 3)
  )
  
  return(results_Ppu)
}

