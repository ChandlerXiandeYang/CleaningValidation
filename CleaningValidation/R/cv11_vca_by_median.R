#' Variability Components Analysis by Median with Bootstrap
#'
#' Perform Variability Components Analysis (VCA) by median for residue percentages based on cleaning events with bootstrap for confidence intervals.
#'
#' @param data A data frame containing the data.
#' @param residue_col The name of the column containing residue percentages.
#' @param cleaning_event_col The name of the column containing cleaning event identifiers.
#' @param n_bootstrap The number of bootstrap iterations. Default is 2000.
#' @return A data frame summarizing variability components analysis by median along with confidence intervals from bootstrap.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#' @examples
#' # Assuming 'Eq_DAR' is the data frame, 'DAR_Pct' is the residue column, 
#' # and 'CleaningEvent' is the cleaning event column.
#' Eq_DAR <- cv01_dfclean(data=Eq_DAR, cleaning_event_col="CleaningEvent",
#' residue_col="DAR", usl_col="USL" ) 
#' Eq_DAR <- cv03_usl_unification(data=Eq_DAR, cleaning_event_col="CleaningEvent", 
#' residue_col="DAR", usl_col="USL")
#' summary <- cv11_vca_by_median(data = Eq_DAR, residue_col = "DAR_Pct", 
#' cleaning_event_col = "CleaningEvent", n_bootstrap = 2000)
#' @importFrom boot boot
#' @importFrom stats median aggregate quantile as.formula
cv11_vca_by_median <-function(data, residue_col, cleaning_event_col, n_bootstrap = 2000) {
    # Function for VCA by median
    vca_by_median <- function(data, residue_col, cleaning_event_col) {
      within_var <- aggregate(as.formula(paste(residue_col, "~", cleaning_event_col)), data = data, FUN = function(x) {
        n <- length(x)
        if (n > 1) {
          med <- median(x)
          sum((x - med)^2) / (n - 1)
        } else {
          return(0)
        }
      })
      
      mean_within_var <- mean(within_var[, 2])
      grand_median <- median(data[[residue_col]])
      group_medians <- aggregate(as.formula(paste(residue_col, "~", cleaning_event_col)), data = data, FUN = median)
      
      n_groups <- nrow(group_medians)
      if (n_groups > 1) {
        between_var <- sum((group_medians[, 2] - grand_median)^2) / (n_groups - 1)
      } else {
        between_var <- 0
      }
      
      total_variability <- mean_within_var + between_var
      percentage_within <- ifelse(total_variability == 0, 0, round((mean_within_var / total_variability) * 100, 3))
      percentage_between <- ifelse(total_variability == 0, 0, round((between_var / total_variability) * 100, 3))
      
      summary_df <- data.frame(
        Component = c("Between CleaningEvents", "Within CleaningEvents", "Total"),
        Var_Comp = round(c(between_var, mean_within_var, total_variability), 3),
        Perc_Total = c(percentage_between, percentage_within, percentage_between + percentage_within),
        Sqrt_Var = round(sqrt(c(between_var, mean_within_var, total_variability)), 3),
        CI_Lower = c("", "", ""),
        CI_Upper = c("", "", "")
      )
      
      return(summary_df)
    }
    
    # Function for bootstrap VCA
    bootstrap_vca_by_median <- function(original_data, residue_col, cleaning_event_col, n_bootstrap) {
      bootstrap_results <- vector("list", n_bootstrap)
      for (i in seq_len(n_bootstrap)) {
        sampled_data <- original_data[sample(nrow(original_data), size = nrow(original_data), replace = TRUE), ]
        vca_result <- vca_by_median(sampled_data, residue_col, cleaning_event_col)
        bootstrap_results[[i]] <- vca_result
      }
      
      results_df <- do.call(rbind, bootstrap_results)
      ci_between <- quantile(results_df[results_df$Component == "Between CleaningEvents", "Var_Comp"], 
                             probs = c(0.025, 0.975), na.rm = TRUE)
      ci_within <- quantile(results_df[results_df$Component == "Within CleaningEvents", "Var_Comp"], 
                            probs = c(0.025, 0.975), na.rm = TRUE)
      ci_between <- if(all(is.na(ci_between))) { c(0, 0) } else { round(ci_between, 3) }
      ci_within <- if(all(is.na(ci_within))) { c(0, 0) } else { round(ci_within, 3) }
      return(list(ci_between = ci_between, ci_within = ci_within))
    }
    
    # Perform VCA by median
    vca_result <- vca_by_median(data, residue_col, cleaning_event_col)
    
    # Perform bootstrap analysis
    bootstrap_results <- bootstrap_vca_by_median(data, residue_col, cleaning_event_col, n_bootstrap)
    
    # Combine results
    final_summary <- vca_result
    final_summary$CI_Lower[1] <- bootstrap_results$ci_between[1]
    final_summary$CI_Upper[1] <- bootstrap_results$ci_between[2]
    final_summary$CI_Lower[2] <- bootstrap_results$ci_within[1]
    final_summary$CI_Upper[2] <- bootstrap_results$ci_within[2]
    
    return(final_summary)
  }