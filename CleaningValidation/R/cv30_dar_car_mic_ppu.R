#' Calculate DAR, CAR, and Mic Ppu Values and Identify the Overall Minimum
#'
#' This function calculates Ppu values for DAR, CAR, and Mic using the KDE method provided by the `cv12_kde_ppu` function. It then uses the `cv29_mic_ppu` function to calculate combined Ppu for Mic and extract the method with the minimum Ppu value. The function assumes the availability of the datasets `Eq_DAR`, `Eq_CAR`, and `Eq_Mic` that conform to expected column naming conventions and data structures. It is reliant on the results of the `cv12_kde_ppu` and `cv29_mic_ppu` functions being consistent and correctly formatted.
#'
#'
#' @param dar_data A dataframe containing DAR data.
#' @param dar_residue_col The name of the DAR residue column.
#' @param dar_cleaning_event_col The name of the DAR cleaning event identifier column.
#' @param dar_usl_col The name of the DAR Upper Specification Limit column.
#' @param car_data A dataframe containing CAR data.
#' @param car_residue_col The name of the CAR residue column.
#' @param car_cleaning_event_col The name of the CAR cleaning event identifier column.
#' @param car_usl_col The name of the CAR Upper Specification Limit column.
#' @param mic_data A dataframe containing Mic data.
#' @param mic_residue_col The name of the Mic residue column.
#' @param mic_cleaning_event_col The name of the Mic cleaning event identifier column.
#' @param mic_usl_col The name of the Mic Upper Specification Limit column.
#' @return A dataframe with Ppu values for DAR, CAR, and Mic, along with the Overall Minimum Ppu.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande  [xiande.yang at gmail.com]
#' @export
#' @examples
#' \donttest{
#' Eq_DAR <- cv03_usl_unification(data = Eq_DAR, cleaning_event_col = "CleaningEvent", 
#'  residue_col = "DAR", usl_col = "USL")
#'  Eq_CAR <- cv03_usl_unification(data = Eq_CAR, cleaning_event_col = "CleaningEvent", 
#'  residue_col = "CAR", usl_col = "USL")
#'   df1 <- cv30_dar_car_mic_ppu(
#'     dar_data = Eq_DAR, 
#'     dar_residue_col = "DAR_Pct", 
#'     dar_cleaning_event_col = "CleaningEvent", 
#'     dar_usl_col = "USL_Pct",
#'     car_data = Eq_CAR, 
#'     car_residue_col = "CAR_Pct", 
#'     car_cleaning_event_col = "CleaningEvent", 
#'     car_usl_col = "USL_Pct",
#'     mic_data = Eq_Mic, 
#'     mic_residue_col = "Mic", 
#'     mic_cleaning_event_col = "CleaningEvent", 
#'     mic_usl_col = "USL")
#' }
cv30_dar_car_mic_ppu <- function(dar_data, dar_residue_col, dar_cleaning_event_col, dar_usl_col,
                                 car_data, car_residue_col, car_cleaning_event_col, car_usl_col,
                                 mic_data, mic_residue_col, mic_cleaning_event_col, mic_usl_col) {

  dar_ppu <- cv12_kde_ppu(data = dar_data, residue_col = dar_residue_col, cleaning_event_col = dar_cleaning_event_col, usl_col = dar_usl_col)
  car_ppu <- cv12_kde_ppu(data = car_data, residue_col = car_residue_col, cleaning_event_col = car_cleaning_event_col, usl_col = car_usl_col)
  
  poisson_test_result <- cv13_poisson_test(data = mic_data, residue_col = mic_residue_col)

  if (poisson_test_result$P_Value < 0.05) {

    mic_ppu <- cv12_kde_ppu(data = mic_data, residue_col = mic_residue_col, cleaning_event_col = mic_cleaning_event_col, usl_col = mic_usl_col)
  } else {

    mic_ppu_combined <- cv29_mic_ppu(data = mic_data, residue_col = mic_residue_col, cleaning_event_col = mic_cleaning_event_col, usl_col = mic_usl_col)
    mic_ppu <- mic_ppu_combined[mic_ppu_combined$Method == "Min", ]
  }

  df <- rbind(
    dar_ppu[, c("Ppu", "Lower_CI", "Upper_CI")],
    car_ppu[, c("Ppu", "Lower_CI", "Upper_CI")],
    mic_ppu[, c("Ppu", "Lower_CI", "Upper_CI")]
  )
  
  overall_min <- which.min(df$Ppu)
  overall_ppu <- df[overall_min, ]

  df <- rbind(df, overall_ppu)
  rownames(df) <- c("DAR", "CAR", "Mic", "Overall_Ppu")
  
  return(df)
}

