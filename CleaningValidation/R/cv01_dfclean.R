#' Clean and preprocess residue data for stability and capability analysis
#'
#' This function ensures data type and no missing data in residue_col, cleaning_event_col, usl_col of data their type. Furthermore, it changes cleaning_event_col to time ordered factor. It cleans and pre-processes the residue data for stability and capability analysis,
#' ensuring that it meets the necessary criteria for analysis.
#'
#' @param data A data frame containing one of drug active-ingredient residue (DAR), cleaning agent residue (CAR),
#' or microbial bioburden residue (Mic) data.
#' @param residue_col The name of the column containing the numeric residue data.
#' @param cleaning_event_col The name of the column containing the Cleaning Event data.
#' @param usl_col The name of the column containing the numeric upper specification limit (USL) data.
#' @return A cleaned and pre-processed data frame such that all variables have no missing values, its CleaningEvent
#' is time-ordered categorical variable, and Residue and USL are numeric.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @examples
#' # Assume Eq_DAR, Eq_CAR, and Eq_Mic are loaded datasets
#'
#' # Clean and preprocess residue data for Eq_DAR
#' Eq_DAR <- cv01_dfclean(data = Eq_DAR, residue_col = "DAR", usl_col = "USL", 
#' cleaning_event_col = "CleaningEvent")
#'
#' # Clean and preprocess residue data for Eq_CAR
#' Eq_CAR <- cv01_dfclean(data = Eq_CAR, residue_col = "CAR", usl_col = "USL", 
#' cleaning_event_col = "CleaningEvent")
#'
#' # Clean and preprocess residue data for Eq_Mic
#' Eq_Mic <- cv01_dfclean(data = Eq_Mic, residue_col = "Mic", usl_col = "USL", 
#' cleaning_event_col = "CleaningEvent")
#' @export
cv01_dfclean <- function(data, residue_col, cleaning_event_col, usl_col ) {

  if (any(is.na(data[[residue_col]]))) {
    stop("Warning: Missing values in ", residue_col)
  }
  if (any(is.na(data[[usl_col ]]))) {
    stop("Warning: Missing values in ", usl_col)
  }
  if (any(is.na(data[[cleaning_event_col]]))) {
    stop("Warning: Missing values in ", cleaning_event_col)
  }

  if (!is.numeric(data[[residue_col]]) || !is.numeric(data[[usl_col]])) {
    stop(paste(residue_col, "or", usl_col, "is not numeric"))
  }

  data[[cleaning_event_col]] <- factor(data[[cleaning_event_col]],
                                  levels = sort(unique(as.numeric(data[[cleaning_event_col]]))),
                                  ordered = TRUE)

  return(data)
}
