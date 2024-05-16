#' Summarize Non-Process Related OOS and Reswab Data Which May Not Be Included in the Analysis
#'
#' This function processes three datasets to identify unique project IDs based on
#' non-process-related out-of-specification (OOS) and reswab cases, then summarizes
#' this information into a dataframe. If your data does not have reswab or OOS, you do not need to use this function.
#'
#' @param Eq_DAR A dataframe containing  equipment DAR  data.
#' @param Eq_CAR A dataframe containing  equipment CAR data.
#' @param Eq_Mic A dataframe containing  equipment Mic data.
#' @return A dataframe summarizing the non-process-related OOS and reswab data.
#' @author Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
#' @export
#' @examples
#' cv02_nonpro_oos_reswab(Eq_DAR, Eq_CAR, Eq_Mic)
cv02_nonpro_oos_reswab <- function(Eq_DAR, Eq_CAR, Eq_Mic) {
    process_ids <- function(data, cleaning_event_col, classification_col, lims_proj_id_col, tagger_func) {
    tag_vector <- sapply(data[[classification_col]], tagger_func)
    unique_project_ids <- unique(data[tag_vector, lims_proj_id_col])
    return(unique_project_ids)
  }
nonpro_oos_tagger <- function(text) {
    clean_text <- tolower(gsub("[^a-zA-Z0-9]", "", text))
    return(grepl("nonprocessrelated", clean_text))
  }
  
  reswab_tagger <- function(text) {
    clean_text <- tolower(gsub("[^a-zA-Z0-9]", "", text))
    return(grepl("reswab", clean_text))
  }
  
  nonpro_oos <- lapply(list(DAR = Eq_DAR, CAR = Eq_CAR, Mic = Eq_Mic), function(data) {
    process_ids(data, "CleaningEvent", "Classification", "LIMSProjectID", nonpro_oos_tagger)
  })
reswab <- lapply(list(DAR = Eq_DAR, CAR = Eq_CAR, Mic = Eq_Mic), function(data) {
    process_ids(data, "CleaningEvent", "Classification", "LIMSProjectID", reswab_tagger)
  })
  OOS_Reswab_Data <- data.frame(
    Residue = c("DAR", "CAR", "Mic"),
    NonPro_OOS = sapply(nonpro_oos, length),
    Reswab = sapply(reswab, length)
  )
  return(OOS_Reswab_Data)
}

