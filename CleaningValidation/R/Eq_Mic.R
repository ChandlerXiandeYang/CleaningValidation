#' Equipment Cleaning Data for Microbial Bioburden
#'
#' A dataset containing cleaning validation data for microbial bioburden (Mic).
#'
#' @format A data frame with 20 rows and 3 variables:
#' \describe{
#'   \item{Mic}{Numeric vector with Mic measurements.}
#'   \item{USL}{Numeric vector with Upper Specification Limits for Mic.}
#'   \item{CleaningEvent}{Factor vector with Cleaning Event identifiers.}
#'   \item{Classification}{Character vector with the deviation status for each cleaning event. Defaults to "normal".}
#'   \item{LIMSProjectID}{Integer or character vector with unique project IDs assigned to each row.}
#' }
#' @source Details about the data source.
#' @keywords datasets
#' @export
Eq_Mic <- data.frame(
  Mic = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0),
  USL = rep(25,20),
  CleaningEvent = factor(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10)),
  Classification = rep("regular", 20),
  LIMSProjectID = 1:20
)
