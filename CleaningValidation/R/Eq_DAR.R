#' Equipment Cleaning Data for DAR
#'
#' A dataset containing cleaning validation data for equipment DAR.
#'
#' @format A data frame with 60 rows and 3 variables:
#' \describe{
#'   \item{DAR}{Numeric vector with DAR measurements.}
#'   \item{USL}{Numeric vector with Upper Specification Limits for DAR.}
#'   \item{CleaningEvent}{Factor vector with Cleaning Event identifiers.}
#'   \item{Classification}{Character vector with the deviation status for each cleaning event. Defaults to "normal".}
#'   \item{LIMSProjectID}{Integer or character vector with unique project IDs assigned to each row.}
#' }
#' @source Details about the data source.
#' @keywords datasets
#' @export
Eq_DAR <- data.frame(
  DAR = c(0.59,0.59,35.10,0.59,0.59,0.59,5.24,0.72,43.78,4.25,17.99,0.63,0.60,0.4,5.2,5.9,0.6,0.4,2.4,1,1,1,0.8,1.1,0.48,0.48,0.48,2.19,0.48,0.48,0.14,0.14,0.14,2,0.14,0.14,0.14,0.14,0.48,0.48,0.14,0.48,0.11,0.11,0.11,0.7,0.11,0.11,0.11,0.33,0.11,0.33,0.33,0.33,0.76,0.76,0.76,0.76,0.76,0.76),
  USL = c(122.2,122.2,122.2,122.2,122.2,122.2,122.2,122.2,122.2,122.2,122.2,122.2,122.2,122.2,122.2,122.2,122.2,122.2,169.7,169.7,169.7,169.7,169.7,169.7,169.7,169.7,169.7,169.7,169.7,169.7,169.7,169.7,169.7,169.7,169.7,169.7,169.7,169.7,169.7,169.7,169.7,169.7,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20),
  CleaningEvent = factor(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,10,10,10)),
  Classification = rep("regular", 60),
  LIMSProjectID = 1:60
)

