#' Equipment Cleaning Data for CAR
#'
#' A dataset containing cleaning validation data for equipment CAR.
#'
#' @format A data frame with 30 rows and 3 variables:
#' \describe{
#'   \item{CAR}{Numeric vector with CAR measurements.}
#'   \item{USL}{Numeric vector with Upper Specification Limits for CAR.}
#'   \item{CleaningEvent}{Factor vector with Cleaning Event identifiers.}
#'   \item{Classification}{Character vector with the deviation status for each cleaning event. Defaults to "normal".}
#'   \item{LIMSProjectID}{Integer or character vector with unique project IDs assigned to each row.}
#' }
#' @source Details about the data source.
#' @keywords datasets
#' @export
Eq_CAR <- data.frame(
  CAR = c(1.26,1.26,1.26,4.21,4.21,4.21,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26,1.26),
  USL = rep(103.3,33),
  CleaningEvent = factor(c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,10,10,10,11,11,11)),
  Classification = rep("regular", 33),
  LIMSProjectID = 1:33
)