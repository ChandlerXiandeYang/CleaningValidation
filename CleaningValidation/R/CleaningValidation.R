#' Cleaning Validation Package
#'
#' This package offers a comprehensive suite of functions for cleaning validation, a critical component of quality control in pharmaceutical manufacturing. The included functions assist in analyzing residue data, evaluating cleaning efficacy, and ensuring that cleaning processes meet regulatory standards.
#'
#' @details
#' The functions primarily return data frames, streamlining data preprocessing, analysis, and the application of statistical methods for cleaning process evaluation. This toolset simplifies the workflow for cleaning validation professionals, providing resources for various tasks.
#' Function cv01 cleans three data types. Functions cv02 to cv12 (excluding cv05) are designed for sequential DAR and CAR analysis. Functions cv13 and cv14 assess whether Mic follows a Poisson distribution. For Mic data that follows a Poisson distribution, function cv05 and functions cv15 to cv29 should be used in sequence. If Mic data do not follow a Poisson distribution, function cv05 and functions cv02 to cv12 (excluding cv06) are applicable. Function cv30 synthesizes the Process Performance Index (Ppu) for DAR, CAR, and Mic.
#' Supplementary to its core capabilities, the package includes datasets—Eq_DAR, Eq_CAR, and Eq_Mic—for demonstrating functionality in practical contexts.
#' @section License:
#' This package is free software; you may redistribute and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License or (at your option) any later version.
#'
#' @examples
#' \dontrun{
#' # Example code here to demonstrate package usage:
#' # This could include data loading, transforming, and cleaning validation analysis.
#' }
#' @name CleaningValidation
#' @aliases CV
#' @keywords packages
NULL