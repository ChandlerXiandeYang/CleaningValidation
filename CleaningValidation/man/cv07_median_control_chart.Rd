% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv07_median_control_chart.R
\name{cv07_median_control_chart}
\alias{cv07_median_control_chart}
\title{Median Control Chart and Density Plot}
\usage{
cv07_median_control_chart(data, cleaning_event_col, residue_pct_median_col)
}
\arguments{
\item{data}{A data frame containing the data to plot.}

\item{cleaning_event_col}{The name of the column containing cleaning event identifiers.}

\item{residue_pct_median_col}{The name of the column containing the calculated median residue percentages.}
}
\value{
A cowplot object containing the combined control chart and density plot.
}
\description{
This function creates a control chart and a density plot for the median residue percentages based on kernel density estimation.
}
\examples{
# Assuming 'Eq_DAR' is a data frame with appropriate columns:
Eq_DAR <- cv03_usl_unification(data = Eq_DAR,  "CleaningEvent",  "DAR",  "USL")
cv07_median_control_chart(data = Eq_DAR, cleaning_event_col = "CleaningEvent", 
residue_pct_median_col="DAR_Pct_Median")
}
\author{
Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
}
