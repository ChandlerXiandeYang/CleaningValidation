% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv02_nonpro_oos_reswab.R
\name{cv02_nonpro_oos_reswab}
\alias{cv02_nonpro_oos_reswab}
\title{Summarize Non-Process Related OOS and Reswab Data Which May Not Be Included in the Analysis}
\usage{
cv02_nonpro_oos_reswab(Eq_DAR, Eq_CAR, Eq_Mic)
}
\arguments{
\item{Eq_DAR}{A dataframe containing  equipment DAR  data.}

\item{Eq_CAR}{A dataframe containing  equipment CAR data.}

\item{Eq_Mic}{A dataframe containing  equipment Mic data.}
}
\value{
A dataframe summarizing the non-process-related OOS and reswab data.
}
\description{
This function processes three datasets to identify unique project IDs based on
non-process-related out-of-specification (OOS) and reswab cases, then summarizes
this information into a dataframe. If your data does not have reswab or OOS, you do not need to use this function.
}
\examples{
cv02_nonpro_oos_reswab(Eq_DAR, Eq_CAR, Eq_Mic)
}
\author{
Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
}
