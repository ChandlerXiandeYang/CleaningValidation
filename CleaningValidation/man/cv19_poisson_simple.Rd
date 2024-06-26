% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv19_poisson_simple.R
\name{cv19_poisson_simple}
\alias{cv19_poisson_simple}
\title{Poisson Fixed Effect Model Summary}
\usage{
cv19_poisson_simple(data, residue_col)
}
\arguments{
\item{data}{A data frame containing the data set for analysis.}

\item{residue_col}{The name of the column representing residue data.}
}
\value{
A data frame with the formatted summary of the Poisson regression model.
}
\description{
Fits a simple Poisson model to the data and returns a data frame containing
the model's term, estimate, standard error, z value, and p-value, formatted to
a fixed number of decimal places.
}
\examples{
# Assuming 'Eq_Mic' is a data frame and 'Mic' is the residue column of interest.
cv19_poisson_simple(data = Eq_Mic, residue_col = "Mic")
}
\author{
Chan, Mohamed, Lou, Wendy, Yang, Xiande [xiande.yang at gmail.com]
}
