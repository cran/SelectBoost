% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{Cascade_confidence}
\alias{Cascade_confidence}
\alias{net_confidence}
\alias{net_confidence_.5}
\alias{net_confidence_thr}
\title{Confidence indices}
\format{
A \code{network.confidence} object with four slots :
\describe{
  \item{network.confidence}{The confidence matrix}
  \item{name}{Names of the variables (genes)}
  \item{F}{F array, see Cascade for more details}
  \item{time_pt}{Repeated measurements}
  \item{cv.subjects}{Logical. Was crossvalidation carried out subjectwise?}
}

An object of class \code{network.confidence} of length 1.

An object of class \code{network.confidence} of length 1.
}
\usage{
net_confidence

net_confidence_.5

net_confidence_thr
}
\description{
Result for confidence indices derivation using the Cascade package
}
\keyword{datasets}
