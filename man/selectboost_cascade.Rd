% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selectboost_microarray.R
\docType{methods}
\name{selectboost_cascade}
\alias{selectboost_cascade}
\alias{selectboost}
\alias{selectboost,micro_array-method}
\alias{selectboost,micro_array,micro_array-method}
\title{Selectboost_cascade}
\usage{
selectboost(M, ...)

\S4method{selectboost}{micro_array}(
  M,
  Fabhat,
  K = 5,
  eps = 10^-5,
  cv.subjects = TRUE,
  ncores = 4,
  use.parallel = FALSE,
  verbose = FALSE,
  group = group_func_2,
  c0value = 0.95
)
}
\arguments{
\item{M}{Microarray class from the Cascade package.}

\item{...}{Additionnal arguments. Not used.}

\item{Fabhat}{F matrix inferred using the inference function from the Cascade package.}

\item{K}{Number of crossvalidation folds.}

\item{eps}{Threshold for assinging a zero value to an inferred parameter. Defaults to 10^-5.}

\item{cv.subjects}{Crossvalidation is made subjectwise using leave one out. Discards the K option.}

\item{ncores}{Numerical value. Number of cores for parallel computing.
Defaults to \code{4}.}

\item{use.parallel}{Boolean. To use parallel computing (doMC) download the extended package from Github.
Set to \code{FALSE}.}

\item{verbose}{Boolean.
Defaults to \code{FALSE}.}

\item{group}{Function. The grouping function.
Defaults to \code{group_func_2}.}

\item{c0value}{Numeric. c0 value to use for confidence computation.
Defaults to \code{TRUE}}
}
\value{
A \code{network.confidence} object.
}
\description{
Selectboost for Cascade inference.
}
\details{
Extending results from the Cascade package: providing confidence indices for the reverse engineered links.

Reference for the Cascade modelling
Vallat, L., Kemper, C. a., Jung, N., Maumy-Bertrand, M., Bertrand, F.,
Meyer, N., Pocheville, A., Fisher, J. W., Gribben, J. G. et Bahram, S.
(2013). Reverse-engineering the genetic circuitry of a cancer cell with predicted
intervention in chronic lymphocytic leukemia. Proceedings of the National
Academy of Sciences of the United States of America, 110(2), 459-64.

Reference for the Cascade package
Jung, N., Bertrand, F., Bahram, S., Vallat, L. et Maumy-Bertrand, M. (2014).
Cascade : A R package to study, predict and simulate the diffusion of a signal
through a temporal gene network. Bioinformatics. ISSN 13674803..
}
\examples{
set.seed(314)
set.seed(314)

\donttest{
data(Cascade_example)
Fab_inf_C <- Net_inf_C@F
#By default community grouping of variables
set.seed(1)
net_confidence <- selectboost(M, Fab_inf_C)
net_confidence_.5 <- selectboost(M, Fab_inf_C, c0value = .5)
#With group_func_1, variables are grouped by thresholding the correlation matrix
net_confidence_thr <- selectboost(M, Fab_inf_C, group = group_func_1)
}

}
\references{
\emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Frédéric Bertrand, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Bioinformatics, 2020. \doi{10.1093/bioinformatics/btaa855}
}
\seealso{
\code{\link{boost}}, \code{\link{fastboost}}, \code{\link{plot.selectboost}}, \code{\link[Cascade]{inference}}

Other Selectboost functions: 
\code{\link{autoboost}()},
\code{\link{boost}},
\code{\link{fastboost}()},
\code{\link{plot_selectboost_cascade}}
}
\author{
Frederic Bertrand, \email{frederic.bertrand@utt.fr}
}
\concept{Selectboost functions}
