% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_selectboost_microarray.R
\docType{methods}
\name{plot_selectboost_cascade}
\alias{plot_selectboost_cascade}
\alias{plot,network.confidence,ANY-method}
\alias{plot,network.confidence,network.confidence-method}
\title{plot_Selectboost_cascade}
\usage{
\S4method{plot}{network.confidence,ANY}(x, col = gray((1:99)/100, alpha = NULL), ...)
}
\arguments{
\item{x}{A \code{network.confidence} object to be plotted.}

\item{col}{Colors for the plot.}

\item{...}{Additionnal arguments passed to the heatmap function.}
}
\value{
Nothing.
}
\description{
Plot result of Selectboost for Cascade inference.
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
data(net_confidences)
plot(net_confidence)
plot(net_confidence_.5)
plot(net_confidence_thr)

}
\references{
\emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Frédéric Bertrand, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Bioinformatics, 2020. \doi{10.1093/bioinformatics/btaa855}
}
\seealso{
\code{\link{boost}}, \code{\link{fastboost}}, \code{\link{selectboost}}, \code{\link[Cascade]{inference}}

Other Selectboost functions: 
\code{\link{autoboost}()},
\code{\link{boost}},
\code{\link{fastboost}()},
\code{\link{selectboost_cascade}}
}
\author{
Frederic Bertrand, \email{frederic.bertrand@utt.fr}
}
\concept{Selectboost functions}
