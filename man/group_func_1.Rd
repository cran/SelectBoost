% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_func_1.R
\name{group_func_1}
\alias{group_func_1}
\title{Generate groups by thresholding.}
\usage{
group_func_1(absXcor, c0)
}
\arguments{
\item{absXcor}{A numeric matrix. The absolute value of a correlation or distance matrix.}

\item{c0}{A numeric scalar. The thresholding}
}
\value{
A list with one entry: the list of groups.
Attributes:
\itemize{
  \item "type": "normal"
  \item "length.groups" the length of each groups.
}
}
\description{
\code{group_func_1} creates groups of variables based on thresholding the input matrix.
}
\details{
This is a function used to create a list of groups using an input matrix and a
thresholding value c0. A group is made, for every column in the input matrix.
}
\examples{
set.seed(314)
group_func_1(cor(matrix(rnorm(50),10,5)),.4)

}
\references{
\emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Frédéric Bertrand, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Bioinformatics, 2020. \doi{10.1093/bioinformatics/btaa855}
}
\seealso{
\code{\link{group_func_2}} and \code{\link{boost.findgroups}}
}
\author{
Frederic Bertrand, \email{frederic.bertrand@utt.fr}
}
