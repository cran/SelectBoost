% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SelectBoost-package.R
\docType{package}
\name{SelectBoost}
\alias{SelectBoost}
\title{SelectBoost}
\description{
Motivation: With the growth of big data, variable selection has become one of the major
challenges in statistics. Although many methods have been proposed in the literature their
performance in terms of recall and precision are limited in a context where the number of
variables by far exceeds the number of observations or in a high correlated setting.
Results: This package implements a new general algorithm which improves the precision of any
existing variable selection method. This algorithm is based on highly intensive simulations and
takes into account the correlation structure of the data. Our algorithm can either produce a
confidence index for variable selection or it can be used in an experimental design planning
perspective.
}
\examples{
set.seed(314)
xran=matrix(rnorm(75),15,5)
ybin=sample(0:1,15,replace=TRUE)
yran=rnorm(15)

#For quick test purpose, not meaningful, should be run with greater value of B
#(disabling parallel computing as well)
res.fastboost <- fastboost(xran,yran,B=3,use.parallel=FALSE)

\donttest{
fastboost(xran,yran)
#Customize resampling levels
fastboost(xran,yran,steps.seq=c(.99,.95,.9),c0lim=FALSE)

#Binary logistic regression
fastboost(xran,ybin,func=lasso_cv_glmnet_bin_min)
}
}
\references{
F. Bertrand, I. Aouadi, N. Jung, R. Carapito, L. Vallat, S. Bahram, M. Maumy-Bertrand (2020). SelectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets, \emph{Bioinformatics}. \doi{10.1093/bioinformatics/btaa855}

SelectBoost was used to decypher networks in
C. Schleiss, [...], M. Maumy-Bertrand, S. Bahram, F. Bertrand, and L. Vallat. (2021). Temporal multiomic modelling reveals a B-cell receptor proliferative program in chronic lymphocytic leukemia. \emph{Leukemia}.
}
