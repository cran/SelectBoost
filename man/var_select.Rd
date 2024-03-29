% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lasso_cv.R
\name{var_select}
\alias{var_select}
\alias{lasso_cv_glmnet_bin_min}
\alias{lasso_cv_glmnet_bin_1se}
\alias{lasso_glmnet_bin_AICc}
\alias{lasso_glmnet_bin_BIC}
\alias{lasso_cv_lars_min}
\alias{lasso_cv_lars_1se}
\alias{lasso_cv_glmnet_min}
\alias{lasso_cv_glmnet_min_weighted}
\alias{lasso_cv_glmnet_1se}
\alias{lasso_cv_glmnet_1se_weighted}
\alias{lasso_msgps_Cp}
\alias{lasso_msgps_AICc}
\alias{lasso_msgps_GCV}
\alias{lasso_msgps_BIC}
\alias{enetf_msgps_Cp}
\alias{enetf_msgps_AICc}
\alias{enetf_msgps_GCV}
\alias{enetf_msgps_BIC}
\alias{lasso_cascade}
\title{Variable selection functions}
\usage{
lasso_cv_glmnet_bin_min(X, Y)

lasso_cv_glmnet_bin_1se(X, Y)

lasso_glmnet_bin_AICc(X, Y)

lasso_glmnet_bin_BIC(X, Y)

lasso_cv_lars_min(X, Y)

lasso_cv_lars_1se(X, Y)

lasso_cv_glmnet_min(X, Y)

lasso_cv_glmnet_min_weighted(X, Y, priors)

lasso_cv_glmnet_1se(X, Y)

lasso_cv_glmnet_1se_weighted(X, Y, priors)

lasso_msgps_Cp(X, Y, penalty = "enet")

lasso_msgps_AICc(X, Y, penalty = "enet")

lasso_msgps_GCV(X, Y, penalty = "enet")

lasso_msgps_BIC(X, Y, penalty = "enet")

enetf_msgps_Cp(X, Y, penalty = "enet", alpha = 0.5)

enetf_msgps_AICc(X, Y, penalty = "enet", alpha = 0.5)

enetf_msgps_GCV(X, Y, penalty = "enet", alpha = 0.5)

enetf_msgps_BIC(X, Y, penalty = "enet", alpha = 0.5)

lasso_cascade(M, Y, K, eps = 10^-5, cv.fun)
}
\arguments{
\item{X}{A numeric matrix. The predictors matrix.}

\item{Y}{A binary factor. The 0/1 classification response.}

\item{priors}{A numeric vector. Weighting vector for the variable selection. When used with the \code{glmnet} estimation function, the weights share the following meanings:
\itemize{
  \item 0: the variable is always included in the model
  \item 1: neutral weight
  \item Inf: variable is always excluded from the model
}}

\item{penalty}{A character value to select the penalty term in msgps
(Model Selection Criteria via Generalized Path Seeking). Defaults to "enet".
"genet" is the generalized elastic net and "alasso" is the adaptive lasso,
which is a weighted version of the lasso.}

\item{alpha}{A numeric value to set the value of \eqn{\alpha} on "enet" and "genet" penalty in msgps
(Model Selection Criteria via Generalized Path Seeking).}

\item{M}{A numeric matrix. The transposed predictors matrix.}

\item{K}{A numeric value. Number of folds to use.}

\item{eps}{A numeric value. Threshold to set to 0 the inferred value of a parameter.}

\item{cv.fun}{A function. Fonction used to create folds. Used to perform corss-validation subkectwise.}
}
\value{
A vector of coefficients.
}
\description{
Compute coefficient vector after variable selection.
}
\details{
\code{lasso_cv_glmnet_bin_min} returns the vector of coefficients
for a binary logistic model estimated by the lasso using the \code{lambda.min} value
computed by 10 fold cross validation. It uses the \code{glmnet} function of
the \code{glmnet}package.

\code{lasso_cv_glmnet_bin_1se} returns the vector of coefficients
for a binary logistic model estimated by the lasso using the \code{lambda.1se}
(lambda.min+1se) value computed by 10 fold cross validation. It uses the \code{glmnet}
function of the \code{glmnet}package.

\code{lasso_glmnet_bin_AICc} returns the vector of coefficients
for a binary logistic model estimated by the lasso and selected according to the
bias-corrected AIC (AICC) criterion. It uses the \code{glmnet}

\code{lasso_glmnet_bin_BIC} returns the vector of coefficients
for a binary logistic model estimated by the lasso and selected according to the BIC
 criterion. It uses the \code{glmnet}

\code{lasso_cv_lars_min} returns the vector of coefficients
for a linear model estimated by the lasso using the \code{lambda.min} value
computed by 5 fold cross validation. It uses the \code{lars} function of the
\code{lars} package.

\code{lasso_cv_lars_1se} returns the vector of coefficients
for a linear model estimated by the lasso using the \code{lambda.1se}
(lambda.min+1se) value computed by 5 fold cross validation.
It uses the \code{lars} function of the \code{lars} package.

\code{lasso_cv_glmnet_min} returns the vector of coefficients
for a linear model estimated by the lasso using the \code{lambda.min} value
computed by 10 fold cross validation. It uses the \code{glmnet} function of the
\code{glmnet} package.

\code{lasso_cv_glmnet_min_weighted} returns the vector of coefficients
for a linear model estimated by the weighted lasso using the \code{lambda.min} value
computed by 10 fold cross validation. It uses the \code{glmnet} function of the
\code{glmnet} package.

\code{lasso_cv_glmnet_1se} returns the vector of coefficients
for a linear model estimated by the lasso using the \code{lambda.1se}
(lambda.min+1se) value computed by 10 fold cross validation. It uses the \code{glmnet}
 function of the
\code{glmnet} package.

\code{lasso_cv_glmnet_1se_weighted} returns the vector of coefficients
for a linear model estimated by the weighted lasso using the \code{lambda.1se}
(lambda.min+1se) value computed by 10 fold cross validation. It uses the \code{glmnet}
function of the \code{glmnet} package.

\code{lasso_msgps_Cp} returns the vector of coefficients
for a linear model estimated by the lasso selectd using Mallows' Cp.
It uses the \code{msgps} function of the \code{msgps} package.

\code{lasso_msgps_AICc} returns the vector of coefficients
for a linear model estimated by the lasso selected according to the bias-corrected AIC
(AICC) criterion. It uses the \code{msgps} function of the \code{msgps} package.

\code{lasso_msgps_GCV} returns the vector of coefficients
for a linear model estimated by the lasso selected according to the generalized
cross validation criterion. It uses the \code{msgps} function of the \code{msgps} package.

\code{lasso_msgps_BIC} returns the vector of coefficients
for a linear model estimated by the lasso selected according to the BIC criterion.
It uses the \code{msgps} function of the \code{msgps} package.

\code{enetf_msgps_Cp} returns the vector of coefficients
for a linear model estimated by the elastic net selectd using Mallows' Cp.
It uses the \code{msgps} function of the \code{msgps} package.

\code{enetf_msgps_AICc} returns the vector of coefficients
for a linear model estimated by the elastic net selected according to the bias-corrected AIC
(AICC) criterion. It uses the \code{msgps} function of the \code{msgps} package.

\code{enetf_msgps_GCV} returns the vector of coefficients
for a linear model estimated by the elastic net selected according to the generalized
cross validation criterion. It uses the \code{msgps} function of the \code{msgps} package.

\code{enetf_msgps_BIC} returns the vector of coefficients
for a linear model estimated by the elastic net selected according to the BIC criterion.
It uses the \code{msgps} function of the \code{msgps} package.

\code{lasso_cascade} returns the vector of coefficients
for a linear model estimated by the lasso.
It uses the \code{lars} function of the \code{lars} package.
}
\examples{
set.seed(314)
xran=matrix(rnorm(150),30,5)
ybin=sample(0:1,30,replace=TRUE)
yran=rnorm(30)
set.seed(314)
lasso_cv_glmnet_bin_min(xran,ybin)

set.seed(314)
lasso_cv_glmnet_bin_1se(xran,ybin)

set.seed(314)
lasso_glmnet_bin_AICc(xran,ybin)

set.seed(314)
lasso_glmnet_bin_BIC(xran,ybin)

set.seed(314)
lasso_cv_lars_min(xran,yran)

set.seed(314)
lasso_cv_lars_1se(xran,yran)

set.seed(314)
lasso_cv_glmnet_min(xran,yran)

set.seed(314)
lasso_cv_glmnet_min_weighted(xran,yran,c(1000,0,0,1,1))

set.seed(314)
lasso_cv_glmnet_1se(xran,yran)

set.seed(314)
lasso_cv_glmnet_1se_weighted(xran,yran,c(1000,0,0,1,1))

set.seed(314)
lasso_msgps_Cp(xran,yran)

set.seed(314)
lasso_msgps_AICc(xran,yran)

set.seed(314)
lasso_msgps_GCV(xran,yran)

set.seed(314)
lasso_msgps_BIC(xran,yran)

set.seed(314)
enetf_msgps_Cp(xran,yran)

set.seed(314)
enetf_msgps_AICc(xran,yran)

set.seed(314)
enetf_msgps_GCV(xran,yran)

set.seed(314)
enetf_msgps_BIC(xran,yran)

set.seed(314)
lasso_cascade(t(xran),yran,5,cv.fun=lars::cv.folds)

}
\references{
\emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Frédéric Bertrand, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Bioinformatics, 2020. \doi{10.1093/bioinformatics/btaa855}
}
\seealso{
\code{\link[glmnet]{glmnet}}, \code{\link[glmnet]{cv.glmnet}}, \code{\link{AICc_BIC_glmnetB}}, \code{\link[lars]{lars}}, \code{\link[lars]{cv.lars}}, \code{\link[msgps]{msgps}}

Other Variable selection functions: 
\code{\link{var_select_all}}
}
\author{
Frederic Bertrand, \email{frederic.bertrand@utt.fr}
}
\concept{Variable selection functions}
