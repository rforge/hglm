\name{hglm-package}
\alias{hglm-package}
\docType{package}
\title{
Hierarchical Generalized Linear Models
}
\description{
 \code{hglm} is used to fit hierarchical generalized linear models. It can be used for linear mixed models and 
  generalized linear models with random effects for a variety of links and a variety of distributions for both
   the outcomes and the random effects. Fixed effects can also be fitted in the dispersion part of the model.
    The function can be called either by specifying the design matrices or as a \code{\link{formula}}.
}
\details{
\tabular{ll}{
Package: \tab hglm\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2009-06-10\cr
License: \tab Unlimited\cr
LazyLoad: \tab yes\cr
}
This is version 1.1 of the hglm package. 
}
\author{
M. Alam, L. Ronnegard, X. Shen
Maintainer: Lars Ronnegard <lrn@du.se>
}
\references{
Lee, Y., Nelder J. A. and Pawitan Y. (2006) \emph{Generalized Linear Models with Random Effect: a unified approach with h-likelihood.}
Chapman and Hall, London
}

\keyword{ package }
\seealso{
\code{\link{summary.hglm}}
}
\examples{
library(MASS)
data(bacteria)
g1<-hglm(fixed=y~week,random=~1|ID,data=bacteria,family=binomial(link=logit),disp=~1)
summary(g1)
plot(g1)
}


