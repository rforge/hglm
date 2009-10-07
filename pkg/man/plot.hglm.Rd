\name{plot.hglm}
\alias{plot.hglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot Hierarchical Generalized Linear Model Objects}
\description{
 Plots individual deviances and hatvalues for \code{hglm} objects
}
\usage{
plot.hglm(Object, pch = "+", col.theme = "colorful", output = "screen", filename = "Rplot", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Object}{the \code{hglm} object to be plotted}
  \item{pch}{symbol used in the plots}
  \item{col.theme}{either \code{colorful} or \code{blackwhite}}
  \item{output}{specifies whether the plots should be printed to screen, pdf or postscript-file}
  \item{filename}{name of output file}
  \item{\dots}{not used}
}
\details{
 A basic plot method for \code{hglm} objects. It produces a set of diagnostic plots for a hierarchical mdel. Plots can be viewed on current scrin or may be saved in current working directore by specifying sutable file name in the \code{filename} argument. 
}

\examples{
library(MASS)
data(bacteria)
g1<-hglm(fixed=y~week,random=~1|ID,data=bacteria,family=binomial(link=logit),disp=~1)
plot(g1)
}