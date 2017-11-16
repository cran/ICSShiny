\name{HRMEST}
\alias{HRMEST}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper for Joint Affine Equivariant Estimation of Multivariate Median and Tyler's Shape Matrix
}
\description{
The function returns, for some multivariate data, the joint affine equivariant estimation of multivariate median and Tyler's shape matrix obtained from \code{\link[ICSNP]{HR.Mest}}.
}
\usage{
HRMEST(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric data matrix or dataframe.
}
  \item{\dots}{
further arguments passed on to \code{\link[ICSNP]{HR.Mest}}.}
}
\details{
The use of the \code{\link{ICSShiny}} function requires to pass as an argument a list with: the location vector and the scatter matrix, as the first two arguments. 

The \code{HRMEST} function is mainly for internal use in the \code{\link{ICSShiny}} application.
}
\value{
\item{location}{the location vector obtained from the joint affine equivariant estimation of multivariate median and Tyler's shape matrix.}
\item{scatter}{the scatter matrix obtained from the joint affine equivariant estimation of multivariate median and Tyler's shape matrix.}
}
\references{
\code{\link[ICSNP]{HR.Mest}}
}
\author{
Klaus Nordhausen
}



\seealso{
\code{\link[ICSNP]{HR.Mest}}, \code{\link{ICSShiny}}
}
\examples{
library(ICSShiny)
res.HR.Mest <- HRMEST(iris[, 1:4], maxiter = 1000)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate }% use one of  RShowDoc("KEYWORDS")
