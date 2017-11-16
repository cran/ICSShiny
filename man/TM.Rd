\name{TM}
\alias{TM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper for Joint M-estimation of location and scatter for a multivariate t-distribution
}
\description{
The function returns, for some multivariate data, the joint M-estimation of location and scatter matrix for a multivariate t-distribution obtained from \code{\link[ICS]{tM}}.
}
\usage{
TM(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric data matrix or dataframe.
}
  \item{\dots}{
further arguments passed to or from other methods.}
}
\details{
The use of the \code{\link{ICSShiny}} function requires to pass as an argument a list with: the location vector and the scatter matrix, as the first two arguments. 

The \code{TM} function is proposed inside the \code{\link{ICSShiny}} application.
}
\value{
\item{location}{the location vector obtained from the joint M-estimation of location and scatter for a multivariate t-distribution.}
\item{scatter}{the scatter matrix obtained from the joint M-estimation of location and scatter for a multivariate t-distribution.}
}
\references{
\code{\link[ICS]{tM}}
}
\author{
Aurore Archimbaud and Joris May
}



\seealso{
\code{\link[ICS]{tM}}, \code{\link{ICSShiny}}
}
\examples{
library(ICSShiny)
res.TM <- TM(iris[, 1:4], df=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate }% use one of  RShowDoc("KEYWORDS")
