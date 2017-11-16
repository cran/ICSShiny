\name{MCD}
\alias{MCD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper for MCD location and scatter estimates 
}
\description{
The function returns, for some multivariate data, the MCD location and scatter estimates obtained from \code{\link[rrcov]{CovMcd}}.
}
\usage{
MCD(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric data matrix or dataframe.
}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
The use of the \code{\link{ICSShiny}} function requires to pass as an argument a list with: the location vector and the scatter matrix, as the first two arguments. 

The \code{MCD} function is proposed inside the \code{\link{ICSShiny}} application.
}
\value{
\item{location}{MCD location vector.}
\item{scatter}{MCD scatter estimate.}
}
\references{
\code{\link[rrcov]{CovMcd}}
}
\author{
Aurore Archimbaud and Joris May
}



\seealso{
\code{\link[rrcov]{CovMcd}},  \code{\link{ICSShiny}}
}
\examples{
library(ICSShiny)
res.MCD <- MCD(iris[, 1:4], alpha = 0.75)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }% use one of  RShowDoc("KEYWORDS")
