\name{print.icsshiny}
\alias{print.icsshiny}

\title{
Prints the ICSshiny Results
}

\description{
Prints an object of class icsshiny, typically the results of a call to \code{\link{ICSShiny}}. The output corresponds to the summary of operations.
}

\usage{
\method{print}{icsshiny}(x, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object of class icsshiny.}
  \item{\dots}{further arguments to be passed to or from methods.}
}



\author{
Aurore Archimbaud and Joris May
}

\seealso{ \code{\link[ICSShiny]{ICSShiny}}
}

\examples{
\dontrun{
library(ICSShiny)
# ICS with Factoshiny:
res.shiny <- ICSshiny(iris)

# click on the "Close the session" button or close the tab
print(res.shiny)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }% 
\keyword{ print }% 