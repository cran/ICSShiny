\name{ICSShiny}
\alias{ICSShiny}

\title{
Invariant Coordinate Selection With a Shiny App
}

\description{
Performs ICS via a shiny app where the user can change the scatter matrices, explore the output and download graphs and components. 
Also the ICS outlier detection framework, from the \code{\link{ICSOutlier}} package is available.
It is inspired from the \code{Factoshiny} application of the \code{FactoMineR} package.
}

\usage{
ICSShiny(x, S1 = MeanCov, S2 = Mean3Cov4, 
         S1args = list(), S2args = list(), seed = NULL, 
         ncores = NULL, iseed = NULL, 
         pkg = "ICSOutlier")
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data matrix or dataframe with at least two numeric variables. Please note that it can contain non-numeric variables, but ICS is only performed on numeric variables.}
  \item{S1}{name of the function which returns the first location vector T1 and scatter matrix S1. See details and \code{\link[ICS]{ics2}} for more information. Default is \code{\link[ICS]{MeanCov}}.} 
  \item{S2}{name of the function which returns the second location vector T2 and scatter matrix S2. See details and \code{\link[ICS]{ics2}} for more information. Default is \code{\link[ICS]{Mean3Cov4}}.} 
  \item{S1args}{list with optional additional arguments when calling function S1.}
  \item{S2args}{list with optional additional arguments when calling function S2.}
  \item{seed}{to fix a seed when needed in order to fix the thresholds. Default is \code{NULL}. See details for more information.}
  \item{ncores}{number of cores to be used in \code{\link{dist.simu.test}} and \code{\link{comp.simu.test}}. If \code{NULL} or 1, no parallel computing is used. Otherwise \link[parallel]{makeCluster} with \code{type = "PSOCK"} is used.}
  \item{iseed}{If parallel computation is used the seed passed on to \code{\link[parallel]{clusterSetRNGStream}}. Default is NULL which means no fixed seed is used.}
  \item{pkg}{When using parallel computing, a character vector listing all the packages which need to be loaded on the different cores via \code{\link{require}}. Must be at least "ICSOutlier" and must contain the packages needed to compute the scatter matrices.}

}

\value{
Returns several tabs on the navigator:
\item{Choice of the parameters}{The scatterplot matrix of an ICS object for the parameters chosen on the left part (variables included/excluded, the location vectors and scatter matrices).}
\item{Component selection}{Three different subtabs to help the user to choose the interesting components. The first sub-tab is the screeplot of the eigenvalues of the ICS object followed by the summary of the analysis. The second sub-tab plots the kernel density of the ICS components. The third sub-tab suggests which components to select, starting from the highest and/or the lowest kurtosis, through different normality tests or simulations.

 The default values of the slidebar in the left are obtained from \code{"agostino.test"} at 5\%.}
\item{Matrix scatterplot of invariant components}{The two sub-tabs aim at identifying groups or outliers by using pairwise plots of invariant coordinates. 
 It offers two ways of plotting them: only two invariant components or a scatterplot matrix with up to six invariant components.
 The left panel allows to color the groups identified by the user and label the observations.}
\item{Outlier identification}{
 This tab plots outlyingness values for each observation based on the selected components. These squared ICS distances are computed through the \code{\link[ICSOutlier]{ics.distances}} function as the Euclidian distance of the observations to the origin using  the selected centered components. The identification of the outliers can be based on different cut-offs: from Monte Carlo simulations as in \code{\link[ICSOutlier]{dist.simu.test}} or by giving a percentage or a number of observations to identify.}
\item{Descriptive statistics}{
This tab gives some descriptive statistics on different subsets of the data (for all the observations, for the observations from a given cluster, for the outlying observations) and enables
to compare the sub-populations. The application includes a boxplot, a kernel density, an histogram and some basic statistics: Min, Q1, Mean, Median, Q3 and Max.}
\item{Data Table}{
 This tab contains the dataset with a nice display and the possibility to choose different sub-populations of the data:
 all the observations, the observations from a given cluster or the outlying observations.}
\item{Save}{This tab allows to display and save the data table of components and the summary of operations. 
The data frame contains the components kept in the analysis as well as the distance generated by these components. 
It also includes the cluster the observation belongs to whether the observation is defined as an outlier, as well as the variables used for labelling and categorizing the data.
The data are saved in a csv format.
The summary of operations contains a summary of all parameters that were used to obtain the current result, it may be useful for another user who may want to get the same result as the original user.
It is saved in a txt format.}
 

The "Close the session" button closes the application and saves the icsshiny object into the global environment.
}

\details{
\describe{
\item{Choice of the parameters}{


 The scatter matrices and their associated location estimators can be selected through the list out of the options: \code{\link[ICS]{MeanCov}}, \code{\link[ICS]{Mean3Cov4}}, \code{\link{MCD}},
 \code{\link{TM}}. It is also possible to run the application with your own functions as long as they are passed as an argument of the call to \code{\link{ICSShiny}}. However, in this case it is not possible to run the simulations' steps for now. 
 
 ICS is only performed on numeric variables. Only non-numeric variables are proposed for labelling and/or categorizing the observations.
 }
\item{Component selection}{


 For computing the kernel densities in the second sub-tab, the weight is given by the Gaussian function and the bandwidth follows the rule of thumb of Silverman (1986).
 
 For the automatic selection of the Invariant Components (IC), the referenced normality tests are the same as in the \code{\link[ICSOutlier]{comp.norm.test}} function: \code{"jarque.test"}, \code{"anscombe.test"},
\code{"bonett.test"}, \code{"agostino.test"}, \code{"shapiro.test"}. All the decisions are corrected from multiple testing by adjusting the levels as in \code{\link[ICSOutlier]{comp.norm.test}}.
The number of components to keep can also be decided from Monte Carlo simulations trough the \code{\link[ICSOutlier]{comp.simu.test}} function. This parallel analysis method 
 may need a very long time to compute, so it is used only if the user clicks on the 'Launch the test' button.}
 }
}


\references{
\cite{Nordhausen, K., Oja, H. and Tyler, D.E. (2008), Tools for exploring multivariate data: The package ICS, \emph{Journal of Statistical Software}, \bold{28}, 1--31. <doi:10.18637/jss.v028.i06>.}  

 \cite{Archimbaud, A., Nordhausen, K. and Ruiz-Gazen, A. (2016), ICS for multivariate outlier detection with application to quality control, <https://arxiv.org/pdf/1612.06118.pdf>.}   
}

\author{
 Aurore Archimbaud and Joris May
}

\seealso{ \code{\link[ICS]{ics2}},\code{\link[ICSOutlier]{ics.outlier}},\cr
\href{http://shiny.rstudio.com}{shiny website}
}

\examples{
if(interactive()){
library(ICSShiny)
# ICS with ICSShiny:
res.shiny <- ICSShiny(iris)

# Close the session by clicking on the button or closing the navigator's tab
# ICS on a result of an ICSshiny object
ICSShiny(res.shiny)

# ICS with ICSShiny and different parameters
res.shiny <- ICSShiny(iris, S1 = MCD, S1args=list(alpha=0.7), seed = 7587)

# ICS with ICSShiny with parallelization of computations and seed
res.shiny <- ICSShiny(iris, iseed = 1234, ncores = 2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}% use one of  RShowDoc("KEYWORDS")
