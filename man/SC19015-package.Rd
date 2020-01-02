\name{SC19015-package}
\alias{SC19015-package}
\alias{SC19015}
\docType{package}
\title{
  Alternative Empirical likelihood 
}
\description{
  This package gives two types of generalization and promotions of Empirical Likelihood: Jackknife Empirical likelihood and Sequential linearization of constraints.
}
\details{
Empirical likelihood is a very powerful nonparametric tool that does not require any distributional assumptions. On the computational side, empirical likelihood involves maximizing nonparametric likelihood supported on the data subject to some constraints. If those constraints are linear, the maximization problem becomes particularly easy with the use of Lagrange multipliers, and usually reduces to solving a fixed number of simultaneous equations. However, the computational
advantage soon loses its appeal in other applications involving nonlinear  statistics.

To overcome the above computational diffculty, Wood, Do, and Broom (1996) proposed a sequential linearization method for empirical likelihood with nonlinear constraints. The method basically involves linearizing the nonlinear constraints and applying Owen's empirical likelihood to the linear term; the process can be iterated several times to obtain better performance.

Jing,Yuan, and Zhou (2013) propose a new approach: the jackknife empirical likelihood (JEL). The method combines two of the popular nonparametric approaches: the jackknife and the empirical likelihood. The key idea of the JEL is to turn the statistic of interest into a sample mean based on jackknife pseudovalues (Quenouille 1956). If we can show that these pseudo-values are asymptotically independent, we can apply Owen's empirical likelihood for the mean of the jackknife pseudo-values.

This package involves the solutions of Jackknife Empirical likelihood and Sequential linearization of constraints.
}
\author{
SC19015

Maintainer: SC19015
}
\references{
  B.-Y. Jing, J. Yuan, W. Zhou, Jackknife empirical likelihood, Journal of the American Statistical Association 104 (487) (2009) 1224-1232.
  
  A. T. Wood, K.-A. Do, B. Broom, Sequential linearization of empirical likelihood constraints with application to ustatistics, Journal of Computational and Graphical Statistics 5 (4) (1996) 365-385.
}
\keyword{ package }
\seealso{
  Optional links to other man pages
}
\examples{
  \dontrun{
     ## Optional simple examples of the most important functions
     ## These can be in \dontrun{} and \donttest{} blocks.   
  }
}
