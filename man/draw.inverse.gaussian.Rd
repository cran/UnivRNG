\name{draw.inverse.gaussian}
\alias{draw.inverse.gaussian}
\title{Generates variation from inverse Gaussian distribution}
\description{This function implements pseudo-random number generation for an inverse Gaussian distribution with pdf

\deqn{f(x|\mu,\lambda)=(\frac{\lambda}{2\pi})^{1/2}x^{-3/2}e^{-\frac{\lambda(x-\mu)^2}{2\mu^2x}}}

for \eqn{x > 0}, \eqn{\mu > 0}, and \eqn{\lambda > 0} where \eqn{\mu}{mu} and \eqn{\lambda}{lambda} are the location and scale parameters, respectively.
}
\usage{
draw.inverse.gaussian(nrep,mu,lambda)
}
\arguments{
  \item{nrep}{Number of data points to generate.}
  \item{mu}{Location parameter for the desired inverse Gaussian distribution.}
  \item{lambda}{Scale parameter for the desired inverse Gaussian distribution.}
}
\value{A list of length five containing generated data, the theoretical mean, the empirical mean, the theoretical variance, and the empirical variance with names y, theo.mean, emp.mean, theo.var, and emp.var, respectively.}
\references{
Michael, J. R., William, R. S., & Haas, R. W. (1976). Generating random variates using transformations with multiple roots. \emph{The American Statistician}, \bold{30}, 88-90.
}
\examples{
draw.inverse.gaussian(nrep=100000,mu=1,lambda=1)

draw.inverse.gaussian(nrep=100000,mu=3,lambda=1)
}
