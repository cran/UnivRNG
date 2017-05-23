\name{draw.laplace}
\alias{draw.laplace}
\title{Generates variates from Laplace distribution}
\description{This function implements pseudo-random number generation for a Laplace (double exponential) distribution with pdf

\deqn{f(x|\lambda,\alpha)=\frac{\lambda}{2}e^{-\lambda|x-\alpha|}}

for \eqn{\lambda}{lambda}>0 where \eqn{\alpha}{alpha} and \eqn{\lambda}{lambda} are the location and scale parameters, respectively.
}
\usage{
draw.laplace(nrep, alpha, lambda)
}
\arguments{
  \item{nrep}{Number of data points to generate.}
  \item{alpha}{Location parameter for the desired Laplace distribution.}
  \item{lambda}{Scale parameter for the desired Laplace distribution.}
}
\value{A list of length five containing generated data, the theoretical mean, the empirical mean, the theoretical variance, and the empirical variance with names y, theo.mean, emp.mean, theo.var, and emp.var, respectively.}
\examples{
draw.laplace(nrep=100000, alpha=4, lambda=2)

draw.laplace(nrep=100000, alpha=-5, lambda=4)
}
