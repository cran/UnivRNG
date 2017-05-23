\name{draw.noncentral.t}
\alias{draw.noncentral.t}
\title{Generates variates from doubly non-central t distribution}
\description{This function implements pseudo-random number generation for a non-central \eqn{t} distribution

\deqn{\frac{Y}{\sqrt{U/\nu}}}

where \eqn{U} is a central chi-square random variable with \eqn{\nu} degrees of freedom and  \eqn{Y} is an independent, normally distributed random variable with variance 1 and mean \eqn{\lambda}. 
}
\usage{
draw.noncentral.t(nrep,nu,lambda)
}
\arguments{
  \item{nrep}{Number of data points to generate.}
  \item{nu}{Degrees of freedom of the desired non-central t distribution.}
  \item{lambda}{Non-centrality parameter of the desired non-central t distribution.}
}
\value{A list of length five containing generated data, the theoretical mean, the empirical mean, the theoretical variance, and the empirical variance with names y, theo.mean, emp.mean, theo.var, and emp.var, respectively.}
\examples{
draw.noncentral.t(nrep=100000,nu=4,lambda=2)

draw.noncentral.t(nrep=100000,nu=5,lambda=1)
}
