\name{draw.rayleigh}
\alias{draw.rayleigh}
\title{Generates variates from Rayleigh distribution}
\description{This function implements pseudo-random number generation for a Rayleigh distribution with pdf

\deqn{f(x|\sigma)=\frac{x}{\sigma^2}e^{-x^2/2\sigma^2}}

for \eqn{x \geq 0} and \eqn{\sigma > 0} where  \eqn{\sigma} is the scale parameter.
}
\usage{
draw.rayleigh(nrep,sigma)
}
\arguments{
  \item{nrep}{Number of data points to generate.}
  \item{sigma}{Scale parameter of the desired Rayleigh distribution.}
}
\value{A list of length five containing generated data, the theoretical mean, the empirical mean, the theoretical variance, and the empirical variance with names y, theo.mean, emp.mean, theo.var, and emp.var, respectively.}
\examples{
draw.rayleigh(nrep=100000,sigma=0.5)

draw.rayleigh(nrep=100000,sigma=3)
}
