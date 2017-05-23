\name{draw.beta.binomial}
\alias{draw.beta.binomial}
\title{Generates variates from Beta-binomial distribution}
\description{This function implements pseudo-random number generation for a Beta-binomial distribution with pmf

\deqn{f(x|n,\alpha,\beta) = \frac{n!}{x!(n-x)!B(\alpha,\beta)}\int_{0}^{1}\pi^{\alpha-1+x}(1-\pi)^{n+\beta-1-x}d\pi}

for \eqn{x=0,1,2,...}, \eqn{\alpha>0}, and \eqn{\beta>0}, where \eqn{n} is the sample size, \eqn{\alpha} and \eqn{\beta} are the shape parameters and \eqn{B(\alpha,\beta)} is the complete beta function.
}
\usage{
draw.beta.binomial(nrep,alpha,beta,n)
}
\arguments{
  \item{nrep}{Number of data points to generate.}
  \item{alpha}{First shape parameter.}
  \item{beta}{Second shape parameter.}
  \item{n}{Number of trials.}
}
\value{A list of length five containing generated data, the theoretical mean, the empirical mean, the theoretical variance, and the empirical variance with names y, theo.mean, emp.mean, theo.var, and emp.var, respectively.}
\examples{
draw.beta.binomial(nrep=100000,alpha=0.2,beta=0.25,n=10)

draw.beta.binomial(nrep=100000,alpha=2,beta=3,n=10)

draw.beta.binomial(nrep=100000,alpha=600,beta=400,n=20)
}
