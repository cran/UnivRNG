\name{draw.weibull}
\alias{draw.weibull}
\title{Generates variates from Weibull distribution}
\description{This function implements pseudo-random number generation for a Weibull distribution with pdf

\deqn{f(x|\alpha,\beta)=\frac{\alpha}{\beta^{\alpha}}x^{\alpha-1}e^{-(x/\beta)^{\alpha}}}

for \eqn{0 \leq x < \infty} and \eqn{\min(\alpha,\beta)>0} where \eqn{\alpha} and \eqn{\beta} are the shape and scale parameters, respectively.
}
\usage{
draw.weibull(nrep, alpha, beta)
}
\arguments{
  \item{nrep}{Number of data points to generate.}
  \item{alpha}{Shape parameter of the desired Weibull distribution.}
  \item{beta}{Scale parameter of the desired Weibull distribution.}
}
\value{A list of length five containing generated data, the theoretical mean, the empirical mean, the theoretical variance, and the empirical variance with names y, theo.mean, emp.mean, theo.var, and emp.var, respectively.}
\examples{
draw.weibull(nrep=100000, alpha=0.5, beta=1)

draw.weibull(nrep=100000, alpha=5, beta=1)
}
