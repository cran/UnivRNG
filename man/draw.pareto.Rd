\name{draw.pareto}
\alias{draw.pareto}
\title{Generates variates from Pareto distribution}
\description{This function implements pseudo-random number generation for a Pareto distribution with pdf

\deqn{f(x|\alpha,\beta)=\frac{ab^{a}}{x^{a+1}}}

for \eqn{0 < b \leq x < \infty} and \eqn{a>0} where \eqn{a} and \eqn{b} are the shape and location parameters, respectively.
}
\usage{
draw.pareto(nrep,shape,location)
}
\arguments{
  \item{nrep}{Number of data points to generate.}
  \item{shape}{Shape parameter of the desired Pareto distribution.}
  \item{location}{Location parameter of the desired Pareto distribution.}
}
\value{A list of length five containing generated data, the theoretical mean, the empirical mean, the theoretical variance, and the empirical variance with names y, theo.mean, emp.mean, theo.var, and emp.var, respectively.}
\examples{
draw.pareto(nrep=100000,shape=11,location=11)

draw.pareto(nrep=100000,shape=8,location=10)
}
