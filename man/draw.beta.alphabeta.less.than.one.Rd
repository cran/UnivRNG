\name{draw.beta.alphabeta.less.than.one}
\alias{draw.beta.alphabeta.less.than.one}
\title{Generates variates from Beta distribution with max\eqn{(\alpha,\beta)<1}}
\description{This function implements pseudo-random number generation for a Beta distribution for \eqn{\max(\alpha,\beta)<1} with pdf

\deqn{f(x|\alpha,\beta)=\frac{1}{B(\alpha,\beta)}x^{\alpha-1}(1-x)^{\beta-1}}

for \eqn{0 \leq x \leq 1}, \eqn{0 < \alpha < 1}, and \eqn{0 < \beta < 1} where \eqn{\alpha} and \eqn{\beta} are the shape parameters and \eqn{B(\alpha,\beta)} is the complete beta function.
}
\usage{
draw.beta.alphabeta.less.than.one(nrep,alpha,beta)
}
\arguments{
  \item{nrep}{Number of data points to generate.}
  \item{alpha}{First shape parameter. Must be less than 1.}
  \item{beta}{Second shape parameter. Must be less than 1.}
}
\value{A list of length five containing generated data, the theoretical mean, the empirical mean, the theoretical variance, and the empirical variance with names y, theo.mean, emp.mean, theo.var, and emp.var, respectively.}
\references{
Jhonk, M. D. (1964). Erzeugung von betaverteilter und gammaverteilter zufallszahlen. \emph{Metrika}, \bold{8}, 5-15.
}
\examples{
draw.beta.alphabeta.less.than.one(nrep=100000,alpha=0.7,beta=0.4)
}
