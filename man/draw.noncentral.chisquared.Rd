\name{draw.noncentral.chisquared}
\alias{draw.noncentral.chisquared}
\title{Generates variates from non-central chi-squared distribution}
\description{This function implements pseudo-random number generation for a non-central chi-squared distribution with pdf

\deqn{f(x|\lambda,\nu)=\frac{e^{-(x+\lambda)/2}x^{\nu/2-1}}{2^{\nu/2}} \sum_{k=0}^{\infty} \frac{(\lambda x)^{k}}{4^{k}k!\Gamma(k+\nu/2)}}

for \eqn{0 \leq x < \infty}, \eqn{\lambda>0}, and \eqn{\nu>1}, where \eqn{\lambda} is the non-centrality parameter and \eqn{\nu} is the degrees of freedom.
}
\usage{
draw.noncentral.chisquared(nrep,dof,ncp)
}
\arguments{
  \item{nrep}{Number of data points to generate.}
  \item{dof}{Degrees of freedom of the desired non-central chi-squared distribution.}
  \item{ncp}{Non-centrality parameter of the desired non-central chi-squared distribution.}
}
\value{A list of length five containing generated data, the theoretical mean, the empirical mean, the theoretical variance, and the empirical variance with names y, theo.mean, emp.mean, theo.var, and emp.var, respectively.}
\examples{
draw.noncentral.chisquared(nrep=100000,dof=2,ncp=1)

draw.noncentral.chisquared(nrep=100000,dof=5,ncp=2)
}
