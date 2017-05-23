\name{draw.t}
\alias{draw.t}
\title{Generates variates from standard t distribution}
\description{This function implements pseudo-random number generation for a standard-\eqn{t} distribution with pdf

\deqn{f(x|\nu)=\frac{\Gamma(\frac{\nu+1}{2})}{\Gamma(\frac{\nu}{2})\sqrt{\nu\pi}}(1+\frac{x^2}{\nu})^{-(\nu+1)/2}}

for \eqn{-\infty < x < \infty} where \eqn{\nu} is the degrees of freedom.
}
\usage{
draw.t(nrep,dof)
}
\arguments{
  \item{nrep}{Number of data points to generate.}
  \item{dof}{Degrees of freedom of the desired t distribution.}
}
\value{A list of length five containing generated data, the theoretical mean, the empirical mean, the theoretical variance, and the empirical variance with names y, theo.mean, emp.mean, theo.var, and emp.var, respectively.}
\references{
Bailey, R. W. (1994). Polar generation of random variates with the t-distribution. \emph{Mathematics of Computation}, \bold{62}, 779-781.
}
\examples{
draw.t(nrep=100000,dof=2)

draw.t(nrep=100000,dof=6)
}
