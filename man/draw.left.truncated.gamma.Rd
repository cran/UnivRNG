\name{draw.left.truncated.gamma}
\alias{draw.left.truncated.gamma}
\title{Generates variates from left truncated Gamma distribution}
\description{This function implements pseudo-random number generation for a left-truncated gamma distribution with pdf

\deqn{f(x|\alpha, \beta) = \frac{1}{(\Gamma(\alpha)-\Gamma_{\tau/\beta}(\alpha))\beta^{\alpha}}x^{\alpha-1}e^{-x/\beta}}

for \eqn{0<\tau\leq x}, and min(\eqn{\tau}{tau}, \eqn{\beta}{beta})>0 where \eqn{\alpha}{alpha} and \eqn{\beta}{beta} are the shape and scale parameters, respectively, \eqn{\tau}{tau} is the cutoff point at which truncation occurs, and \eqn{\Gamma_{\tau/\beta}} is the incomplete gamma function.
}
\usage{
draw.left.truncated.gamma(nrep,alpha,beta,tau)
}
\arguments{
  \item{nrep}{Number of data points to generate.}
  \item{alpha}{Shape parameter for the desired gamma distribution.}
  \item{beta}{Scale parameter fot the desired gamma distribution.}
  \item{tau}{Point of left truncation.}
}
\value{A list of length five containing generated data, the theoretical mean, the empirical mean, the theoretical variance, and the empirical variance with names y, theo.mean, emp.mean, theo.var, and emp.var, respectively.}
\references{
Dagpunar, J. S. (1978). Sampling of variates from a truncated gamma distribution. \emph{Journal of Statistical Computation and Simulation}, \bold{8}, 59-64.
}
\examples{
draw.left.truncated.gamma(nrep=100000,alpha=5,beta=1,tau=0.5)

draw.left.truncated.gamma(nrep=100000,alpha=2,beta=2,tau=0.1)
}
