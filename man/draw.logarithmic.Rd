\name{draw.logarithmic}
\alias{draw.logarithmic}
\title{Generates variates from logarithmic distribution}
\description{This function implements pseudo-random number generation for a logarithmic distribution with pmf

\deqn{f(x|\theta) = - \frac{\theta^{x}}{x\log(1-\theta)}}

for \eqn{x=1,2,3,...} and \eqn{0 < \theta < 1}.
}
\usage{
draw.logarithmic(nrep,theta)
}
\arguments{
  \item{nrep}{Number of data points to generate.}
  \item{theta}{Rate parameter of the desired logarithmic distribution.}
}
\value{A list of length five containing generated data, the theoretical mean, the empirical mean, the theoretical variance, and the empirical variance with names y, theo.mean, emp.mean, theo.var, and emp.var, respectively.}
\references{
Kemp, A. W. Efficient generation of logarithmically distributed pseudo-random variables. \emph{Applied Statistics}, \bold{30}, 249-253.
}
\examples{
draw.logarithmic(nrep=100000,theta=0.33)

draw.logarithmic(nrep=100000,theta=0.66)
}
