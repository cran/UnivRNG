\name{draw.zeta}
\alias{draw.zeta}
\title{Generates variates from Zeta (Zipf) distribution}
\description{This function implements pseudo-random number generation for a Zeta (Zipf) distribution with pmf

\deqn{f(x|\alpha)=\frac{1}{\zeta(\alpha)x^{\alpha}}}

for \eqn{x=1,2,3,...} and \eqn{\alpha > 1} where \eqn{\zeta(\alpha)=\sum_{x=1}^{\infty} x^{-\alpha}}.
}
\usage{
draw.zeta(nrep, alpha)
}
\arguments{
  \item{nrep}{Number of data points to generate.}
  \item{alpha}{Parameter of the desired zeta distribution.}
}
\value{A list of length five containing generated data, the theoretical mean, the empirical mean, the theoretical variance, and the empirical variance with names y, theo.mean, emp.mean, theo.var, and emp.var, respectively.}
\references{
Devroye, L. (1986). \emph{Non-Uniform random variate generation}. New York: Springer-Verlag.
}
\examples{
draw.zeta(nrep=100000,alpha=4)
}
