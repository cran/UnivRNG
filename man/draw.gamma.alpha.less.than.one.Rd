\name{draw.gamma.alpha.less.than.one}
\alias{draw.gamma.alpha.less.than.one}
\title{Generates variation from Gamma distribution with \eqn{\alpha<1}}
\description{This function implements pseudo-random number generation for a gamma distribution for \eqn{\alpha<1} with pdf

\deqn{f(x|\alpha,\beta)=\frac{1}{\Gamma(\alpha)\beta^{\alpha}}x^{\alpha-1}e^{-x/\beta}}

for \eqn{0 \leq x < \infty} and \eqn{\min(\alpha,\beta)>0} where \eqn{\alpha} and \eqn{\beta} are the shape and scale parameters, respectively.
}
\usage{
draw.gamma.alpha.less.than.one(nrep,alpha,beta)
}
\arguments{
  \item{nrep}{Number of data points to generate.}
  \item{alpha}{Shape parameter for desired gamma distribution. Must be less than 1.}
  \item{beta}{Scale parameter for desired gamma distribution.}
}
\value{A list of length five containing generated data, the theoretical mean, the empirical mean, the theoretical variance, and the empirical variance with names y, theo.mean, emp.mean, theo.var, and emp.var, respectively.}
\references{
Ahrens, J. H., & Dieter, U. (1974). Computer methods for sampling from gamma, beta, poisson and binomial distributions. \emph{Computing}, \bold{1}, 223-246.
}
\examples{
draw.gamma.alpha.less.than.one(nrep=100000,alpha=0.5,beta=2)
}
