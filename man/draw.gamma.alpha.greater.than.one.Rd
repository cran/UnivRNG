\name{draw.gamma.alpha.greater.than.one}
\alias{draw.gamma.alpha.greater.than.one}
\title{Generates variation from Gamma distribution with \eqn{\alpha>1}}
\description{This function implements pseudo-random number generation for a Gamma distribution for \eqn{\alpha>1} with pdf

\deqn{f(x|\alpha,\beta)=\frac{1}{\Gamma(\alpha)\beta^{\alpha}}x^{\alpha-1}e^{-x/\beta}}

for \eqn{0 \leq x < \infty} and \eqn{\min(\alpha,\beta)>0} where \eqn{\alpha} and \eqn{\beta} are the shape and scale parameters, respectively.
}
\usage{
draw.gamma.alpha.greater.than.one(nrep,alpha,beta)
}
\arguments{
  \item{nrep}{Number of data points to generate.}
  \item{alpha}{Shape parameter for desired gamma distribution. Must be greater than 1.}
  \item{beta}{Scale parameter for desired gamma distribution.}
}
\value{A list of length five containing generated data, the theoretical mean, the empirical mean, the theoretical variance, and the empirical variance with names y, theo.mean, emp.mean, theo.var, and emp.var, respectively.}
\references{
Cheng, R. C. H., & Feast, G. M. (1979). Some simple gamma variate generation. \emph{Applied Statistics}, \bold{28}, 290-295.
}
\examples{
draw.gamma.alpha.greater.than.one(nrep=100000,alpha=2,beta=2)

draw.gamma.alpha.greater.than.one(nrep=100000,alpha=3,beta=0.4)
}
