\name{draw.von.mises}
\alias{draw.von.mises}
\title{Generates variates from Von Mises distribution}
\description{This function implements pseudo-random number generation for a Von Mises distribution with pdf

\deqn{f(x|K)=\frac{1}{2\pi I_{0}(K)}e^{Kcos(x)}}

for \eqn{-\pi \leq x \leq \pi} and \eqn{K > 0} where \eqn{I_{0}(K)} is a modified Bessel function of the first kind of order 0.
}
\usage{
draw.von.mises(nrep,K)
}
\arguments{
  \item{nrep}{Number of data points to generate.}
  \item{K}{Parameter of the desired von Mises distribution.}
}
\value{A list of length three containing generated data, the theoretical mean, and the empirical mean with names y, theo.mean, and emp.mean, respectively.}
\references{
Best, D. J., & Fisher, N. I. (1979). Efficient simulation of the von mises distribution. \emph{Applied Statistics}, \bold{28}, 152-157.
}
\examples{
draw.von.mises(nrep=100000,K=10)

draw.von.mises(nrep=100000,K=0.5)
}
