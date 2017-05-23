\name{draw.noncentral.F}
\alias{draw.noncentral.F}
\title{Generates variates from doubly non-central F distribution}
\description{This function implements pseudo-random number generation for a doubly non-central \eqn{F} distribution 

\deqn{F=\frac{X_{1}^{2}/n}{X_{2}^{2}/m}}

where \eqn{X_{1}^{2}\sim \chi^{2}(n,\lambda_{1})}, \eqn{X_{2}^{2}\sim \chi^{2}(m,\lambda_{2})}, \eqn{n} and \eqn{m} are numerator and denominator degrees of freedom, respectively, and \eqn{\lambda_{1}} and \eqn{\lambda_{2}} are the numerator and denominator non-centrality parameters, respectively. It includes central and singly non-central F distributions as a special case.
}
\usage{
draw.noncentral.F(nrep,dof1,dof2,ncp1,ncp2)
}
\arguments{
  \item{nrep}{Number of data points to generate.}
  \item{dof1}{Numerator degress of freedom.}
  \item{dof2}{Denominator degrees of freedom.}
  \item{ncp1}{Numerator non-centrality parameter.}
  \item{ncp2}{Denominator non-centrality parameter.}
}
\value{A vector containing generated data.}
\seealso{\code{\link{draw.noncentral.chisquared}}}
\examples{
draw.noncentral.F(nrep=100000,dof1=2,dof2=4,ncp1=2,ncp2=4)
}
