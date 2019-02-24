\name{UnivRNG-package}
\alias{UnivRNG-package}
\alias{UnivRNG}
\docType{package}
\title{
Univariate Pseudo-Random Number Generation
}
\description{
This package implements the algorithms described in Demirtas (2005) for pseudo-random number generation of 17 univariate distributions. The following distributions are available: Left Truncated Gamma, Laplace, Inverse Gaussian, Von Mises, Zeta (Zipf), Logarithmic, Beta-Binomial, Rayleigh, Pareto, Non-central \eqn{t}, Non-central Chi-squared, Doubly non-central \eqn{F}, Standard \eqn{t}, Weibull, Gamma with \eqn{\alpha}{alpha}<1, Gamma with \eqn{\alpha}{alpha}>1, and Beta with \eqn{\alpha}{alpha}<1 and \eqn{\beta}{beta}<1. For some distributions, functions that have similar capabilities exist in the base package; the functions herein should be regarded as complementary tools.

The methodology for each random-number generation procedure varies and each distribution has its own function. \code{draw.left.truncated.gamma}, \code{draw.von.mises}, \code{draw.inverse.gaussian}, \code{draw.zeta}, \code{draw.gamma.alpha.less.than.one}, and \code{draw.beta.alphabeta.less.than.one} are based on acceptance/rejection region techniques. \code{draw.rayleigh}, \code{draw.pareto}, and \code{draw.weibull} utilize the inverse CDF method. The chop-down method is used for \code{draw.logarithmic}. In \code{draw.laplace}, a sample from an exponential distribution with mean \eqn{1/\lambda}{lambda} is generated and subsequently the sign is changed with probability 0.5 and all variables are shifted by \eqn{\alpha}{alpha}. For the Beta-Binomial distribution in \code{draw.beta.binomial}, \eqn{\pi}{pi} is generated as the appropriate \eqn{\beta}{beta} and used as the success probability for the binomial portion. \code{draw.noncentral.t} utilizes on arithmetic functions of normal and chi-squared random variables. \code{draw.noncentral.chisquared} is based on the sum of squared random normal variables, and \code{draw.noncentral.F} is a ratio of chi-squared random variables generated via \code{draw.noncentral.chisquared}. \code{draw.t} employs a rejection polar method developed by Bailey (1994). \code{draw.gamma.alpha.greater.than.one} uses a ratio of uniforms method by Cheng and Feast (1979).
}
\details{
\tabular{ll}{
Package: \tab UnivRNG\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2019-02-22\cr
License: \tab GPL-2 | GPL-3\cr
}}
\author{
Hakan Demirtas, Rawan Allozi, Ran Gao

Maintainer: Ran Gao <rgao8@uic.edu>
}
\references{
Bailey, R. W. (1994). Polar generation of random variates with the t-distribution. \emph{Mathematics of Computation}, \bold{62}, 779-781.

Cheng, R. C. H., & Feast, G. M. (1979). Some simple gamma variate generation. \emph{Applied Statistics}, \bold{28}, 290-295.

Demirtas, H. (2005). Pseudo-random number generation in R for some univariate distributions. \emph{Journal of Modern Applied Statistical Methods}, \bold{4(1)}, 300-311.
}
