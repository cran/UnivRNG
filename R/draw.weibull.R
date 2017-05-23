draw.weibull=function(nrep, alpha, beta){
  if ((alpha<=0)|(beta<=0)){
    stop("alpha and beta must be positive!\n")
  }
  u=runif(nrep)
  weibull=beta*((-log(u))^(1/alpha))
  emp.mean=round(mean(weibull), 5)
  emp.var=round(var(weibull), 5)
  theo.mean=beta*gamma(1+(1/alpha))
  theo.mean=round(theo.mean, 5)
  theo.var=beta^2*(gamma(1+(2/alpha))-(gamma(1+(1/alpha)))^2)
  theo.var=round(theo.var, 5)
  return(list(y=weibull, theo.mean=theo.mean, emp.mean=emp.mean, theo.var=theo.var, emp.var=emp.var))
}