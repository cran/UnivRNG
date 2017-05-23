draw.rayleigh=function(nrep,sigma){
  if (sigma<=0){
    stop("Standard deviation must be positive!\n")
  } 
  u=runif(nrep)
  rayl=sigma*sqrt(-2*log(u))
  emp.mean=round(mean(rayl), 5)
  emp.var=round(var(rayl), 5)
  theo.mean=sigma*sqrt(pi/2)
  theo.mean=round(theo.mean, 5)
  theo.var=(sigma^2)*(4-pi)/2
  theo.var=round(theo.var, 5)
  return(list(y=rayl, theo.mean=theo.mean, emp.mean=emp.mean, theo.var=theo.var, emp.var=emp.var))
}