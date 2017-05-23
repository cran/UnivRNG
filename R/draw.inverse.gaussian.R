draw.inverse.gaussian=function(nrep,mu,lambda){
  if (mu<=0){
    stop("Location parameter must be positive!\n")
  }
  if (lambda<=0){
    stop("Scale parameter must be positive!\n")
  }
  inv.gaus=numeric(nrep)
  for (i in 1:nrep){
    v=rnorm(1)
    y=v^2
    x1=mu+(mu^2*y/(2*lambda))-(mu/(2*lambda))*(sqrt(4*mu*lambda*y+mu^2*y^2))
    u=runif(1)
    inv.gaus[i]=x1
    w=(u>(mu/(mu+x1)))
    inv.gaus[i][w]=mu^2/x1
  }
  emp.mean=round(mean(inv.gaus), 5)
  emp.var=round(var(inv.gaus), 5)
  theo.mean=mu
  theo.var=(mu^3)/lambda
  return(list(y=inv.gaus, theo.mean=theo.mean, emp.mean=emp.mean, theo.var=theo.var, emp.var=emp.var))
}