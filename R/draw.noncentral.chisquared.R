draw.noncentral.chisquared=function(nrep,dof,ncp){
  if (ncp<0){
    stop("Non-Centrality parameter must be non-negative!\n")
  } 
  if (dof<=1){
    stop("Degrees of freedom must be greater than 1!\n")
  } 
  x=numeric(nrep)
  for (i in 1:nrep){
    dof.int=floor(dof)
    dof.frac=dof-dof.int
    mui=sqrt(ncp/dof.int)
    jitter=0
    if (dof.frac!=0){
      jitter=rchisq(1,dof.frac)
    } 
    x[i]=sum((rnorm(dof.int)+mui)^2)+jitter
  }
  emp.mean=round(mean(x), 5)
  emp.var=round(var(x), 5)
  theo.mean=ncp+dof
  theo.mean=round(theo.mean, 5)
  theo.var=4*ncp+2*dof
  theo.var=round(theo.var, 5)
  return(list(y=x, theo.mean=theo.mean, emp.mean=emp.mean, theo.var=theo.var, emp.var=emp.var))
}