draw.t=function(nrep,dof){
  if (dof<=1){
    stop("Degrees of freedom must be greater than 1!\n")
  } 
  x=numeric(nrep)
  for (i in 1:nrep){
    index=0
    while (index<1){
      v1=runif(1,-1,1)
      v2=runif(1,-1,1)
      r2=v1^2+v2^2 
      r=sqrt(r2)
      w=(r2<1)
      x[i]=v1*sqrt(abs((dof*(r^(-4/dof)-1)/r2)))
      index=sum(w)
    }
  }
  if(dof>1){
    emp.mean=round(mean(x), 5)
    theo.mean=0
    theo.mean=round(theo.mean, 5)
  } else {
    warning("Mean only defined when dof>1.")
    theo.mean="Mean only defined when dof>1."
    emp.mean=NA
  }
  if(dof>2){
    emp.var=round(var(x), 5)
    theo.var=dof/(dof-2)
    theo.var=round(theo.var, 5)
  } else {
    warning("Variance only defined when dof>2.")
    theo.var="Variance only defined when dof>2."
    emp.var=NA
  }
  return(list(y=x, theo.mean=theo.mean, emp.mean=emp.mean, theo.var=theo.var, emp.var=emp.var))
}