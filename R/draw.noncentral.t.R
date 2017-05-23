draw.noncentral.t=function(nrep,nu,lambda){
  if (nu<=1){
    stop("Degrees of freedom must be greater than 1!\n")
  }
  x=numeric(nrep)
  for (i in 1:nrep){
    x[i]=rt(1,nu)+(lambda/sqrt(rchisq(1,nu)/nu))
  }
  if(nu>1){
    emp.mean=round(mean(x), 5)
    theo.mean=sqrt(nu/2)*lambda*gamma((nu-1)/2)/gamma(nu/2)
    theo.mean=round(theo.mean, 5)
  } else {
    warning("Mean only defined when nu>1.")
    theo.mean="Mean only defined when nu>1."
    emp.mean=NA
  }
  if(nu>2){
    emp.var=round(var(x), 5)
    theo.var=(1+lambda^2)*nu/(nu-2)-(theo.mean^2)
    theo.var=round(theo.var, 5)
  } else {
    warning("Variance only defined when nu>2.")
    theo.var="Variance only defined when nu>2."
    emp.var=NA
  }
  return(list(y=x, theo.mean=theo.mean, emp.mean=emp.mean, theo.var=theo.var, emp.var=emp.var))
}