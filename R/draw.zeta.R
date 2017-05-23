draw.zeta=function(nrep,alpha){
  if (alpha<=1){
    stop("alpha must be greater than 1!\n")
  } 
  zeta=numeric(nrep)
  for (i in 1:nrep){
    index=0
    while (index<1){
      u1=runif(1)
      u2=runif(1)
      x=floor(u1^(-1/(alpha-1)))
      t=(1+1/x)^(alpha-1)
      w=x<(t/(t-1))*(2^(alpha-1)-1)/(2^(alpha-1)*u2) 
      zeta[i]=x
      index=sum(w)
    }
  }
  if(alpha>2) {
    myx=1:100
    emp.mean=round(mean(zeta), 5)
    theo.mean=sum(myx^-(alpha-1))/sum(myx^-(alpha))
    theo.mean=round(theo.mean, 5)
  } else {
    warning("Mean only defined when alpha>2.")
    theo.mean="Mean only defined when alpha>2."
    emp.mean=NA
  }
  if(alpha>3) {
    emp.var=round(var(zeta), 5)
    theo.var=sum(myx^-(alpha-2))/sum(myx^-(alpha))-(sum(myx^-(alpha-1))/sum(myx^-(alpha)))^2
    theo.var=round(theo.var, 5)
  } else {
    warning("Variance only defined when alpha>3.")
    theo.var="Variance only defined when alpha>3."
    emp.var=NA
  }
  return(list(y=zeta, theo.mean=theo.mean, emp.mean=emp.mean, theo.var=theo.var, emp.var=emp.var))
}