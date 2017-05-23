draw.logarithmic=function(nrep,theta){
  if ((theta<=0)|(theta>1)){
    stop("theta must be between 0 and 1!\n")
  } 
  x=numeric(nrep)
  for (i in 1:nrep){
    index=0
    x0=1
    u=runif(1)
    while (index<1){
      t=-(theta^x0)/(x0*log(1-theta))
      px=t
      w=(u<=px)
      x[i]=x0
      u=u-px
      index=sum(w)
      x0=x0+1
    }
  }
  emp.mean=round(mean(x), 5)
  emp.var=round(var(x), 5)
  theo.mean=-(theta/(1-theta))*(1/log(1-theta))
  theo.mean=round(theo.mean, 5)
  theo.var=-theta*(theta+log(1-theta))/((1-theta)^2*(log(1-theta))^2)
  theo.var=round(theo.var, 5)
  return(list(y=x, theo.mean=theo.mean, emp.mean=emp.mean, theo.var=theo.var, emp.var=emp.var))
}