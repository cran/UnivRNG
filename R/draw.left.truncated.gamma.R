draw.left.truncated.gamma=function(nrep,alpha,beta,tau){
  if(tau<0){
    stop("Cutoff point must be positive!\n")
  }
  if((alpha<=1)){
    stop("Shape parameter must be greater than 1!\n")
  }
  if((beta<=0)){
    stop("Scale parameter must be positive!\n")
  } 
  y=numeric(nrep)
  for (i in 1:nrep){
    index=0
    scaled.tau=tau/beta
    lambda=(scaled.tau-alpha+sqrt((scaled.tau- alpha)^2+4*scaled.tau))/(2*scaled.tau)
    while (index<1){
      u=runif(1)
      u1=runif(1)
      y[i]=(-log(u1)/lambda)+tau
      w=((1-lambda)*y[i]-(alpha-1)*(1+log(y[i])+log((1-lambda)/(alpha- 1)))<=-log(u))
      index=sum(w)
      }
    }
  y=y*beta
  emp.mean=round(mean(y), 5)
  emp.var=round(var(y), 5)
  theo.mean=(beta)*((gamma(alpha+1)-pgamma(tau,alpha+1)))/(gamma(alpha)-pgamma(tau,alpha))
  theo.mean=round(theo.mean, 5)
  theo.var=(beta^2)*((gamma(alpha+2)-pgamma(tau,alpha+2)))/(gamma(alpha)-pgamma(tau,alpha))-(theo.mean^2)
  theo.var=round(theo.var,5)
  return(list(y=y, theo.mean=theo.mean, emp.mean=emp.mean, theo.var=theo.var, emp.var=emp.var))
}
