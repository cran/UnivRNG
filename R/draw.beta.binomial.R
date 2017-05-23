draw.beta.binomial=function(nrep,alpha,beta,n){
  if((alpha<=0)|(beta<=0)){
    stop("alpha and beta must be positive!\n")
  } 
  if (floor(n)!=n){
    stop("Size must be an integer!\n")
  }
  if (floor(n)<2){
    stop("Size must be greater than 2!\n")
  } 
  beta.variates=numeric(nrep)
  beta.binom=numeric(nrep)
  for (i in 1:nrep){
    beta.variates[i]=rbeta(1,alpha,beta) 
    beta.binom[i]=rbinom(1,n,beta.variates[i])
  }
  emp.mean=round(mean(beta.binom), 5)
  emp.var=round(var(beta.binom), 5)
  theo.mean=(n*alpha)/(alpha+beta)
  theo.mean=round(theo.mean, 5)
  theo.var=(n*alpha*beta*(alpha+beta+n))/((alpha+beta)^2*(alpha+beta+1))
  theo.var=round(theo.var, 5)
  return(list(y=beta.binom, theo.mean=theo.mean, emp.mean=emp.mean, theo.var=theo.var, emp.var=emp.var))
}