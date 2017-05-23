draw.beta.alphabeta.less.than.one=function(nrep,alpha,beta){
  if ((alpha>=1)|(alpha<=0)|(beta>=1)|(beta<=0)) {
    stop ("Both shape parameters must be between 0 and 1!\n")
  } 
  x=numeric(nrep)
  for (i in 1:nrep){
    index=0
    while (index<1){
      u1=runif(1)
      u2=runif(1)
      v1=u1^(1/alpha)
      v2=u2^(1/beta)
      summ=v1+v2
      w=(summ<=1)
      x[i]=v1/summ
      index=sum(w)
    }
  }
  emp.mean=round(mean(x), 5)
  emp.var=round(var(x), 5)
  theo.mean=(alpha)/(alpha+beta)
  theo.mean=round(theo.mean, 5)
  theo.var=(alpha*beta)/((alpha+beta)^2*(alpha+beta+1))
  theo.var=round(theo.var, 5)
  return(list(y=x, theo.mean=theo.mean, emp.mean=emp.mean, theo.var=theo.var, emp.var=emp.var))
}