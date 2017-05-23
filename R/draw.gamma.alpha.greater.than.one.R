draw.gamma.alpha.greater.than.one=function(nrep,alpha,beta){
  if (beta<=0){
    stop("Scale parameter must be positive!\n")
  }
  if (alpha<=1){
    stop("Shape parameter must be greater than 1!\n")
  }
  x=numeric(nrep)
  for (i in 1:nrep){
    index=0
    while (index<1){
      u1=runif(1)
      u2=runif(1)
      v=(alpha-1/(6*alpha))*u1/((alpha-1)*u2)
      w1=((2*(u2-1)/(alpha-1))+v+(1/v)<=2)
      w2=((2*log(u2)/(alpha-1))-log(v)+v<=1)
      x[i][w1]=(alpha-1)*v
      x[i][!w1&w2]=(alpha-1)*v
      index=1*w1+1*(!w1&w2)
    }
  }
  x=x*beta
  emp.mean=round(mean(x), 5)
  emp.var=round(var(x), 5)
  theo.mean=alpha*beta
  theo.mean=round(theo.mean, 5)
  theo.var=alpha*beta^2
  theo.var=round(theo.var, 5)
  return(list(y=x, theo.mean=theo.mean, emp.mean=emp.mean, theo.var=theo.var, emp.var=emp.var))
}
