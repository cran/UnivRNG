draw.laplace=function(nrep, alpha, lambda){
  if(lambda<=0){
      stop("Scale parameter must be positive!\n")
  } 
  y=rexp(nrep,lambda)
  change.sign=sample(c(0,1), nrep, replace = TRUE)
  y[change.sign==0]=-y[change.sign==0]
  laplace=y+alpha 
  emp.mean=round(mean(laplace), 5)
  emp.var=round(var(laplace), 5)
  theo.mean=alpha
  theo.var=2/(lambda^2)
  return(list(y=laplace, theo.mean=theo.mean, emp.mean=emp.mean, theo.var=theo.var, emp.var=emp.var))
}