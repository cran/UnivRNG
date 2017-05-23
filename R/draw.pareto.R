draw.pareto=function(nrep,shape,location){
  if(shape<=0){
    stop("Shape parameter must be positive!\n")
  }
  if(location<=0){
    stop("Location parameter must be positive!\n")
  }
  u=runif(nrep)
  pareto=location/(u^(1/shape))
  if(shape>1){
    emp.mean=round(mean(pareto), 5)
    theo.mean=(shape*location)/(shape-1)
    theo.mean=round(theo.mean, 5)
  } else {
    warning("Mean only defined when shape>1.")
    theo.mean="Mean only defined when shape>1."
    emp.mean=NA
  }
  if(shape>2){
    emp.var=round(var(pareto), 5)
    theo.var=(shape*location^2)/((shape-2)*(shape-1)^2)
    theo.var=round(theo.var, 5)
  } else {
    warning("Variance only defined when shape>2.")
    theo.var="Variance only defined when shape>2."
    emp.var=NA
  }
  return(list(y=pareto, theo.mean=theo.mean, emp.mean=emp.mean, theo.var=theo.var, emp.var=emp.var))
}