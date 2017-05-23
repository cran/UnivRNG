draw.gamma.alpha.less.than.one=function(nrep,alpha,beta){
  if (beta<=0){
    stop("Scale parameter must be positive!\n")
  } 
  if ((alpha<=0)|(alpha>=1)){
    stop("Shape parameter must be between 0 and 1!\n")
  } 
  x=numeric(nrep)
  for (i in 1:nrep){
    index=0
    while (index<1){
      u1=runif(1)
      u2=runif(1)
      t=0.07+0.75*sqrt(1-alpha)
      b=1+exp(-t)*alpha/t
      v=b*u1
      w1=(v<=1)
      w2=(v>1)
      x1=t*(v^(1/alpha))
      w11=(u2<=(2-x1)/(2+x1))
      w12=(u2<=exp(-x1))
      x[i][w1&w11]=x1[w1&w11]
      x[i][w1&!w11&w12]=x1[w1&!w11&w12]
      x2=-log(t*(b-v)/alpha)
      y=x2/t 
      w21=(u2*(alpha+y*(1-alpha))<=1)
      w22=(u2<=y^(alpha-1))
      x[i][w2&w21]=x2[w2&w21] 
      x[i][w2&!w21&w22]=x2[w2&!w21&w22] 
      index=1*(w1&w11)+1*(w1&!w11&w12)+1*(w2&w21)+1*(w2&!w21&w22)
    }
  } 
  x=beta*x
  emp.mean=round(mean(x), 5)
  emp.var=round(var(x), 5)
  theo.mean=alpha*beta
  theo.mean=round(theo.mean, 5)
  theo.var=alpha*beta^2
  theo.var=round(theo.var, 5)
  return(list(y=x, theo.mean=theo.mean, emp.mean=emp.mean, theo.var=theo.var, emp.var=emp.var))
}