draw.von.mises=function(nrep,K){
  if (K<=0){
    stop("K must be positive!\n")
  }
  x=numeric(nrep)
  for (i in 1:nrep){
    index=0
    while (index<1){
      u1=runif(1)
      u2=runif(1)
      u3=runif(1)
      tau=1+(1+4*K^2)^0.5
      rho=(tau-(2*tau)^0.5)/(2*K)
      r=(1+rho^2)/(2*rho)
      z=cos(pi*u1)
      f=(1+r*z)/(r+z)
      c=K*(r-f)
      w1=(c*(2-c)-u2>0)
      w2=(log(c/u2)+1-c>=0)
      y=sign(u3-0.5)*acos(f)
      x[i][w1|w2]=y 
      index=1*(w1|w2)
      }
  }
  emp.mean=round(mean(x), 5)
  return(list(y=x, theo.mean=0, emp.mean=emp.mean))
}