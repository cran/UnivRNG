draw.noncentral.F=function(nrep,dof1,dof2,ncp1,ncp2){
  if (ncp1<0){
    stop("Numerator non-centrality parameter must be non- negative!\n")
  }
  if (ncp2<0){
    stop("Denominator non-centrality parameter must be non- negative!\n")
  }
  if (dof1<=1){
    stop("Numerator degrees of freedom must be greater than 1!\n")
  }
  if (dof2<=1){
    stop("Denominator degrees of freedom must be greater than 1!\n")
  } 
  x=draw.noncentral.chisquared(nrep,dof1,ncp1)$y/draw.noncentral.chisquared(nrep,dof2,ncp2)$y
  return(x)
}