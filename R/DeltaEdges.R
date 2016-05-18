DeltaEdges=function(eNum,gcNum,n){
  lowerBound=eNum[which(gcNum>=1/sqrt(n))[1]]
  upperBound=eNum[which(gcNum>=0.5)[1]]
  return(c(2*n^(2/3),(upperBound- lowerBound)*n))
}