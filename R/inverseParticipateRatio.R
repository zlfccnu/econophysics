#'Function used to calculate the inverse participate ratio
#'@param mat a matrix
#'@return a vector of length equals to the number of the eigenvalue
#'@export

inverseParticipateRatio=function(mat){
  eigenValue=eigen(mat)
  ipr=apply(eigenValue$vector, 2,function(x){sum(x^4)})
  iprDF=cbind(eigenValue$values,ipr)
  colnames(iprDF)<- c("values","ipr")
  return(iprDF)
}