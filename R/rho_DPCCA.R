library("MASS")
#' Function used to calculate the DPCCA partial coefficient
#' @param corMat the DCCA correlation matrix
#' @return the partial correlation matrix
#' @export

rho_DPCCA=function(corMat){
  invCorMat=MASS::ginv(corMat)
  colnames(invCorMat)=colnames(corMat)
  rownames(invCorMat)=rownames(corMat)
  invCorMatTemp=invCorMat
  for(i in 1:(dim(invCorMat)[1]-1)){
    for(j in (i+1):dim(invCorMat)[1]){
      invCorMat[i,j] = -invCorMatTemp[i,j]/sqrt(invCorMatTemp[i,i]*invCorMatTemp[j,j])
      invCorMat[j,i] =invCorMat[i,j]
    }
  }
  diag(invCorMat)<- 1
  return(invCorMat)
}