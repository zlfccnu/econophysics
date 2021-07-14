#' Function to calculate the modified RV coefficient, i.e, the correlation coefficient between matrices
#' @param mat1 the first matrix
#' @param mat2 the second matrix
#' @return a value between -1 and 1
#' @export

RV_Mat=function(mat1,mat2){
  return(sum(diag(t(mat1)%*%mat2))/sqrt(sum(diag(t(mat1)%*%mat1))*sum(diag(t(mat2)%*%mat2))))
}