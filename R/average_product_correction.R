#' average product correction for normal matrix
#' @param norm_mat the normal matrix
#' @return a matrix
#' @export
average_product_correction=function(norm_mat){
  N=dim(norm_mat)[1]
  f_i=apply(norm_mat,MARGIN = 1,mean)
  f=sum(norm_mat)/N*(N-1)
  norm_mat=norm_mat- f_i%*%t(f_i)/f
  return(norm_mat)
}