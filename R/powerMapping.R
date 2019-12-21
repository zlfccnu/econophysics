#' power mapping the correlation matrix
#' @param corMat the original correlation matrix
#' @param q the order of the power mapping
#' @return a matrix after power mapping
#' @export

powerMapping <- function(corMat,q=1.5) {
  a=unlist(corMat)
  a=sign(a)*abs(a)^q
  corMat=matrix(data = a,nrow = dim(corMat)[1],ncol=dim(corMat)[2],dimnames = dimnames(corMat))
  return(corMat)
}