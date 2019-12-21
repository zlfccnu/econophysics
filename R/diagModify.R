#' change the diag function in order to complain with pipe operator
#' @param x a matrix
#' @param diagValue a numeric value or a vector
#' @return a matrix
#' @export
diagModify=function(x,diagValue){
  diag(x)=diagValue
  return(x)
}