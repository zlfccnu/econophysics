#' element wise add function for matrix
#' @param x a list of matrices
#' @return a matrix
#' @export
elementWiseMatrixAdd <- function(x) Reduce("+", x)