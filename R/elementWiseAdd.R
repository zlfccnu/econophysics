#' element wise add function for matrix
#' @param x a list of matrices
#' @return a matrix
elementWiseAdd <- function(x) Reduce("+", x)