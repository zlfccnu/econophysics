#' Function used to calculate the Jensen-Shannon between two matrices, i.e, the distance between eigenvalue distribution
#' @param mat1 a matrix
#' @param mat2 a matrix
#' @param n the number of equally spaced points at which the density is to be estimated
#' @param bw the bandwidth of the density function
#' @param thread the multi threads
#' @return a numeric value between 0 and 1

JS_MatrixDistance=function (mat1, mat2, n = 512, bw = 0.01,thread=3) 
{
  
  registerDoMC(thread)
  eigenValues <- foreach(mat = list(mat1, mat2)) %dopar% 
  {
    eigen(mat, only.values = TRUE)$values
  }
  p1 <- density(eigenValues[[1]], n = n, bw = bw)$y
  p2 <- density(eigenValues[[2]], n = n, bw = bw)$y
  p1 <- p1/sum(p1)
  p2 <- p2/sum(p2)
  p <- 0.5 * (p1 + p2)
  JS_D <- 0.5 * sum(p1 * log(p1/p)) + 0.5 * sum(p2 * log(p2/p))
  return(sqrt(JS_D))
}