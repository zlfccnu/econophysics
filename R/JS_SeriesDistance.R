#'Function used to calculate the Jensen-Shannon between two vectors, i.e, the distance between distributions of two vectors
#'  @param x a vector
#'  @param y a vector
#'  @param n the number of equally spaced points at which the density is to be estimated
#'  @param bw the bandwidth of the density function
#'  @param thread the multi threads
#'  @return a numeric value between 0 and 1
#'  @export
JS_SeriesDistance=function (x, y, n = 512, bw = 0.01,thread=3)
{
  registerDoMC(thread)
  p1 <- density(x, n = n, bw = bw)$y
  p2 <- density(y, n = n, bw = bw)$y
  p1 <- p1/sum(p1)
  p2 <- p2/sum(p2)
  p <- 0.5 * (p1 + p2)
  JS_D <- 0.5 * sum(p1 * log(p1/p)) + 0.5 * sum(p2 * log(p2/p))
  return(sqrt(JS_D))
}