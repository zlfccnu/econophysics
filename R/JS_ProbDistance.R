#'Function used to calculate the Jensen-Shannon between two matrices, i.e, the distance between eigenvalue distribution
#'  @param p1 a discrete probability density function
#'  @param p2 a discrete probability density function
#'  @return a numeric value between 0 and 1
#'  @export
JS_ProbDistance=function (p1, p2) 
{
  p1=p1+0.0000001
  p2=p2+0.0000001
  p1 <- p1/sum(p1)
  p2 <- p2/sum(p2)
  p <- 0.5 * (p1 + p2)
  JS_D <- 0.5 * sum(p1 * log(p1/p)) + 0.5 * sum(p2 * log(p2/p))
  return(sqrt(JS_D))
}