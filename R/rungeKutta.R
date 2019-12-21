#' runge kutta method for kuramoto model
#' @param theta_i the phase for the focal oscillator
#' @param theta_j the phase vector for the neighbor of i
#' @param h the stepsize of the RK method
#' @param couple_vec the couping vector for i and its neighbors
#' @param natFreq the nature frequency for the oscillator
#' @return the phase of the focal oscillator at next time
#' @export

RK=function(theta_i,theta_j,h,couple_vec,natFreq){
  K_1=natFreq + sum(couple_vec*sin(theta_j-theta_i))
  K_2=natFreq + sum(couple_vec*sin(theta_j-theta_i+K_1*h/2))
  K_3=natFreq + sum(couple_vec*sin(theta_j-theta_i+K_2*h/2))
  K_4=natFreq + sum(couple_vec*sin(theta_j-theta_i+K_3*h))
  theta_i=theta_i+h*((K_1+2*K_2+2*K_3+K_4)/6)
  return(theta_i)
}