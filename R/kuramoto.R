#' simulate the kuramoto model
#' @param graph the network structure, can be weighted
#' @param h the stepsize for RK method
#' @param phase the initial phase of the oscillators
#' @param natFreq the nature frequency of those oscillators
#' @param thread the multi threads number
#' @param steps the number of steps of the simulation
#' @param lambda the coupling strength between the oscillators
#' @param weight a logical value, whether use edge weight or not
#' @param N the size of the network
#' @return a list with the order parameter and the phase
#' @export
kuramoto=function(graph=NULL,adjMat,h=0.01,phase=runif(N,0,2*pi),natFreq=rnorm(N),thread=3,steps=1000,lambda=0.1,weight=TRUE,N=vcount(graph)){
  if(is.null(graph)){
    adjMat=adjMat
    N=dim(adjMat)[1]
  }else{
    N=vcount(graph)
    if(weight==TRUE){
      adjMat=get.adjacency(graph,sparse = FALSE,attr = "weight")
    }else{
      adjMat=get.adjacency(graph,sparse = FALSE)
    }
  }
  
  adjMat=lambda*adjMat## construct the effective adjMat
  diag(adjMat)<- 0 ## not couple with itself
  order_parameter=double(steps)
  registerDoMC(thread)
  for(j in 1:steps){
      phase=foreach(i=1:N,.combine = "c")%dopar%{
      RK(theta_i = phase[i],theta_j = phase,couple_vec = adjMat[i,],h=h,natFreq = natFreq[i])
    }
    
    complex_phase=complex(imaginary = phase)
    order_parameter[j]=Mod(mean(exp(complex_phase)))
  }
  

  return(list(order_parameter,phase))
}
