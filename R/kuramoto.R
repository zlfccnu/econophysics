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
kuramoto=function(graph,h=0.01,phase=runif(N,0,2*pi),natFeq=rnorm(N),thread=3,steps=1000,lambda=0.1,weight=TRUE,N=vcount(graph)){
  
  N=vcount(graph)
  if(weight==TRUE){
    adjMat=get.adjacency(graph,sparse = FALSE,attr = "weight")
  }else{
    adjMat=get.adjacency(graph,sparse = FALSE)
  }
  order_parameter=NULL
  
  for(j in 1:steps){
    registerDoMC(thread)
    phase=foreach(i=1:N,.combine = "c")%dopar%{
      adjNode=neighborhood(graph,i,order = 1)
      couple_vec=lambda*adjMat[i,]
      couple_vec[i]=0 ##i does not couple with itself
      RK(theta_i = phase[i],theta_j = phase,couple_vec = couple_vec,h=h,natFreq = natFeq[i])
    }
    
    complex_phase=complex(imaginary = phase)
    order_parameter=c(order_parameter,Mod(mean(exp(complex_phase))))
  }
  
  return(list(order_parameter,phase))
}
