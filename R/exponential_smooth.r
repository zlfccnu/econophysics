exponential_smooth_cor<- function(x,y,theta=NULL){
  l<- length(x)
  if(is.null(theta)){
    theta<- l/3## the best dt for moderate time series length
  }
  w_0<- (1- exp(-1/theta))/(1- exp(-l/theta))
  w<- w_0*exp(((1:l)-l)/theta)
  w<- w/sum(w)
  x_mean<- sum(w*x)
  y_mean<- sum(w*y)
  x_sigma<- sqrt(sum(w*(x-x_mean)^2))
  y_sigma<- sqrt(sum(w*(y-y_mean)^2))
  x_y_sigma<- sum(w*(x- x_mean)*(y- y_mean))
  rho<- x_y_sigma/(x_sigma*y_sigma)
  return(rho)
  }
