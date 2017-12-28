#' Function used to initialize some system parameters
#' @return can not be seen 

initializePars=function(){
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  options(scipen=100,max.print=2000,editor = 'vim')
  enableJIT(3)
  par(tck=0.02)
}