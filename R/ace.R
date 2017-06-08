#'adaptive cluster expansion for potts model inference
#'@param directory the working directory
#'@param input the input file name
#'@param output the ouput file name
#'@param sampleNum the length of the time series
#'@param g2 the L2 regulazation strength
#'@export
ace=function(directory,input,output,sampleNum,g2){
  commands=paste("ace","-d",directory,"-i",input,"-o",output,"-g2",g2,"-b",sampleNum)
  system(command = commands)
}