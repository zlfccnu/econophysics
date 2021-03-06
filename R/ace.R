#'adaptive cluster expansion for potts model inference
#'@param directory the working directory
#'@param input the input file name
#'@param output the ouput file name
#'@param sampleNum the length of the time series
#'@param g2 the L2 regulazation strength
ace=function(directory,input,output,sampleNum,g2){
  input=system(paste("basename",input,".p"),intern = TRUE)
  output=system(paste("basename",output,".j"),intern = TRUE)
  commands=paste("ace","-d",directory,"-i",input,"-o",output,"-g2",g2,"-b",sampleNum)
  system(command = commands)
}