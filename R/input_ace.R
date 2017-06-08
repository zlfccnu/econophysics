#'convert from configuration to state probability and joint probability
#'@param x a data.frame for configuration
#'@param p_theta the threashold for the probability
#'@param thread the multithread number
#'@param fileName the outpu file name
#'@export
input_ace=function(x,p_theta=0.05,thread=3,fileName){
  ## be sure the output file can be created
  if(file.exists(fileName)){
    readline(prompt=paste("remove",fileName,"Press [enter] to continue! OR [ESC] to escape!"))
    file.remove(fileName)
  }
  
  stateP=function(x,p_theta=0.05,thread=3){
    cl=makeCluster(thread)
    sampleLength=dim(x)[1]
    statep=parApply(cl,x,MARGIN = 2,function(x,sampleLength,p_theta){
      x=as.vector(x)
      x=table(x)/sampleLength
      x=x[which(x>=p_theta)]
      return(x)
    },sampleLength=sampleLength,p_theta=p_theta)
    stopCluster(cl)
    ifelse(is.list(statep),return(statep),return(df2list(t(statep))))
    
  }
  
  statePP=function(statep,thread=3){
    cl=makeCluster(thread)
    index=combn(length(statep),2)
    statepp=parApply(cl,index,MARGIN = 2,function(index,statep){
      P_P=expand.grid(statep[index])
      P_P=as.numeric(P_P[,1]*P_P[,2])
      return(P_P)
    },statep=statep)
    stopCluster(cl)
    ifelse(is.list(statepp),return(statepp),return(df2list(t(statepp))))
    
  }
  
  state_p=stateP(x = x,p_theta = p_theta,thread = thread)
  state_pp=statePP(statep  = state_p,thread = thread)
  state_input=state_p
  state_input[(length(state_input)+1):(length(state_input)+length(state_pp))]=state_pp
  state_input=lapply(state_input,as.numeric)
  
  cl=makeCluster(thread)
  parLapply(cl=cl,X=state_input,fun = function(x){
    write.table(matrix(x,nrow=1),fileName,append = T,sep=" ",col.names = FALSE,row.names = FALSE,quote = FALSE)
  })
  
}