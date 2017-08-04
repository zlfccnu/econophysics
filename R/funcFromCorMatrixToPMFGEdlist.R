#' Planar Graph Edgelist
#'@param corMat a correlation matrix
#'@param outputFile a string as the name of outout file
#'@return a edgelist file in current directory
#'@export
corMat2PMFG=function(corMat,outputFile=NULL,desending=TRUE,format=c("edgelist", "pajek", "ncol","lgl","graphml", "dimacs", "gml", "dot", "leda")){
  ## construct the sorted correlation data.frame
  N=dim(corMat)[1]
  L=N*(N-1)/2
  v1=rep(0,L)
  v2=rep(0,L)
  corValue=rep(0,L)
  ##CORE LOOP
  flag=1
  for(i in c(1:N)){
    j=i+1
    while(j<=N){
      v1[flag]=i
      v2[flag]=j
      corValue[flag]=corMat[i,j]
      flag=flag+1
      j=j+1
    }
  }
  ##GET THE DATAFRAME
  corSort=data.frame(v1,v2,corValue)
  if(desending==TRUE){
    corSort=corSort[order(corSort[,3],decreasing = TRUE),]
  }else{
    corSort=corSort[order(corSort[,3],decreasing = FALSE),]
  }
  
  PMFG=graph.empty(N,directed = FALSE)
  for(i in c(1:L)){
    if(ecount(PMFG)==(3*(N-2))){
      break
    }
    else{
      edgeToAdd=corSort[1:2][i,]
      PMFG=add.edges(PMFG,edgeToAdd)
      edgelist=get.edgelist(PMFG)-1
      if(!funcPlanarTest(edgelist,N)){## c++ code using BGL
        PMFG=delete.edges(PMFG,E(PMFG,P=edgeToAdd))
      }
    }
  }
  if(!is.null(outputFile)){
    format=match.arg(format)
    write.graph(PMFG,outputFile,format=format)
  }else{
    return(PMFG)
  }
}
