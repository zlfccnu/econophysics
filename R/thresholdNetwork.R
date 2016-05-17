rankBasedThresholdNetwork = function(MAT, mix=FALSE, d = 2) {
  
  ## edgelist for both ascending and descending order
  edgeListAs = data.frame()
  edgeListDe = data.frame()
  nameList = rownames(MAT)
  for (i in 1:d) {
    ## the index for the d_th largest elements
    largeIndex = apply(MAT, 1, function(x) {
      which(rank(-x) == i)
    })
    largeEdgeList=cbind(nameList, nameList[largeIndex])
    largeEdgeList=as.data.frame(largeEdgeList)
    corVlues=apply(MAT, 1, function(x) {
      sort(x,TRUE)[i]
    })
    largeEdgeList=cbind(corVlues,largeEdgeList)
    ## sort
    largeEdgeListAs=largeEdgeList[order(largeEdgeList[,1]),]#ascending
    largeEdgeListDe=largeEdgeList[order(-largeEdgeList[,1]),]#descending
    edgeListAs = rbind(edgeListAs,largeEdgeListAs)
    edgeListDe = rbind(edgeListDe,largeEdgeListDe)
  }
  colnames(edgeListAs) = c("cor","vertex1", "vertex2")
  colnames(edgeListDe) = c("cor","vertex1", "vertex2")
  ## mixed rank or not
  if(mix==TRUE){
    edgeListAs=edgeListAs[order(edgeListAs[,1]),]
    edgeListDe=edgeListDe[order(-edgeListDe[,1]),]
  }
  
  ## GRAPH
  GRAPH_As = graph.empty(n = 0)+ vertices(nameList)
  GRAPH_De = graph.empty(n = 0)+ vertices(nameList)
  GRAPH_ListAs=list()
  GRAPH_ListDe=list()
  for(i in 1:dim(edgeListAs)[1]){
    GRAPH_As=GRAPH_As+edge(edgeListAs[i,2:3])
    if(is.simple(GRAPH_As)){
      GRAPH_ListAs[[length(GRAPH_ListAs)+1]]=GRAPH_As
    }else{
      GRAPH_As=simplify(GRAPH_As)
    }
    
    GRAPH_De=GRAPH_De+edge(edgeListDe[i,2:3])
    if(is.simple(GRAPH_De)){
      GRAPH_ListDe[[length(GRAPH_ListDe)+1]]=GRAPH_De
    }else{
      GRAPH_De=simplify(GRAPH_De)
    }
  }
  return(list(GRAPH_ListAs,GRAPH_ListDe))
}