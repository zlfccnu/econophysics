#' Function used to construct the rank based threashold network
#' @param MAT the original correlation matrix
#' @param mix whether use the mixed rank or not
#' @param d the d_th largest elements should be used
#' @return a dataframe
#' @export
rankBasedThresholdNetworkStatiscs = function(MAT, mix=FALSE, d = 2) {

    ## edgelist for both ascending and descending order
    edgeListAs = data.frame()
    edgeListDe = data.frame()
    if(is.null(colnames(MAT))){
      colnames(MAT)= 1:dim(MAT)[1]
      rownames(MAT)=1:dim(MAT)[1]
    }
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
    GRAPH_As = graph.empty(n = 0,directed = FALSE)+ vertices(nameList)
    GRAPH_De = graph.empty(n = 0,directed = FALSE)+ vertices(nameList)
    vNum=vcount(GRAPH_As)
    edgeNodeRatioAs=c()
    edgeNodeRatioDe=c()
    norGiantSizeAs=c()
    norGiantSizeDe=c()
    plfitKSsAs=c()
    plfitKSsDe=c()
    plfitKSpAs=c()
    plfitKSpDe=c()
    meanClusterSizeAs=c()
    meanClusterSizeDe=c()
    for(i in 1:dim(edgeListAs)[1]){
      GRAPH_As=GRAPH_As+edge(edgeListAs[i,2],edgeListAs[i,3])
      csizeAs=clusters(GRAPH_As)
      if(is.simple(GRAPH_As)){
        edgeNodeRatioAs=append(edgeNodeRatioAs,ecount(GRAPH_As)/vNum)
        norGiantSizeAs=append(norGiantSizeAs,max(csizeAs$csize)/vNum)
        meanClusterSizeAs=append(meanClusterSizeAs,mean(sort(csizeAs$csize,decreasing = TRUE)[-1]^2)/mean(sort(csizeAs$csize,decreasing = TRUE)[-1]))
        plfitAs=power.law.fit(csizeAs$csize,1)
        plfitKSsAs=append(plfitKSsAs,plfitAs$KS.stat)
        plfitKSpAs=append(plfitKSpAs,plfitAs$KS.p)
        
      }else{
        GRAPH_As=simplify(GRAPH_As)
      }
      
      GRAPH_De=GRAPH_De+edge(edgeListDe[i,2],edgeListDe[i,3])
      csizeDe=clusters(GRAPH_De)
      if(is.simple(GRAPH_De)){
        edgeNodeRatioDe=append(edgeNodeRatioDe,ecount(GRAPH_De)/vNum)
        norGiantSizeDe=append(norGiantSizeDe,max(csizeDe$csize)/vNum)
        meanClusterSizeDe=append(meanClusterSizeDe,mean(sort(csizeDe$csize,decreasing = TRUE)[-1]^2)/mean(sort(csizeDe$csize,decreasing = TRUE)[-1]))
        plfitDe=power.law.fit(csizeDe$csize,1)
        plfitKSsDe=append(plfitKSsDe,plfitDe$KS.stat)
        plfitKSpDe=append(plfitKSpDe,plfitDe$KS.p)
      }else{
        GRAPH_De=simplify(GRAPH_De)
      }
    }
    
    ## return vlues
      return(as.data.frame(cbind(edgeNodeRatioAs,norGiantSizeAs,meanClusterSizeAs,plfitKSsAs,plfitKSpAs,edgeNodeRatioDe,norGiantSizeDe,meanClusterSizeDe,plfitKSsDe,plfitKSpDe)))
}