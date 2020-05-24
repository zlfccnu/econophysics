#' Function used to calculate the basic network topology
#' @param GRAPH an igraph object
#' @return a vector with six elements
#' @export
basicGraphTopology=function(GRAPH){
    library(econophysics)
    if(is.directed(GRAPH)&&is.weighted(GRAPH)){
      topology=rep(0,11)
      names(topology)=c("vertex","edge","clustering","shortestPath","heterogeneity","averageInDegree","averageOutDegree","averageInStrength","averageOutStrength","density","assortativity")
      topology["vertex"]=vcount(GRAPH)
      topology["edge"]=ecount(GRAPH)
      topology["clustering"]=transitivity(GRAPH)
      topology["shortestPath"]=average.path.length(GRAPH)
      topology["heterogeneity"]=funcHeteroIndex(GRAPH)
      topology["averageInDegree"]=mean(degree(GRAPH,mode = "in"))
      topology["averageOutDegree"]=mean(degree(GRAPH,mode = "out"))
      topology["averageInStrength"]=mean(strength(GRAPH,mode = "in"))
      topology["averageOutStrength"]=mean(strength(GRAPH,mode = "out"))
      topology["density"]=graph.density(GRAPH)
      topology["assortativity"]=assortativity.degree(GRAPH)
    }else{
      topology=rep(0,8)
      names(topology)=c("vertex","edge","clustering","shortestPath","heterogeneity","averageDegree","density","assortativity")
      topology["vertex"]=vcount(GRAPH)
      topology["edge"]=ecount(GRAPH)
      topology["clustering"]=transitivity(GRAPH)
      topology["shortestPath"]=average.path.length(GRAPH)
      topology["heterogeneity"]=funcHeteroIndex(GRAPH)
      topology["averageDegree"]=mean(degree(GRAPH))
      topology["density"]=graph.density(GRAPH)
      topology["assortativity"]=assortativity.degree(GRAPH)
    }
   topology=t(topology)
   topology=as_tibble(topology)
    return(topology)
}