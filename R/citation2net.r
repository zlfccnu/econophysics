#' convert the citation information to co-occurrence network
#' @param database A dataframe with authors, institutions, keywords at each row
#' @return an weighted and named igraph object
#' @export
citation2net=function(database){
  require(igraph)
  ## construct the key words network
  database=as.data.frame(database)
  database %>% unlist() %>% unique() %>% na.omit() %>% as.character()-> databaseList
  databaseNet= graph.empty(directed = FALSE)
  databaseNet=databaseNet + vertices(databaseList)
  
  for(i in 1:dim(database)[1]){
    eachRow=as.character(na.omit(unlist(database[i,])))
    if(length(eachRow)>=2){
      eachRow=combn(eachRow,2)
      databaseNet=add.edges(graph = databaseNet,edges = as.vector(eachRow))
    }
    print(i)
  }
  ## simplify the network, delete duplicate edges etc.
  databaseNet=simplify(databaseNet)
  
  ## add edge weight
  databaseNet=set.edge.attribute(databaseNet,"weight",value=0)
  for(i in 1:dim(database)[1]){
    eachRow=as.character(na.omit(unlist(database[i,])))
    if(length(eachRow)>=2){
      eachRow=combn(eachRow,2)
      for(j in 1:dim(eachRow)[2]){
        E(databaseNet)[eachRow[1,j]%--%eachRow[2,j]]$weight=E(databaseNet)[eachRow[1,j]%--%eachRow[2,j]]$weight+1
      }
    }
    print(i)
  }
  return(databaseNet)
}