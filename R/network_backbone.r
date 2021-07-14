#' extract the network backbone based on the null model proposed by PNAS paper:Extracting the multiscale backbone of complex weighted networks
#' @param net a tbl_graph object from tidygraph pkg
#' @param alpha the significant level
#' @param cut_mode parameter valid for the directed network, "or" means the alpha_ij value for 
#' @return a igraph object
#' @export
network_backbone<- function(net,alpha=0.1,cut_mode=c("or","and")){
  require(tidygraph)
  require(igraph)
  prefer_dplyr<- function(x){
    library("conflicted")
    library("dplyr")
    ls("package:dplyr") %>% lapply(conflict_prefer,winner="dplyr") %>% invisible()
  }
  prefer_dplyr()
  if(!is.tbl_graph(net)){
    stop("the net variable must be tbl_graph object")
  }
    if(!is.weighted(net)){
    stop("the network should be a weighted one")
    }
  ### check the edge attributes
  
  if(is.directed(net)){
    ### add the in and out degree and strength
    net<- net %>% activate("nodes") %>% mutate(s_in=centrality_degree(weights = weight,mode = "in"))
    net<- net %>% activate("nodes") %>% mutate(s_out=centrality_degree(weights = weight,mode = "out"))
    net<- net %>% activate("nodes") %>% mutate(k_in=centrality_degree(mode="in"))
    net<- net %>% activate("nodes") %>% mutate(k_out=centrality_degree(mode="out"))
    
    net<- net %>% activate("edges") %>%left_join(y=as_tibble(net %>% activate("nodes")) %>% select(name,s_out),by = c("from_name"="name"))
    net<- net %>% activate("edges") %>%left_join(y=as_tibble(net %>% activate("nodes")) %>% select(name,s_in),by = c("to_name"="name"))
    net<- net %>% activate("edges") %>%left_join(y=as_tibble(net %>% activate("nodes")) %>% select(name,k_out),by = c("to_name"="name"))
    net<- net %>% activate("edges") %>%left_join(y=as_tibble(net %>% activate("nodes")) %>% select(name,k_in),by = c("to_name"="name"))
    ### edge attributes
    net<- net %>% activate("edges") %>% mutate(p_ij_out=weight/s_out)
    net<- net %>% activate("edges") %>% mutate(p_ij_in=weight/s_in)
    func_integral<- function(p,k){
      func_temp<- function(k){
        test<- function(x){
          (1-x)^(k-2)
          }
        return(test)
      }
      alpha<- 1-(k-1)*integrate(f = func_temp(k=k),lower = 0,upper = p,stop.on.error = FALSE)[[1]]
      return(alpha)
    }
    ### a perturbation 0.0000000001 is necessary for the case with k=1 or 0
    net<- net %>% activate("edges") %>% mutate(alpha_ij_out= sapply(X = 1:length(p_ij_out),FUN = function(x){
      return(func_integral(p_ij_out[x]-0.00000000001,k_out[x]))
    }))
    net<-  net %>% activate("edges") %>% mutate(alpha_ij_in= sapply(X = 1:length(p_ij_in),FUN = function(x){
      return(func_integral(p_ij_in[x]-0.00000000001,k_in[x]))
    }))
    
    ### cut the edges
    if(cut_mode=="or"){
      net<- net %>% activate("edges") %>% filter(alpha_ij_out<=alpha|alpha_ij_in<=alpha)
    }
    if(cut_mode=="and"){
      net<- net %>% activate("edges") %>% filter(alpha_ij_out<=alpha,alpha_ij_in<=alpha)
    }
  }
  if(!is.directed(net)){
    ### add the in and out degree and strength
    net<- net %>% activate("nodes") %>% mutate(s=centrality_degree(weights = weight))
    net<- net %>% activate("nodes") %>% mutate(k=centrality_degree())

    net<- net %>% activate("edges") %>%left_join(y=as_tibble(net %>% activate("nodes")) %>% select(name,s),by = c("from_name"="name"))
    net<- net %>% activate("edges") %>%left_join(y=as_tibble(net %>% activate("nodes")) %>% select(name,k),by = c("to_name"="name"))
    ### edge attributes
    net<- net %>% activate("edges") %>% mutate(p_ij=weight/s)
    
    func_integral<- function(p,k){
      func_temp<- function(k){
        test<- function(x){
          (1-x)^(k-2)
        }
        return(test)
      }
      alpha<- 1-(k-1)*integrate(f = func_temp(k=k),lower = 0,upper = p,stop.on.error = FALSE)[[1]]
      return(alpha)
    }
    ### a perturbation 0.0000000001 is necessary for the case with k=1 or 0
    net<- net %>% activate("edges") %>% mutate(alpha_ij= sapply(X = 1:length(p_ij),FUN = function(x){
      return(func_integral(p_ij[x]-0.00000000001,k[x]))
    }))
   
    ### cut the edges
    net<- net %>% activate("edges") %>% filter(alpha_ij<=alpha)
  }
  net<- as.igraph(net) %>% delete_vertices(v=which(degree(net)==0))
  return(net)
}



