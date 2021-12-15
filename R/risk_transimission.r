#' modeling the risk transmission of the trade network
#' @param net a tidygraph object
#' @param mode two different supply shock scenario
#' @param n_id the initial shocked node id
#' @param e_id the initial shocked edge id
#' @param alpha the threshold above which the infected nodes during the transmission process will change into active state, the default is 0.1, if it is NULL, the alpha value for each country will be scaled to [alpha_lower_limit,1] based on the export value or the degree of each country.
#' @param alpha_lower_limit the lower limit of the alpha value range when alpha=NULL.
#' @param return_net whether return the final network structure or not
#' @param alpha_increase_type export or degree, in the mitigate scenario, export means we will increase the alpha according to the export volume, degree means the alpha parameter will increase according to the node degree
#' @return the active node size for each iteration
#' @export
#' @references 
risk_transmission<- function(net,mode=c("Node_driven","Edge_driven"),n_id=NULL,e_id=NULL,alpha=0.1,alpha_lower_limit=0.1,alpha_increase_type=c("export","degree"),return_net=FALSE){
  require("conflicted")
  conflict_prefer("select","dplyr")
  conflict_prefer("filter","dplyr")
  conflict_prefer("mutate","dplyr")
  conflict_prefer("arrange","dplyr")
  ### graph attributes checking
  if(!"year"%in%names(get.graph.attribute(net))){
    stop("the net must have year attribute")
  }
  temp_fun<- function(net,mode=c("Node_driven","Edge_driven"),n_id=NULL,e_id=NULL,alpha=0.1,alpha_increase_type=c("export","degree")){
    stopifnot(is.tbl_graph(net))
    ### initialize the import reduction state which will determine the avalanche state, 1 is active 0 is not active
    trade_volume<- E(net)$weight %>% sum
    vertex_num<- vcount(net)
    edge_num<- ecount(net)
    net<- net %>% activate(nodes) %>% mutate(node_id=1:vcount(net))
    net<- net %>% activate(edges) %>% mutate(edge_id=1:ecount(net)) %>% mutate(weight_new=weight)
    net<- net %>% activate(nodes) %>% mutate(import= centrality_degree(weights = weight_new,mode = "in"))
    net<- net %>% activate(nodes) %>% mutate(export= centrality_degree(weights = weight_new,mode = "out"))
    net<- net %>% activate(nodes) %>% mutate(out_deg= centrality_degree(weights = NULL,mode = "out"))
    if(!is.null(alpha)){
      net<- net %>% activate(nodes) %>% mutate(alpha=alpha)
    }else{
      if(alpha_increase_type=="export")
      net<- net %>% activate(nodes)%>% mutate(alpha=scales::rescale(export,to=c(alpha_lower_limit,1)))## the alpha is proportional to the export
      if(alpha_increase_type=="degree")
        net<- net %>% activate(nodes)%>% mutate(alpha=scales::rescale(out_deg,to=c(alpha_lower_limit,1)))
    }
    if(mode=="Node_driven"){
      net<- net %>% activate(nodes) %>%mutate(state=ifelse(node_id%in%n_id,1,0))
      net<- net %>% activate(nodes) %>% mutate(iteration=ifelse(node_id%in%n_id,1,0))
      net<- net %>% activate(edges) %>% mutate(state=0)
      net<- net %>% activate(edges) %>% mutate(iteration=0)
      state_flag=length(n_id)
      active_node_size=length(n_id)
      iteration_flag=1
      country=V(net)$name[n_id]
      n_id_total=n_id
      year=get.graph.attribute(net,"year")
      while(state_flag!=0){
        ### change the edge weight from the selected nodes into zero 
        net<- net %>% activate(edges) %>% mutate(weight_new=ifelse(from%in%n_id,0,weight_new))
        net<- net %>% activate(nodes) %>% mutate(import_new=centrality_degree(weights = weight_new,mode = "in"))
        ## update the state for newly affected nodes and edges
        net<- net %>% activate(nodes) %>% mutate(import_reduction = (import-import_new)/import) %>% mutate_at(vars(import_reduction), ~replace(., is.nan(.), 0)) %>% mutate(state=ifelse(import_reduction>=alpha&state==0,1,state)) 
        node_id_new<- net %>% activate(nodes) %>% filter(import_reduction>= alpha&iteration==0) %>% pull(node_id)  ### the newly effected node id
        net<- net %>% activate(edges) %>% mutate(state=ifelse(from%in%n_id,1,state))
        
        ### the edges that have the iteration should be those connected to the effected nodes
        net<- net %>% activate(edges) %>% mutate(iteration=ifelse(from%in%n_id&iteration==0&to%in%node_id_new,iteration_flag,iteration))
        ## update the iteration flag for newly affected nodes and edges
        iteration_flag=iteration_flag+1
        net<- net %>% activate(nodes) %>% mutate(iteration=ifelse(import_reduction>=alpha&iteration==0,iteration_flag,iteration))
 
        ## update the node id state flag to determine the increase of the active nodes & update the source id
        n_id_total<- append(n_id_total,node_id_new)
        n_id<- node_id_new
        state_flag<- (length(node_id_new))
        
        if(state_flag!=0){
          active_node_size<- append(active_node_size,length(n_id_total))
        }
      }
      ### return the network with the risk transmission paths(modify here)
      net<- activate(.data = net,what = nodes)
      net<- filter(net,iteration!=0)%>% mutate(.,node_id=1:vcount(.))### the node_id is different as the original net
      net<- net %>% activate(edges) %>% filter(iteration!=0)
      net<- net %>% activate(nodes) %>% arrange(desc(iteration))
      node_data<- net %>% activate(nodes) %>% as_tibble()
      iteration_seq<- node_data$iteration %>% unique()
      qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
      color_code = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
      color_code<- color_code[iteration_seq]
      color_code<- tibble(iteration=iteration_seq,color=color_code)
      net<- net %>% activate(edges) %>% left_join(x = .,y=color_code,by=c("iteration"))
      net<- net %>% activate(nodes) %>% left_join(x=.,y=color_code,by=c("iteration"))
      if(is.null(alpha)){
        alpha=alpha_lower_limit
        return(list(tibble(year,country,avalanche=max(active_node_size),iteration=length(active_node_size),node_id=n_id_total[1],vertex_num,vol_dec_rate=sum(E(net)$weight)/trade_volume),net))
      }else{
        return(list(tibble(year,country,avalanche=max(active_node_size),iteration=length(active_node_size),alpha=alpha,node_id=n_id_total[1],vertex_num,vol_dec_rate=sum(E(net)$weight)/trade_volume),net))
      }
    }
    
    if(mode=="Edge_driven"){
      n_id<- net %>% activate(edges) %>% filter(edge_id==e_id) %>% pull(to)
      net<- net %>% activate(nodes) %>%mutate(state=ifelse(node_id%in%n_id,1,0))
      net<- net %>% activate(nodes) %>% mutate(iteration=ifelse(node_id%in%n_id,1,0))
      net<- net %>% activate(edges) %>% mutate(state=0)
      net<- net %>% activate(edges) %>% mutate(iteration=0)
      state_flag=length(n_id)
      active_node_size=length(n_id)
      iteration_flag=1
      edge_id=e_id
      n_id_total=n_id
      year=get.graph.attribute(net,"year")
      while(state_flag!=0){
        ### change the edge weight from the selected nodes into zero
        net<- net %>% activate(edges) %>% mutate(weight_new=ifelse(from%in%n_id,0,weight_new)) %>% mutate(iteration=iteration_flag)
        net<- net %>% activate(nodes) %>% mutate(import_new=centrality_degree(weights = weight_new,mode = "in"))
        ## update the state
        net<- net %>% activate(nodes) %>% mutate(import_reduction = (import-import_new)/import) %>% mutate_at(vars(import_reduction), ~replace(., is.nan(.), 0)) %>% mutate(state=ifelse(import_reduction>=alpha,1,state)) 
        net<- net %>% activate(edges) %>% mutate(state=ifelse(from%in%n_id,1,state))
        node_id_new<- net %>% activate(nodes) %>% filter(import_reduction>= alpha&iteration==0) %>% pull(node_id)  ### the newly effected node id
        net<- net %>% activate(edges) %>% mutate(iteration=ifelse(from%in%n_id&iteration==0&to%in%node_id_new,iteration_flag,iteration))
        ## update the iteration flag
        iteration_flag=iteration_flag+1
        net<- net %>% activate(nodes) %>% mutate(iteration=ifelse(import_reduction>=alpha&iteration==0,iteration_flag,iteration))
        
        ## update the node id state flag to determine the increase of the active nodes & update the source id
        n_id_total<- append(n_id_total,node_id_new)
        n_id<- node_id_new
        state_flag<- (length(node_id_new))
        
        if(state_flag!=0){
          active_node_size<- append(active_node_size,length(n_id_total))
        }
      }
      
      country=net %>% activate(edges) %>% select(from,to,from_name,to_name,edge_id) %>% filter(edge_id==e_id) %>% as_tibble()
      ### return the network with the risk transmission paths(modify here)
        net<- activate(.data = net,what = nodes)
        net<- filter(net,iteration!=0)%>% mutate(.,node_id=1:vcount(.))
        net<- net %>% activate(edges) %>% filter(iteration!=0)
        net<- net %>% activate(nodes) %>% arrange(desc(iteration))
        node_data<- net %>% activate(nodes) %>% as_tibble()
        iteration_seq<- node_data$iteration %>% unique()
        qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
        color_code = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
        color_code<- color_code[iteration_seq]
        color_code<- tibble(iteration=iteration_seq,color=color_code)
        net<- net %>% activate(edges) %>% left_join(x = .,y=color_code,by=c("iteration"))
        net<- net %>% activate(nodes) %>% left_join(x=.,y=color_code,by=c("iteration"))
        if(is.null(alpha)){
          alpha=alpha_lower_limit
          return(list(tibble(year,country,avalanche=max(active_node_size),iteration=length(active_node_size),alpha=alpha,edge_num,vol_dec_rate=sum(E(net)$weight)/trade_volume),net))
        }else{
          return(list(tibble(year,country,avalanche=max(active_node_size),iteration=length(active_node_size),alpha=alpha,edge_num,vol_dec_rate=sum(E(net)$weight)/trade_volume),net))
        }
    }
    
  }
  
  if(mode=="Node_driven"&!is.null(n_id)){### modelling the selected vertices
    results<- temp_fun(net = net,mode = mode,n_id = n_id,alpha = alpha,alpha_increase_type = alpha_increase_type)
    if(isTRUE(return_net)){
      return(results)
    }else{
      return(results[[1]])
    }
  }
  
  if(mode=="Node_driven"&is.null(n_id)){### modelling all the vertices
    node_size_list<- list()
    for(i in 1:vcount(net)){
      node_size_list[[1+length(node_size_list)]]<- temp_fun(net = net,mode = mode,n_id = i,alpha=alpha,alpha_increase_type = alpha_increase_type)
    }
    if(isTRUE(return_net)){
      net_list<- node_size_list %>% lapply(function(x){x[[2]]})
      names(net_list)<- V(net)$name
      risk_transmission_info<- node_size_list %>% lapply(function(x){x[[1]]}) %>% bind_rows()%>% arrange(desc(avalanche)) %>% mutate(rank = max(avalanche) - avalanche) %>% mutate(rank = rank(rank,ties.method = "first"))
      return(list(risk_transmission_info=risk_transmission_info,net_list=net_list))
    }else{
      return(node_size_list %>% lapply(function(x){x[[1]]}) %>% bind_rows()%>% arrange(desc(avalanche)) %>% mutate(rank = max(avalanche) - avalanche) %>% mutate(rank = rank(rank,ties.method = "first")))
    }
  }
  
  if(mode=="Edge_driven"&!is.null(e_id)){
    results<- temp_fun(net = net,mode = mode,e_id = e_id,alpha = alpha,alpha_increase_type = alpha_increase_type)
    if(isTRUE(return_net)){
      return(results)
    }else{
      return(results[[1]])
    }
  }
  
  if(mode=="Edge_driven"&is.null(e_id)){
    node_size_list<- list()
    for(i in 1:ecount(net)){
      node_size_list[[1+length(node_size_list)]]<- temp_fun(net = net,mode = mode,e_id = i,alpha=alpha,alpha_increase_type = alpha_increase_type) 
    }
    if(isTRUE(return_net)){
      risk_transmission_info<- node_size_list %>% lapply(function(x){x[[1]]}) %>% bind_rows()%>% arrange(desc(avalanche)) %>% mutate(rank = max(avalanche) - avalanche) %>% mutate(rank = rank(rank,ties.method = "first"))
      net_list=node_size_list %>% lapply(function(x){x[[2]]})
      e_names<- ends(net,E(net)) %>% as_tibble()
      e_names<- paste0(e_names[1] %>% pull,"_",e_names[2] %>% pull)
      names(net_list)<- e_names
      return(list(risk_transmission_info=risk_transmission_info,net_list=net_list))
    }else{
      return(node_size_list %>% lapply(function(x){x[[1]]}) %>% bind_rows()%>% arrange(desc(avalanche)) %>% mutate(rank=max(avalanche)-avalanche) %>% mutate(rank=rank(rank,ties.method = "first")))
    }
  }
  
}