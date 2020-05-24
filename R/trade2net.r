#' convert the uncomtrade data tibble to igraph networks
#' @param trade_data a tidyverse like tibble generate by the "polish_data" function
#' @return a tidygraph object
#' @export
trade_data2net=function(trade_data){
  #vars_names=c("year","reporter","partner","netweight_tone","reporter_continent","partner_continent")
  ## check the variables of the trade data tibble
  select<- dplyr::select
  filter<- dplyr::filter
  rename<- dplyr::rename
  bind_rows<- dplyr::bind_rows
  distinct<- dplyr::distinct
  mutate<- dplyr::mutate
  left_join<- dplyr::left_join
  ## reuse codes, will not change
  tmp_net<- list()
  years<- select(trade_data,year) %>% pull() %>% unique()
  for(i in years){
    ## filter edge list data for one year
    edgeList_data<- filter(trade_data,year==i) %>% select(from=reporter,to=partner,weight=netweight_tone)
    ## all the country names
    tmp_country<- edgeList_data %>% select(from,to) %>% unlist() %>% unique()
    ## compile the vertex information such as lat and lon, continent info as a tibble
    reporter_info<- select(trade_data,reporter,reporter_latitude,reporter_longitude,reporter_continent)
    reporter_info<- rename(reporter_info,country=reporter,latitude=reporter_latitude,longitude=reporter_longitude,continent=reporter_continent)
    partner_info<- select(trade_data,partner,partner_latitude,partner_longitude,partner_continent)
    partner_info<- rename(partner_info,country=partner,latitude=partner_latitude,longitude=partner_longitude,continent=partner_continent)
    continent_info<- bind_rows(reporter_info,partner_info)
    continent_info<- distinct(continent_info)
    vertices_info<- filter(continent_info,country%in%tmp_country)
    vertices_info<- rename(vertices_info,name=country, lat=latitude,lon=longitude,continent=continent)
    ## constrcut the network
    tmp_net[[length(tmp_net)+1]]=graph_from_data_frame(d = edgeList_data,directed = TRUE,vertices = vertices_info)
    print(i)
  }
  tmp_net=lapply(tmp_net,igraph::simplify)## remove mutiple edges
  names(tmp_net)<- years
  tmp_net=lapply(tmp_net,tidygraph::as_tbl_graph)
  ## add the year as edge information, edge contians the trade information of each year
  tmp_func=function(graph,year){
    graph<- tidygraph::activate(graph,edges) %>% mutate(year=year)
  }
  for(i in years){
    tmp_net[[as.character(i)]]<- tmp_func(tmp_net[[as.character(i)]],year = i)
  }
  ## add continent to the edge
  tmp_func=function(graph){
    graph<- tidygraph::activate(graph,edges) %>% mutate(from_name=get.edgelist(graph)[,1],to_name=get.edgelist(graph)[,2])
    y=bind_cols(name=V(graph)$name,continent=V(graph)$continent)
    graph<- activate(graph,edges) %>% left_join(x=.,y=y,by =c("from_name"="name")) %>% rename(from_continent=continent) %>% left_join(x=.,y=y,by =c("to_name"="name")) %>% rename(to_continent=continent)
    return(graph)
  }
  tmp_net=lapply(tmp_net,tmp_func)
  return(tmp_net)
}