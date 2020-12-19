#' ranking plot of countries fro the uncomtrade data
#' @param net_list, a list of tidygraph/igraph objects, with year names
#' @param Y, the specific year index for the net_list
#' @param n, the number of countries to plot the ranking plot
#' @param mode, plot the import or export ranking
#' @return a ggplot object
#' @export
ranking_plot<- function(net_list,Y,n=10,mode=c("import","export")){
  require("ggplot2")
  require("signal")
  ranking_nodes=function(GRAPH,YEAR){
    ranking=bind_cols(country=V(GRAPH)$name,pageRank=page_rank(GRAPH)$vector,degree_out=igraph::degree(GRAPH,mode = "out"),degree_in=igraph::degree(GRAPH,mode = "in"),betweenness=betweenness(GRAPH),strength_out=strength(GRAPH,mode = "out"),strength_in=strength(GRAPH,mode="in"),eigen_central=eigen_centrality(GRAPH)$vector,alpha_central=alpha_centrality(GRAPH),authority_score=authority.score(GRAPH)$vector,closeness=closeness(GRAPH),eccentricity=eccentricity(GRAPH),year=rep(YEAR,vcount(GRAPH)))
  }
  
  ## ranking the country
  ## we need to consider the direction of the graph
  years=as.integer(names(net_list))
  rankingCountry=lapply(1:length(net_list),function(x){
    ranking_nodes(net_list[[x]],years[x])
  })
  rankingCountry<- bind_rows(rankingCountry)
  
  top_countries<- switch(mode,
                         "import"={group_by(rankingCountry,year)%>% mutate(countryRank=order(order(strength_in,decreasing = TRUE)))%>%dplyr::filter(year==Y,countryRank<=n) %>% ungroup() %>% dplyr::select(country) %>% pull},
                         "export"={group_by(rankingCountry,year)%>% mutate(countryRank=order(order(strength_out,decreasing = TRUE)))%>%dplyr::filter(year==Y,countryRank<=n) %>% ungroup() %>% dplyr::select(country) %>% pull}
  )
  
  df<- switch(mode,
              "import"={group_by(rankingCountry,year)  %>% mutate(countryRank=rank(-strength_in)) %>% dplyr::filter(year<=Y,country%in%top_countries) %>% dplyr::select(year,country,countryRank,strength_in) %>% dplyr::arrange(countryRank,.by_group=TRUE) %>% ungroup()},
              "export"={group_by(rankingCountry,year)  %>% dplyr::mutate(countryRank=rank(-strength_out)) %>% dplyr::filter(year<=Y,country%in%top_countries) %>% dplyr::select(year,country,countryRank,strength_out) %>% dplyr::arrange(countryRank,.by_group=TRUE) %>% ungroup()}
  )
  
  reverselog_trans <- function(base = exp(1)) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    trans_new(paste0("reverselog-", format(base)), trans, inv, 
              log_breaks(base = base), 
              domain = c(1e-100, Inf))
  }
  
  p<- switch(mode,
             "import"={ggplot(df)+geom_line(data =df %>%group_by(country) %>%do({tibble(year = seq(min(.$year), max(.$year),length.out=10000),countryRank = signal::pchip(.$year, .$countryRank, year))}),aes(x=year,y=countryRank,color=country),size=1,show.legend = FALSE) +scale_y_continuous(trans=reverselog_trans(2))+geom_point(data = df ,aes(year,countryRank,size=strength_in,color=country)) +  labs(title = sprintf("Top %d Import Countries As of %d",n,Y))+scale_size_continuous(guide = FALSE, range = c(1,10))+guides(colour = guide_legend(override.aes = list(size=5)))},
             "export"={ggplot(df)+geom_line(data =df %>%group_by(country) %>%do({tibble(year = seq(min(.$year), max(.$year),length.out=10000),countryRank = signal::pchip(.$year, .$countryRank, year))}),aes(x=year,y=countryRank,color=country),size=1,show.legend = FALSE) +scale_y_continuous(trans=reverselog_trans(2))+geom_point(data = df ,aes(year,countryRank,size=strength_out,color=country)) +  labs(title = sprintf("Top %d Export Countries As of %d",n,Y))+scale_size_continuous(guide = FALSE, range = c(1,10))+guides(colour = guide_legend(override.aes = list(size=5)))}
  )
  return(p)
}
