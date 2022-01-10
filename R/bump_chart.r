#' bump chart or ranking chart for the uncomtrade data
#' @param net_list 
#' @param y the ranking end year, for example 2017
#' @param rank_limit the largest ranking number of countries 
#' @param rank_name the variable name use in dataframe d to perform ranking
#' @param trade_flow the ranking is for export or import
#' @return a ggplot object
#' @export

bump_chart = function(net_list,Y,rank_limit=10,rank_range=c(30,1),size_range=c(3,10)){
  require(pacman)
  p_load(ggplot2,patchwork)
  trade_volume = net_list %>% map(.f = function(x){
    y=strength(x,mode="out")
    y=as_tibble(y,rownames = "country")
    y=tibble(y,mode="out")
    z=strength(x,mode = "in")
    z=as_tibble(z,rownames = "country")
    z=tibble(z,mode="in")
    return(bind_rows(y,z))
  }) %>% bind_rows(.id = "year") %>% mutate(year=as.integer(year)) %>%group_by(year,mode)
  
  top_trade = net_list %>% map(.f = function(x){
    y=strength(x,mode="out")
    y=as_tibble(y,rownames = "country")
    y=tibble(y,mode="out")
    z=strength(x,mode = "in")
    z=as_tibble(z,rownames = "country")
    z=tibble(z,mode="in")
    return(bind_rows(y,z))
  }) %>% bind_rows(.id = "year") %>%group_by(year,mode) %>% mutate(year=as.integer(year)) %>% top_n(n = rank_limit,wt = value) %>% arrange(desc(value),.by_group = TRUE)
  top_exporter = top_trade %>% filter(mode=="out",year==Y) %>% dplyr::select(country) %>% pull
  top_importer = top_trade %>% filter(mode=="in",year==Y) %>% dplyr::select(country) %>% pull
  
  df_out<- trade_volume %>% group_by(year,mode) %>% arrange(desc(value),.by_group = TRUE) %>% mutate(countryRank=row_number()) %>% dplyr::filter(mode=="out",country%in%top_exporter)%>% ungroup() %>% dplyr::select(year,country,countryRank,value) 
  df_in<- trade_volume %>% group_by(year,mode) %>% arrange(desc(value),.by_group = TRUE) %>% mutate(countryRank=row_number()) %>% dplyr::filter(mode=="in",country%in%top_importer)%>% ungroup() %>% dplyr::select(year,country,countryRank,value) 
  ### the export plot
 p_out<- ggplot(df_out)+
    geom_line(data = df_out %>%
                group_by(country) %>%
                do({tibble(year = seq(min(.$year), max(.$year),length.out=10000),countryRank = signal::pchip(.$year, .$countryRank, year))}),aes(x=year,y=countryRank,color=country),size=1,show.legend = FALSE) +scale_y_reverse(lim=rank_range)+geom_point(data = df_out,aes(year,countryRank,size=value,color=country))+ guides(colour = guide_legend(override.aes = list(size=7 )))+scale_color_discrete(breaks=top_exporter)+scale_size_continuous(guide = "none", range = size_range) +  labs(title = sprintf("(A) Top %d %s Countries As of %d",rank_limit,"Export",Y))+theme(text = element_text(family = "Times"))
 
 p_in<-  ggplot(df_in)+
   geom_line(data = df_in %>%
               group_by(country) %>%
               do({tibble(year = seq(min(.$year), max(.$year),length.out=10000),countryRank = signal::pchip(.$year, .$countryRank, year))}),aes(x=year,y=countryRank,color=country),size=1,show.legend = FALSE) +scale_y_reverse(lim=rank_range)+geom_point(data = df_in,aes(year,countryRank,size=value,color=country))+ guides(colour = guide_legend(override.aes = list(size=7 )))+scale_color_discrete(breaks=top_importer)+scale_size_continuous(guide = "none", range = size_range) +  labs(title = sprintf("(B) Top %d %s Countries As of %d",rank_limit,"Import",Y))+theme(text = element_text(family = "Times"))
 return(list(p_out,p_in))
}