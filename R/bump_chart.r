#' bump chart or ranking chart for the uncomtrade data
#' @param d the ranking dataframe provide by the node_ranking() function
#' @param y the ranking end year, for example 2017
#' @param rank_limit the largest ranking number of countries 
#' @param rank_name the variable name use in dataframe d to perform ranking
#' @param trade_flow the ranking is for export or import
#' @return a ggplot object
#' @export

bump_chart = function(d,y,rank_limit=10,rank_name,trade_flow=c("Export","Import")){
  var=sym(rank_name)
  df<- group_by(d,year)%>% mutate(countryRank=order(order(!!var,decreasing = TRUE)))%>%dplyr::filter(year==y,countryRank<=rank_limit) %>% select(year,country,countryRank,!!var)
  df<- group_by(d,year)  %>% mutate(countryRank=order(order(!!var,decreasing = TRUE))) %>% dplyr::filter(country%in%unique(df$country)) %>% select(year,country,countryRank,!!var)
  df<- ungroup(df)
  x<- df %>% dplyr::filter(year==y) %>% arrange(desc(!!var)) %>% select(country)
  x<- x$country
 q<- ggplot(df)+
    geom_line(data = df %>%
                group_by(country) %>%
                do({
                  tibble(year = seq(min(.$year), max(.$year),length.out=10000),
                         countryRank = signal::pchip(.$year, .$countryRank, year))
                }),aes(x=year,y=countryRank,color=country),size=1,show.legend = FALSE) +scale_y_reverse(lim=c(20,1))+geom_point(data = df,aes(year,countryRank,size=!!var,color=country))+ guides(colour = guide_legend(override.aes = list(size=7 )))+scale_color_discrete(breaks=x)+scale_size_continuous(guide = FALSE, range = c(3,10)) +  labs(title = sprintf("Top %d %s Countries As of %d",rank_limit,trade_flow,y))
 return(q)
}