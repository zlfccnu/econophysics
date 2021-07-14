#' calculate the barycenter of the trade network
#' @param net a tbl_graph object
#' @references https://www.mckinsey.com/featured-insights/urbanization/urban-world-cities-and-the-rise-of-the-consuming-class
#' @return a tibble with import and export latitude and lontitude 
barycenter<- function(net){
  stopifnot(is.tbl_graph(net))
  net<- net %>% as_tbl_graph() %>%activate(nodes) %>% mutate(s_in=strength(net,mode = "in"),s_out=strength(net,mode = "out"),s_all= strength(net,mode="all"))
  net<- net %>% activate(nodes) %>% mutate(lat_in=(lat*s_in)/sum(s_in),lon_in=(lon*s_in)/sum(s_in))
  net<- net %>% activate(nodes) %>% mutate(lat_out=(lat*s_out)/sum(s_out),lon_out=(lon*s_out)/sum(s_out))
  net<- net %>% activate(nodes) %>% mutate(lat_all=(lat*s_all)/sum(s_all),lon_all=(lon*s_all)/sum(s_all))
  return(tibble(year=net %>% activate(edges) %>% pull(year) %>% unique(),lat_in=(as_tibble(net) %>% pull(lat_in) %>% sum(na.rm = TRUE)),lon_in=(as_tibble(net) %>% pull(lon_in) %>% sum(na.rm = TRUE)),lat_out=(as_tibble(net) %>% pull(lat_out) %>% sum(na.rm = TRUE)),lon_out=(as_tibble(net) %>% pull(lon_out) %>% sum(na.rm = TRUE)),lat_all=(as_tibble(net) %>% pull(lat_all) %>% sum(na.rm = TRUE)),lon_all=(as_tibble(net) %>% pull(lon_all) %>% sum(na.rm = TRUE))))
}
