#' the export similarity index for the trade data, only calculate the 
ESI_ab<- function(trade_data_tmp,a,b,commodity_codes){
  trade_data_tmp<- trade_data_tmp %>% filter(reporter==a|reporter==b,commodity_code%in%commodity_codes)
  trade_data_tmp %>% group_by(reporter,commodity_code) %>% summarise(trade_value_usd_k=sum(trade_value_usd_k)) %>% ungroup()-> trade_data_tmp### sum all the partner countries, if only one partner in the trade data, no effect
  ## extend the trade data
  trade_data_tmp<- expand_grid(reporter = trade_data_tmp$reporter %>% unique(),commodity_code=commodity_codes) %>% left_join(x=.,y=trade_data_tmp,by=c("reporter","commodity_code")) %>% replace_na(replace = list(trade_value_usd_k=0)) 
  trade_data_tmp %>% group_by(reporter) %>% mutate(trade_value_usd_k=trade_value_usd_k/sum(trade_value_usd_k)) %>% replace_na(replace = list(trade_value_usd_k = 0)) %>% group_by(commodity_code) %>% summarise(trade_value_usd_k=min(trade_value_usd_k)) %>% summarise(sum(trade_value_usd_k)) %>% pull()-> s
  s<- s*100
  return(s)
}