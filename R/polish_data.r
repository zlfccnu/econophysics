#' polish data from the uncomtrade database
#' @param data a tidyverse like tibble
#' @param merge merge HK and Maco to mainland China or not
#' @param metric_var whether use the weight(by KG) of the cargo or the value(by USD)
#' @param weight_method the evaluation method to polish the metric variable
#' @param continent_info the supplementary data for the continent information
#' @param keep_world whether keep the data for world total
#' @return a tidyverse like tibble
#' @export
polish_data=function(data,merge=FALSE,metric_var=c("weight","value"),weight_method=c("mean","max","min"),continent_info=continent,keep_world=FALSE){
  ## check the variable names existence
  if(metric_var=="weight"){
    var_names<- c("year","commodity_code","reporter","partner","trade_flow","netweight_kg","type","reporter_iso","partner_iso","reporter_code","partner_code","trade_flow_code","commodity")
  }else{
    var_names<- c("year","commodity_code","reporter","partner","trade_flow","trade_value_usd","type","reporter_iso","partner_iso","reporter_code","partner_code","trade_flow_code","commodity")
  }
  if(!(var_names%in% names(data) %>% prod())){
    stop("variables not match!")
  }
  
  nes_names=c("Africa CAMEU region, nes","Areas, nes","CACM, nes","Caribbean, nes","Eastern Europe, nes","Europe EFTA, nes","Europe EU, nes","LAIA, nes","North America and Central America, nes","Northern Africa, nes","Oceania, nes","Other Africa, nes","Other Asia, nes","Other Europe, nes","Rest of America, nes","Western Asia, nes")
  
  if(isFALSE(keep_world)){
    data<- filter(data,partner!="World",reporter!="World")
  }
  
  data<- filter(data,reporter!="EU-28",partner!="EU-28",!partner%in%nes_names,!reporter%in%nes_names) %>% filter(reporter!="So. African Customs Union",partner!="So. African Customs Union") %>% filter(reporter!="Antarctica",partner!="South Georgia and the South Sandwich Islands",partner!="Antarctica",reporter!="South Georgia and the South Sandwich Islands")
  
  data=mutate(data,reporter=replace(reporter,which(reporter=="Belgium-Luxembourg"),"Belgium"),partner=replace(partner,which(partner=="Belgium-Luxembourg"),"Belgium"))
  ## replace Fmr Germany to Germany
  data=mutate(data,reporter=replace(reporter,which(reporter=="Fmr Fed. Rep. of Germany"),"Germany"),partner=replace(partner,which(partner=="Fmr Fed. Rep. of Germany"),"Germany"),reporter_iso=replace(reporter_iso,which(reporter_iso=="DDR"),"DEU"),partner_iso=replace(partner_iso,which(partner_iso=="DDR"),"DEU"))
  data=mutate(data,reporter=replace(reporter,which(reporter=="Fmr Dem. Rep. of Germany"),"Germany"),partner=replace(partner,which(partner=="Fmr Dem. Rep. of Germany"),"Germany"),reporter_iso=replace(reporter_iso,which(reporter_iso=="DDR"),"DEU"),partner_iso=replace(partner_iso,which(partner_iso=="DDR"),"DEU"))
  ## replace Fmr USSR with Russian Federation
  data=mutate(data,reporter=replace(reporter,which(reporter=="Fmr USSR"),"Russian Federation"),partner=replace(partner,which(partner=="Fmr USSR"),"Russian Federation"),reporter_iso=replace(reporter_iso,which(reporter_iso=="SUN"),"RUS"),partner_iso=replace(partner_iso,which(partner_iso=="SUN"),"RUS"))
  ## replace Fmr Yugoslavia with Serbia
  data=mutate(data,reporter=replace(reporter,which(reporter=="Fmr Yugoslavia"),"Serbia"),partner=replace(partner,which(partner=="Fmr Yugoslavia"),"Serbia"),reporter_iso=replace(reporter_iso,which(reporter_iso=="YUG"),"SRB"),partner_iso=replace(partner_iso,which(partner_iso=="YUG"),"SRB"))
  ## replace Fmr Sudan to Sudan
  data=mutate(data,reporter=replace(reporter,which(reporter=="Fmr Sudan"),"Sudan"),partner=replace(partner,which(partner=="Fmr Sudan"),"Sudan"))##remove the total trade and to organization such as EU quantity,Areas, NES
  ## replace Fmr Vietnam
  data=mutate(data,reporter=replace(reporter,which(reporter=="Fmr Dem. Rep. of Vietnam"),"Viet Nam"),partner=replace(partner,which(partner=="Fmr Dem. Rep. of Vietnam"),"Viet Nam"),reporter_iso=replace(reporter_iso,which(reporter_iso=="VDR
"),"VNM"),partner_iso=replace(partner_iso,which(partner_iso=="VDR"),"VNM"))
  data=mutate(data,reporter=replace(reporter,which(reporter=="Fmr Rep. of Vietnam"),"Viet Nam"),partner=replace(partner,which(partner=="Fmr Rep. of Vietnam"),"Viet Nam"))
  ## replace US related locations,"US Misc. Pacific Isds"
  data=mutate(data,reporter=replace(reporter,which(reporter=="US Misc. Pacific Isds"),"USA"),partner=replace(partner,which(partner=="US Misc. Pacific Isds"),"USA"),reporter_iso=replace(reporter_iso,which(reporter=="US Misc. Pacific Isds
"),"USA"),partner_iso=replace(partner_iso,which(partner=="US Misc. Pacific Isds"),"USA"))
  
  if(isTRUE(merge)){
    data<- mutate(data,reporter=ifelse(trade_flow=="Re-Export"&reporter=="China, Hong Kong SAR"&partner=="China","China",reporter)) %>%  mutate(reporter=ifelse(trade_flow=="Re-Export"&reporter=="China, Macao SAR"&partner=="China","China",reporter))
    data<- data %>% mutate(partner=ifelse(trade_flow=="Re-Export"&partner=="China, Hong Kong SAR","China",partner)) %>% mutate(partner=ifelse(trade_flow=="Re-Export"&partner=="China, Macao SAR","China",partner))##
    data<- mutate(data,reporter=ifelse(trade_flow=="Import"&reporter=="China, Hong Kong SAR","China",reporter)) %>%  mutate(reporter=ifelse(trade_flow=="Import"&reporter=="China, Macao SAR","China",reporter))
    data<- mutate(data,partner=ifelse(trade_flow=="Export"&partner=="China, Hong Kong SAR","China",partner)) %>% mutate(partner=ifelse(trade_flow=="Export"&partner=="China, Macao SAR","China",partner))
    data<- data %>% filter(reporter!="China"|partner!="China")
  } 
  data<- data %>% mutate(trade_flow_combine=replace(trade_flow,trade_flow=="Re-Export","Export")) %>% filter(trade_flow!="Re-Import")
  
  
  ### choose the available variables
  if(metric_var=="weight"){
    data<- data %>% filter(!is.na(reporter_iso),!is.na(partner_iso)) %>% drop_na(netweight_kg) %>% filter(!near(netweight_kg,0)) %>%mutate(netweight_ton=netweight_kg/1000) %>% group_by(year,trade_flow_combine,reporter,partner,commodity,commodity_code) %>% summarise(netweight_ton=sum(netweight_ton)) %>% ungroup()
    
    data<- switch(weight_method,
                  mean={ data %>% mutate(new_reporter=ifelse(trade_flow_combine=="Import",partner,reporter),new_partner=ifelse(trade_flow_combine=="Import",reporter,partner),reporter=new_reporter,partner=new_partner) %>% dplyr::select(-new_reporter,-new_partner)%>% group_by(year,reporter,partner,commodity,commodity_code) %>% summarise(netweight_ton=mean(netweight_ton)) %>% ungroup()},
                  max={ data %>% mutate(new_reporter=ifelse(trade_flow_combine=="Import",partner,reporter),new_partner=ifelse(trade_flow_combine=="Import",reporter,partner),reporter=new_reporter,partner=new_partner) %>% dplyr::select(-new_reporter,-new_partner)%>% group_by(year,reporter,partner,commodity,commodity_code) %>% summarise(netweight_ton=max(netweight_ton)) %>% ungroup()},
                  min={ data %>% mutate(new_reporter=ifelse(trade_flow_combine=="Import",partner,reporter),new_partner=ifelse(trade_flow_combine=="Import",reporter,partner),reporter=new_reporter,partner=new_partner) %>% dplyr::select(-new_reporter,-new_partner)%>% group_by(year,reporter,partner,commodity,commodity_code) %>% summarise(netweight_ton=min(netweight_ton)) %>% ungroup()}
    )
    
  }else{
    data<- data %>% filter(!is.na(reporter_iso),!is.na(partner_iso)) %>% drop_na(trade_value_usd) %>% filter(!near(trade_value_usd,0)) %>%mutate(trade_value_usd_k=trade_value_usd/1000) %>% group_by(year,trade_flow_combine,reporter,partner,commodity,commodity_code) %>% summarise(trade_value_usd_k=sum(trade_value_usd_k)) %>% ungroup()
    
    data<- switch(weight_method,
                  mean={ data %>% mutate(new_reporter=ifelse(trade_flow_combine=="Import",partner,reporter),new_partner=ifelse(trade_flow_combine=="Import",reporter,partner),reporter=new_reporter,partner=new_partner) %>% dplyr::select(-new_reporter,-new_partner)%>% group_by(year,reporter,partner,commodity,commodity_code) %>% summarise(trade_value_usd_k=mean(trade_value_usd_k)) %>% ungroup()},
                  max={ data %>% mutate(new_reporter=ifelse(trade_flow_combine=="Import",partner,reporter),new_partner=ifelse(trade_flow_combine=="Import",reporter,partner),reporter=new_reporter,partner=new_partner) %>% dplyr::select(-new_reporter,-new_partner)%>% group_by(year,reporter,partner,commodity,commodity_code) %>% summarise(trade_value_usd_k=max(trade_value_usd_k)) %>% ungroup()},
                  min={ data %>% mutate(new_reporter=ifelse(trade_flow_combine=="Import",partner,reporter),new_partner=ifelse(trade_flow_combine=="Import",reporter,partner),reporter=new_reporter,partner=new_partner) %>% dplyr::select(-new_reporter,-new_partner)%>% group_by(year,reporter,partner,commodity,commodity_code) %>% summarise(trade_value_usd_k=min(trade_value_usd_k)) %>% ungroup()}
    )
    
  }
  
  ## add the continent information
  ## add the continent info for the reporter
  continent<- continent_info
  data<- left_join(data,continent %>%rename(reporter_latitude=latitude,reporter_longitude=longitude,reporter_continent = continent),by=c("reporter"="country"))
  ## add the continent info for the partner
  data<- left_join(data,continent %>%rename(partner_latitude=latitude,partner_longitude=longitude,partner_continent = continent),by=c("partner"="country"))
  return(data)
}