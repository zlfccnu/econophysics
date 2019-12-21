#' polish data from the uncomtrade database
#' @param data a tidyverse like tibble
#' @param merge merge HK and Maco to mainland China or not
#' @return a tidyverse like tibble
#' @export
polish_data=function(data,merge=FALSE){
  filter<- dplyr::filter
  nes_names=c("Africa CAMEU region, nes","Areas, nes","CACM, nes","Caribbean, nes","Eastern Europe, nes","Europe EFTA, nes","Europe EU, nes","LAIA, nes","North America and Central America, nes","Northern Africa, nes","Oceania, nes","Other Africa, nes","Other Asia, nes","Other Europe, nes","Rest of America, nes","Western Asia, nes")
  data<- filter(data,partner!="World",reporter!="World",reporter!="EU-28",partner!="EU-28",!partner%in%nes_names,!reporter%in%nes_names) %>% filter(reporter!="So. African Customs Union",partner!="So. African Customs Union") %>% filter(reporter!="Antarctica",partner!="South Georgia and the South Sandwich Islands",partner!="Antarctica",reporter!="South Georgia and the South Sandwich Islands")
  ## 将Belgium-Luxembourg替换为Belgium
  data=mutate(data,reporter=replace(reporter,which(reporter=="Belgium-Luxembourg"),"Belgium"),partner=replace(partner,which(partner=="Belgium-Luxembourg"),"Belgium"))
  ## replace Fmr Germany to Germany
  data=mutate(data,reporter=replace(reporter,which(reporter=="Fmr Fed. Rep. of Germany"),"Germany"),partner=replace(partner,which(partner=="Fmr Fed. Rep. of Germany"),"Germany"),reporter_iso=replace(reporter_iso,which(reporter_iso=="DDR"),"DEU"),partner_iso=replace(partner_iso,which(partner_iso=="DDR"),"DEU"))
  data=mutate(data,reporter=replace(reporter,which(reporter=="Fmr Dem. Rep. of Germany"),"Germany"),partner=replace(partner,which(partner=="Fmr Dem. Rep. of Germany"),"Germany"),reporter_iso=replace(reporter_iso,which(reporter_iso=="DDR"),"DEU"),partner_iso=replace(partner_iso,which(partner_iso=="DDR"),"DEU"))
  ## replace Fmr USSR with Russian Federation
  data=mutate(data,reporter=replace(reporter,which(reporter=="Fmr USSR"),"Russian Federation"),partner=replace(partner,which(partner=="Fmr USSR"),"Russian Federation"),reporter_iso=replace(reporter_iso,which(reporter_iso=="SUN"),"RUS"),partner_iso=replace(partner_iso,which(partner_iso=="SUN"),"RUS"))
  ## replace Fmr Yugoslavia with Serbia
  data=mutate(data,reporter=replace(reporter,which(reporter=="Fmr Yugoslavia"),"Serbia"),partner=replace(partner,which(partner=="Fmr Yugoslavia"),"Serbia"),reporter_iso=replace(reporter_iso,which(reporter_iso=="YUG"),"SRB"),partner_iso=replace(partner_iso,which(partner_iso=="YUG"),"SRB"))
  ## replace Fmr Sudan to Sudan
  data=mutate(data,reporter=replace(reporter,which(reporter=="Fmr Sudan"),"Sudan"),partner=replace(partner,which(partner=="Fmr Sudan"),"Sudan"))##remove the total trade and to orignazation such as EU quantity,Areas, NES,未知地区
  if(isTRUE(merge)){## merge the HK and Macao with China,当交易流向为Re-Export的时候才将HK和Macao改成China，同时将两个地方的import指向中国大陆,因为HK本身自己生产的垃圾也会export到中国，或者其他国家，这种类型的不变
    data<- mutate(data,reporter=ifelse(trade_flow=="Re-Export"&reporter=="China, Hong Kong SAR"&partner=="China","China",reporter)) %>%  mutate(reporter=ifelse(trade_flow=="Re-Export"&reporter=="China, Macao SAR"&partner=="China","China",reporter))## 将HK reexport中国的改成China
    data<- data %>% mutate(partner=ifelse(trade_flow=="Re-Export"&partner=="China, Hong Kong SAR","China",partner)) %>% mutate(partner=ifelse(trade_flow=="Re-Export"&partner=="China, Macao SAR","China",partner))##将其他国家re-export 到HK的改成China
    data<- mutate(data,reporter=ifelse(trade_flow=="Import"&reporter=="China, Hong Kong SAR","China",reporter)) %>%  mutate(reporter=ifelse(trade_flow=="Import"&reporter=="China, Macao SAR","China",reporter))
    data<- mutate(data,partner=ifelse(trade_flow=="Export"&partner=="China, Hong Kong SAR","China",partner)) %>% mutate(partner=ifelse(trade_flow=="Export"&partner=="China, Macao SAR","China",partner))
    data<- data %>% filter(reporter!="China"|partner!="China")## 在上一步修改HK和Macao之后，将China 到 China的去掉
  }
  ## 然后将其他的Re-Export改成Export，并且将Re-Import去掉
  data<- data %>% mutate(trade_flow_combine=replace(trade_flow,trade_flow=="Re-Export","Export")) %>% filter(trade_flow!="Re-Import")
  data<- data %>% filter(!is.na(reporter_iso),!is.na(partner_iso)) %>% drop_na(netweight_kg) %>% filter(!near(netweight_kg,0)) %>%mutate(netweight_tone=netweight_kg/1000) %>% group_by(year,trade_flow_combine,reporter,partner) %>% summarise(netweight_tone=sum(netweight_tone)) %>% ungroup()
  return(data)
}
