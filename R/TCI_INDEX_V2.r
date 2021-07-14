#' Trade complementarity index by Micbael Michaely
#' @param trade_data the polished trade data tibble
#' @param commodity_code the commodity code vector
#' @param Year logical value, whether the trade_data should be splited by year or not
#' @param country_names the name vector of the countries under consideration
#' @return a list of TCI matrices if Year is true, otherwise a single matrix
#' @references Trade Preferential Agreements in Latin America An Ex-Ante Assessment, Micbael Michaely
#' @export
TCI_v2<- function(trade_data, commodity_code, Year=TRUE,country_names=NULL){
  require("conflicted")
  conflict_prefer(name = "select",winner = "dplyr")
  conflict_prefer(name = "filter",winner = "dplyr")
  
  tci_ij<- function(trade_data,i,j,commodity_codes){
      a<- (trade_data %>% filter(reporter==j,partner=="World",commodity_code%in%commodity_codes) %>% pull(trade_value_usd_k)) %>% sum
  if(length(a)==0){
    a<- NA
  }
  x_j<- (trade_data %>% filter(reporter==j,partner=="World",commodity_code%in%commodity_codes) %>% select(commodity_code,trade_value_usd_k)) %>% mutate(x_j = trade_value_usd_k/a) %>% select(commodity_code,x_j)
  x_j_tmp<- tibble(commodity_code=commodity_codes,trade_value_usd_k=0)
  x_j<- full_join(x=x_j,y=x_j_tmp,by="commodity_code") %>% mutate(x_j=ifelse(is.na(x_j),0,x_j)) %>% select(commodity_code,x_j)
  ### m_j
  m_tmp<- trade_data %>% filter(partner==i,commodity_code%in%commodity_codes) %>% pull(trade_value_usd_k) %>% sum
  if(length(m_tmp)==0){
    m_tmp=NA
  }
  m_i<- trade_data %>% filter(partner==i,commodity_code%in%commodity_codes) %>% group_by(partner,commodity_code) %>% summarise(trade_value_usd_k=sum(trade_value_usd_k)) %>% ungroup() %>% mutate(m_i=trade_value_usd_k/m_tmp) %>% select(commodity_code,m_i)
  m_i_tmp<- tibble(commodity_code=commodity_codes,trade_value_usd_k=0)
  m_i<- full_join(x=m_i,y=m_i_tmp,by="commodity_code") %>% mutate(m_i=ifelse(is.na(m_i),0,m_i)) %>% select(commodity_code,m_i)
  
  tci<-  inner_join(x_j,m_i,by="commodity_code") ## we should expand the tibble by the commodity codes
  if(dim(tci)[1]==0){
    tci<- 0
  }else{
    tci<- tci %>% mutate(c_ij = abs(m_i - x_j)/2) %>% summarise(100*(1-sum(c_ij))) %>% pull
  }
  return(tci)
}

## check the variables
## Year==TRUE
if(isTRUE(Year)){
  trade_data<- trade_data %>% group_by(year) %>% group_split()## split the trade data by year
  
  ## construct the TCI matrix list
  TCI_mat_list<- list()
  ## for loop to calculate the ESI
  for(i in 1:length(trade_data)){
    trade_data_tmp<- trade_data[[i]]
    print(paste("the data for ",unique(trade_data_tmp$year)))
    ### setup the country names for the ESI matrix
    if(is.null(country_names)){
      country_names<- sort(unique(trade_data_tmp$reporter))
    }
    TCI_mat<- matrix(data = 0,nrow = length(country_names),ncol = length(country_names))
    rownames(TCI_mat)<- country_names
    colnames(TCI_mat)<- country_names
    ## calculate TCI matrix
    for(j in 1:length(country_names)){
      for(k in 1:length(country_names)){
        print(paste(country_names[j],"_",country_names[k]))
        TCI_mat[j,k]<- tci_ij(trade_data=trade_data_tmp,i = country_names[j],j = country_names[k],commodity_codes = commodity_code)
      }
    }
    TCI_mat_list[[1+length(TCI_mat_list)]]<- TCI_mat
    print(i)
  }
  return(TCI_mat_list)
}else{## Year==FALSE
  ## construct the TCI matrix list
  ## for loop to calculate the ESI
  trade_data_tmp<- trade_data %>% group_by(reporter,partner,commodity_code) %>% summarise(trade_value_usd_k=sum(trade_value_usd_k)) %>% ungroup()
  ### setup the country names for the ESI matrix
  if(is.null(country_names)){
    country_names<- sort(unique(trade_data_tmp$reporter))
  }
  TCI_mat<- matrix(data = 0,nrow = length(country_names),ncol = length(country_names))
  rownames(TCI_mat)<- country_names
  colnames(TCI_mat)<- country_names
  ## calculate ESI matrix
  for(j in 1:length(country_names)){
    for(k in 1:length(country_names)){
      TCI_mat[j,k]<- tci_ij(trade_data  =trade_data_tmp,i = country_names[j],j = country_names[k],commodity_codes = commodity_code)
    }
  }
  return(TCI_mat) 
}

}