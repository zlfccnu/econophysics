#' the RCA and Normalized RCA 
#' @param trade_data the trade dataset
#' @param commodity_code a vector of commodity codes SITC V3
#' @param Year whether split the trade_data set by year or aggregate the data
#' @param country_names the countries that be used to calculate the RCA index
#' @return a named vector of RCA and NRCA
#' @export
SRCA<- function(trade_data, commodity_code, Year=TRUE,country_names=NULL){
  require("conflicted")
  conflict_prefer(name = "select",winner = "dplyr")
  conflict_prefer(name = "filter",winner = "dplyr")
  ## the similarity function
  RCA<- function(trade_data,country,commodity_codes){
    
    ## x_i_k
    x_i<- (trade_data %>% filter(reporter==country,partner=="World",commodity_code=="TOTAL") %>% pull(trade_value_usd_k))
    if(length(x_i)==0){
      x_i<- trade_data %>% filter(reporter==country,partner!="World",commodity_code!="TOTAL") %>% pull(trade_value_usd_k) %>% sum()
    }
    x_i_k<- trade_data %>% filter(reporter==country,partner!="World",commodity_code%in%commodity_codes) %>% pull(trade_value_usd_k) %>% sum()
    
    x_k<- trade_data %>% filter(partner=="World",commodity_code%in%commodity_codes) %>% pull(trade_value_usd_k) %>% sum()
    x<- trade_data %>% filter(partner=="World",commodity_code=="TOTAL") %>% pull(trade_value_usd_k) %>% sum()
    rca<- (x_i_k/x_i)/(x_k/x)
    nrca<- (rca-1)/(rca+1)
    return(tibble(country=country,rca=rca,nrca=nrca))
  }
  
  ## check the variables
  ## Year==TRUE
  if(isTRUE(Year)){
    trade_data<- trade_data %>% group_by(year) %>% group_split()## split the trade data by year
    ## construct the rca list
    rca_list<- list()
    ## for loop to calculate the ESI
    for(i in 1:length(trade_data)){
      trade_data_tmp<- trade_data[[i]]
      print(paste("the data for ",unique(trade_data_tmp$year)))
      ### setup the country names for the rca
      if(is.null(country_names)){
        country_names<- sort(unique(trade_data_tmp$reporter))
      }
      ## calculate rca 
      rca_list_tmp=list()
      for(j in 1:length(country_names)){
        rca_list_tmp[[1+length(rca_list_tmp)]]<- RCA(trade_data = trade_data_tmp,country = country_names[j],commodity_codes = commodity_code)
      }
      rca_list[[1+length(rca_list)]]<- bind_rows(rca_list_tmp)
    }
      
    return(rca_list)
  }else{## Year==FALSE
    ## construct the TCI matrix list
    ## for loop to calculate the ESI
    trade_data_tmp<- trade_data %>% group_by(reporter,partner,commodity_code) %>% summarise(trade_value_usd_k=sum(trade_value_usd_k)) %>% ungroup()
    ### setup the country names for the ESI matrix
    if(is.null(country_names)){
      country_names<- sort(unique(trade_data_tmp$reporter))
    }
    rca_list=list()
    for(j in 1:length(country_names)){
      rca_list[[1+length(rca_list)]]<- RCA(trade_data = trade_data_tmp,country = country_names[j],commodity_codes = commodity_code)
    }
    rca_list<- bind_rows(rca_list)
    return(rca_list)
  }
}