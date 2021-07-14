#' the trade complementarity index
#' @param trade_data the tibble of trade data
#' @param commodity_code a vector of commodity codes
#' @param Year logical, whether split the trade_data tibble by year or not
#' @param country_names the country names that used to calculate the TCI pairs, if its NULL then will determine automatically
#' @return a TCI matrix list(split the trade data by year) or a single matrix 
#' @references Trade Intensities and the Analysis of Bilateral Trade Flows in a Many-Country World : A Survey,1982
#' @export

TCI<- function(trade_data, commodity_code, Year=TRUE,country_names=NULL){
  require("conflicted")
  conflict_prefer(name = "select",winner = "dplyr")
  conflict_prefer(name = "filter",winner = "dplyr")
  ## the similarity function
  tci_ij<- function(trade_data,i,j,commodity_codes){
    
    ## x_i
    d<- (trade_data %>% filter(reporter==i,partner=="World",commodity_code%in%commodity_codes) %>% pull(trade_value_usd_k)) %>% sum
    if(length(d)==0){
      d<- (trade_data %>% filter(reporter==i,partner!="World") %>% pull(trade_value_usd_k)) %>% sum
    }
    x_i<- (trade_data %>% filter(reporter==i,partner=="World",commodity_code%in%commodity_codes) %>% select(commodity_code,trade_value_usd_k)) %>% mutate(x_i = trade_value_usd_k/d) %>% select(commodity_code,x_i)
    x_i_tmp<- tibble(commodity_code=commodity_codes,trade_value_usd_k=0)
    x_i<- full_join(x=x_i,y=x_i_tmp,by="commodity_code") %>% mutate(x_i=ifelse(is.na(x_i),0,x_i)) %>% select(commodity_code,x_i) %>% mutate(x_i=ifelse(x_i==Inf,0,x_i))
    ### m_w
    a<- ((trade_data %>% filter(reporter=="World",commodity_code%in%commodity_codes) %>% summarise(total=sum(trade_value_usd_k)) %>% pull)- (trade_data %>% filter(partner==i,commodity_code%in%commodity_codes)%>% pull(trade_value_usd_k) %>% sum))
    if(length(a)==0){
      a<- NA
    }
    
    b<- trade_data %>% filter(reporter=="World",commodity_code%in%commodity_codes) %>% group_by(reporter,commodity_code) %>% summarise(trade_value_usd_k=sum(trade_value_usd_k)) %>%ungroup() %>%  select(commodity_code,trade_value_usd_k)
    ## expand by commodity_codes
    b_tmp<- tibble(commodity_code=commodity_codes,trade_value_usd_k=0)
    b<- full_join(x=b,y=b_tmp,by="commodity_code") %>% mutate(trade_value_usd_k=ifelse(is.na(trade_value_usd_k.x),0,trade_value_usd_k.x)) %>% select(commodity_code,trade_value_usd_k)
    
    c<- trade_data %>% filter(partner==i,commodity_code%in%commodity_codes) %>% group_by(partner,commodity_code) %>% summarise(trade_value_usd_k=sum(trade_value_usd_k)) %>% ungroup() %>% select(commodity_code,trade_value_usd_k)
    ## expand by commodity_codes
    c_tmp<- tibble(commodity_code=commodity_codes,trade_value_usd_k=0)
    c<- full_join(x=c,y=c_tmp,by="commodity_code") %>% mutate(trade_value_usd_k=ifelse(is.na(trade_value_usd_k.x),0,trade_value_usd_k.x)) %>% select(commodity_code,trade_value_usd_k)
    
    m_w<- inner_join(b,c,by ="commodity_code") %>% mutate(m_w= a/(trade_value_usd_k.x - trade_value_usd_k.y))%>% mutate(m_w=ifelse(m_w<0,0,m_w)) %>% select(commodity_code,m_w) %>% mutate(m_w=ifelse(m_w==Inf,0,m_w))## be careful that the export of a single country may larger than the global total export due to the data quality. for example China
    
    ### m_j
    m_tmp<- trade_data %>% filter(partner==j,commodity_code%in%commodity_codes) %>% pull(trade_value_usd_k) %>% sum
    if(length(m_tmp)==0){
      m_tmp=NA
    }
    m_j<- trade_data %>% filter(partner==j,commodity_code%in%commodity_codes) %>% group_by(partner,commodity_code) %>% summarise(trade_value_usd_k=sum(trade_value_usd_k)) %>% ungroup() %>% mutate(m_j=trade_value_usd_k/m_tmp) %>% select(commodity_code,m_j) 
    ## expand by commodity_code
    m_j_tmp<- tibble(commodity_code=commodity_codes,trade_value_usd_k=0)
    m_j<- full_join(x=m_j,y=m_j_tmp,by="commodity_code") %>% mutate(m_j=ifelse(is.na(m_j),0,m_j)) %>% select(commodity_code,m_j)%>% mutate(m_j=ifelse(m_j==Inf,0,m_j))
    
   tci<-  inner_join(x_i,m_w,by="commodity_code") %>% inner_join(m_j,by="commodity_code") ### we need to expand the tibble
   if(dim(tci)[1]==0){
     tci<- 0
   }else{
     tci<- tci %>% mutate(c_ij=x_i*m_w*m_j) %>% summarise(sum(c_ij,na.rm = TRUE)) %>% pull
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

