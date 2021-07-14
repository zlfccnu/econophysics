#' the export similarity index for the trade data and will return a ESI matrix if you have many commodities
#' @param trade_data the tibble covnerted by the polish_data function
#' @param commodity_codes the codes of the commodities based on which the ESI is calculated
#' @param Year split the trade_data by year variable or not, if true the final output will be a list of ESI matrices, otherwise just a single matrix
#' @param partner_average if true, it will combine all the partners(importers here) together to calculate the trade value or volume, otherwise the final ESI index will be the average of the ESI for each importer
#' @references  A Measure of `Export Similarity' and Its Possible Uses, Author(s): J. M. Finger and M. E. Kreinin, Source: The Economic Journal, Vol. 89, No. 356 (Dec., 1979), pp. 905-912
#' @return a list of ESI matrices if the Year parameter is true, otherwise a single matrix
#' @export

ESI<- function(trade_data,commodity_codes,Year=TRUE,partner_average=TRUE,country_names=NULL){
  require("conflicted")
  conflict_prefer(name = "select",winner = "dplyr")
  conflict_prefer(name = "filter",winner = "dplyr")
  ## the similarity function
  s_ab<- function(trade_data_tmp,a,b,commodity_codes){
    trade_data_tmp<- trade_data_tmp %>% filter(reporter==a|reporter==b,commodity_code%in%commodity_codes)
    trade_data_tmp %>% group_by(reporter,commodity_code) %>% summarise(trade_value_usd_k=sum(trade_value_usd_k)) %>% ungroup()-> trade_data_tmp### sum all the partner countries, if only one partner in the trade data, no effect
    ## extend the trade data
    trade_data_tmp<- expand_grid(reporter = trade_data_tmp$reporter %>% unique(),commodity_code=commodity_codes) %>% left_join(x=.,y=trade_data_tmp,by=c("reporter","commodity_code")) %>% replace_na(replace = list(trade_value_usd_k=0)) 
    trade_data_tmp %>% group_by(reporter) %>% mutate(trade_value_usd_k=trade_value_usd_k/sum(trade_value_usd_k)) %>% replace_na(replace = list(trade_value_usd_k = 0)) %>% group_by(commodity_code) %>% summarise(trade_value_usd_k=min(trade_value_usd_k)) %>% summarise(sum(trade_value_usd_k)) %>% pull()-> s
    s<- s*100
    return(s)
  }
  ## check the variables
  ## Year==TRUE
  if(isTRUE(Year)){
    trade_data<- trade_data %>% filter(commodity_code%in%commodity_codes)
    trade_data<- trade_data %>% group_by(year) %>% group_split()## split the trade data by year
    
    ## construct the ESI matrix list
    ESI_mat_list<- list()
    ## for loop to calculate the ESI
    for(i in 1:length(trade_data)){
      trade_data_tmp<- trade_data[[i]]
      
      ### sum all the partners, convert partners to global market partner_average==TRUE
      if(isFALSE(partner_average)){
        trade_data_tmp<- trade_data_tmp %>% group_by(reporter,commodity_code) %>% summarise(trade_value_usd_k=sum(trade_value_usd_k))## sum the trade value for different partners
        
        ### setup the country names for the ESI matrix
        if(is.null(country_names)){
          country_names<- sort(unique(trade_data_tmp$reporter))
        }
        ESI_mat<- matrix(data = 0,nrow = length(country_names),ncol = length(country_names))
        rownames(ESI_mat)<- country_names
        colnames(ESI_mat)<- country_names
        ## calculate ESI matrix
        for(j in 1:(length(country_names)-1)){
          for(k in (j+1):length(country_names)){
            ESI_mat[j,k]<- s_ab(trade_data_tmp,a = country_names[j],b=country_names[k],commodity_codes = commodity_codes)-> ESI_mat[k,j]
          }
        }
      }else{
        ### setup the country names for the ESI matrix
        if(is.null(country_names)){
          country_names<- sort(unique(trade_data_tmp$reporter))
        }
        ### partner country average
        ESI_mat<- matrix(data = 0,nrow = length(country_names),ncol = length(country_names))
        rownames(ESI_mat)<- country_names
        colnames(ESI_mat)<- country_names
        ## calculate ESI matrix
        for(j in 1:(length(country_names)-1)){
          for(k in (j+1):length(country_names)){
            trade_data_tmp_list<- trade_data_tmp %>% filter(reporter==country_names[j]|reporter==country_names[k])%>% group_by(partner) %>% group_split()
            ESI_mat[j,k]<- ESI_mat[k,j]<-  mean(sapply(trade_data_tmp_list,FUN = s_ab,a = country_names[j],b=country_names[k],commodity_codes = commodity_codes))
          }
        }
        
      }
      
      ESI_mat_list[[1+length(ESI_mat_list)]]<- ESI_mat
    }
    return(ESI_mat_list)
}else{## Year==FALSE
    trade_data<- trade_data %>% filter(commodity_code%in%commodity_codes)
    ## construct the ESI matrix list
    ## for loop to calculate the ESI
     trade_data_tmp<- trade_data
      
      ### sum all the partners, convert partners to global market if partner_average==FALSE
      if(isFALSE(partner_average)){
        trade_data_tmp<- trade_data_tmp %>% group_by(reporter,commodity_code) %>% summarise(trade_value_usd_k=sum(trade_value_usd_k))## sum the trade value for different partners
        ### setup the country names for the ESI matrix
        if(is.null(country_names)){
          country_names<- sort(unique(trade_data_tmp$reporter))
        }
        ESI_mat<- matrix(data = 0,nrow = length(country_names),ncol = length(country_names))
        rownames(ESI_mat)<- country_names
        colnames(ESI_mat)<- country_names
        ## calculate ESI matrix
        for(j in 1:(length(country_names)-1)){
          for(k in (j+1):length(country_names)){
            ESI_mat[j,k]<- s_ab(trade_data_tmp,a = country_names[j],b=country_names[k],commodity_codes = commodity_codes)-> ESI_mat[k,j]
          }
        }
       }else{
        ### setup the country names for the ESI matrix
        if(is.null(country_names)){
          country_names<- sort(unique(trade_data_tmp$reporter))
        }
        ### partner country average
        ESI_mat<- matrix(data = 0,nrow = length(country_names),ncol = length(country_names))
        rownames(ESI_mat)<- country_names
        colnames(ESI_mat)<- country_names
        ## calculate ESI matrix
        for(j in 1:(length(country_names)-1)){
          for(k in (j+1):length(country_names)){
            trade_data_tmp_list<- trade_data_tmp %>% filter(reporter==country_names[j]|reporter==country_names[k])%>% group_by(partner) %>% group_split()
            ESI_mat[j,k]<- ESI_mat[k,j]<-  mean(sapply(trade_data_tmp_list,FUN = s_ab,a = country_names[j],b=country_names[k],commodity_codes = commodity_codes))
          }
        }
       }
     return(ESI_mat) 
    }
}