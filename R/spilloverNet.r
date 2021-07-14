#' generate the spillover network for a return dataframe
#' @param return_ts a tibble object with multiple return time series for assets, the first column is the date indexes
#' @param VaR_ts a tisbble object with multiple VaR time series for assets, the first column is the date indexes
#' @param M the lag paramter for the risk spillover
#' @param BETA the significant threshold for the causality test
#' @return a tidygraph object, the risk spillover network
#' @export

spilloverNet<- function(return_ts,VaR_ts,M,BETA){
    ### check if the first column is the date variable
    col_types<- return_ts %>% summarise_all(class) %>% as.character()
    if(!"Date"%in%col_types){
      stop("the return ts tibble should include the Date index")
    }
    col_types<- VaR_ts %>% summarise_all(class) %>% as.character()
    if(!"Date"%in%col_types){
      stop("the VaR ts tibble should include the Date index")
    }
    
    tickers=colnames(return_ts)[-1]
    edge_data <- tibble::tibble(from=tickers,to=tickers) %>% tidyr::expand(from=from,to=to)## all possible edges
    edge_data<- edge_data %>% dplyr::filter(from!=to)## remove the duplicate combinations
    edge_data<- edge_data %>% mutate(sp_value= purrr::pmap_dbl(list(from,to),function(from,to,return_ts,VaR_ts,M,BETA){
      sp_value<- spillover(r1=return_ts %>% dplyr::pull(from),r2 = return_ts %>% dplyr::pull(from),VaR1 = VaR_ts %>% dplyr::pull(from),VaR2 = VaR_ts %>% dplyr::pull(to),M=M,BETA = BETA)
      return(sp_value[1])
    },return_ts=return_ts,VaR_ts=VaR_ts,M=M,BETA=BETA))
    edge_data<- edge_data %>% dplyr::filter(sp_value!=0) 
    net<- as_tbl_graph(edge_data)
    return(net)
}
