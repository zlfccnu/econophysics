#' generate the spillover network for a return dataframe
#' @param 
#' @param 
#' @return 
#' @export

spilloverNet<- function(dat,parallel=TRUE, n_cores=3){
  parallel::makeCluster(n_cores)
  spilloverMatListM5=foreach(i=1:length(sp500_returns_slices))%dopar%{
    n=dim(sp500_returns_VaR[[i]])[2]-1
    spilloverMat=matrix(data = 0,nrow = n,ncol = n)
    tickers=colnames(sp500_returns_VaR[[i]])[-1]
    colnames(spilloverMat)=tickers
    rownames(spilloverMat)=tickers
    for(j in 1:(n-1)){
      for(k in (j+1):n){
        r1=pull(select(sp500_returns_slices[[i]],tickers[j]))
        r2=pull(select(sp500_returns_slices[[i]],tickers[k]))
        VaR1=pull(select(sp500_returns_VaR[[i]],tickers[j]))
        VaR2=pull(select(sp500_returns_VaR[[i]],tickers[k]))
        spValue=spillover(r1=r1,r2 = r2,VaR1 = VaR1,VaR2 = VaR2,M=M,BETA = BETA)
        spilloverMat[tickers[j],tickers[k]]=spValue[1]
        spilloverMat[tickers[k],tickers[j]]=spValue[2]
      }
    }
    return(spilloverMat)
  }
}