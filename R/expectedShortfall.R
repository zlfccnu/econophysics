#'use the box constriant to calculate the expected shortfall of a portfolio
#'@param tickers the names of the stocks
#'@param returns the return time series of stocks, log return
#'@return a numeric value 
#' @export
expectedShortfall=function(tickers,returns){
  stopifnot(is.xts(returns))
  if(prod(tickers%in%colnames(returns))==1){
    wMin=0
    wMax=1
    searchSize=50000
    itermax=200
    controlDE=DEoptim.control(itermax = itermax)
    ES_tmp=portfolio.spec(assets = tickers)
    ES_tmp=add.constraint(portfolio=ES_tmp, type='box', min = wMin, max=wMax)
    ES_tmp=add.constraint(portfolio = ES_tmp,type="weight_sum",min_sum=0.99,max_sum=1.01)
    ES_tmp=add.objective(portfolio=ES_tmp, type="risk", name="CVaR",arguments=list(p=0.95,clean="boudt"))
    ES_tmp_out=optimize.portfolio(R=returns[,tickers], portfolio=ES_tmp ,trace = FALSE,search_size = searchSize,optimize_method="DEoptim",control=controlDE)
    return(ES_tmp_out$out)
  }else{
    stop("the returns dataframe should have colnames!")
  }
}