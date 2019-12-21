#' use the DEoptim algorithm to calculate the optimal ES by using the box constriant 
#' @param stockNames the ticker of stocks
#' @param returns the return matrix of a bunch of stocks
#' @param wMin the minimum weight of each stock
#' @param wMax the maximum weight of each stock
#' @param searchSize the search steps of DEoptim algorithm
#' @param intermax the parameter for DEoptim.control function
#' @param min_sum the minimum of the weight sum
#' @param max_sum the maximum of the weight sum
#' @param p the confidence level of the ES
#' @param trace whether print the interctive process or not
#' @return A numeric value
#' @export

optimalES=function(stockNames,returns,wMin=0,wMax=1,searchSize=50000,itermax=200,min_sum=0.99,max_sum=1.01,p=0.95,trace=FALSE){
  if(prod(stockNames%in%colnames(returns))==1){
    controlDE=DEoptim.control(itermax = itermax,trace=trace)
    ES=portfolio.spec(assets = stockNames)
    ES=add.constraint(portfolio=ES, type='box', min = wMin, max=wMax)
    ES=add.constraint(portfolio = ES,type="weight_sum",min_sum=min_sum,max_sum=max_sum)
    ES=add.objective(portfolio=ES, type="risk", name="CVaR",arguments=list(p=p,clean="boudt"))
    ESOptimOut=optimize.portfolio(R=as.xts(returns), portfolio=ES,trace =trace,search_size = searchSize,optimize_method="DEoptim",control=controlDE)
    return(ESOptimOut$out)
  }else{
    stop("please give the correct stock ticker names!")
  }
}