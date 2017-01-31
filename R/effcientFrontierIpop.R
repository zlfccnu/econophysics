#' get the efficient frontier, no matter the covariance matrix is postive semidefinite or not
#'@param retruns The return matrix of all the assets,T*N dimension
#'@param covMat The predefined covariance matrix
#'@param short whether short selling is prohibited or not
#'@param max.allocation the maximum allocation of the asset, can control the over conceration
#'@param risk.premim the maximum risk aversion parameter
#'@param risk.increment the incremental of the risk aversion
#'@param thread the multi threads number
#'@param maxiter Maximum number of iterations
#'@return a data.frame include the weight, variance, return and shape ratio
#'@export
eff.frontier_ipop=function(returns,covMat=NULL,short="yes",max.allocation=NULL,risk.premium.up=20,thread=3,maxiter=40){
  ## parameter definition
  b=1
  r=0
  n=dim(returns)[2]## number of assets
  ## the average return of every asset, the minus means maximize the return
  Amat=matrix(1,ncol=401)
  if(short=="no"){
    l=matrix(0,n)
    u=matrix(1,n)
  }
  if(short=="yes"){
    l= matrix(-(n-2),n)## the lower bound of the weight of each asset
    u=matrix(1,n) ## the upper bound of the weight of each asset
  }
  registerDoMC(thread)
  eff=NULL
  riskSeq= (exp(seq(0.001,log(risk.premium.up),0.1))-1)
  if(is.null(covMat)){
    eff<- foreach(i=riskSeq,.combine = "rbind")%dopar%{
      cMat=i*matrix(-colMeans(returns),nrow=n)
      effTemp=ipop(c=cMat,H=cov(returns),A=Amat,b=b,l=l,u=u,r=r,maxiter = maxiter)
      effTemp=primal(effTemp)
      effTemp[abs(effTemp)<= 1e-7]<- 0
      W=as.matrix(as.numeric(effTemp),nrow=n)
      Std.Dev=sqrt(t(W)%*%cov(returns)%*%W)[1,1]
      Exp.Return=(t(W)%*%colSums(returns))[1,1]
      effTemp=c(effTemp,i,Std.Dev,Exp.Return)
      names(effTemp)<- c(colnames(returns),"riskAversion","Std.Dev","Exp.Return")
      return(effTemp)
    }
  }
  if(!is.null(covMat)){
    eff<- foreach(i=riskSeq,.combine = "rbind")%dopar%{
      cMat=i*matrix(-colMeans(returns),nrow=n)
      effTemp=ipop(c=cMat,H=covMat,A=Amat,b=b,l=l,u=u,r=r,maxiter = maxiter)
      effTemp=primal(effTemp)
      effTemp[abs(effTemp)<= 1e-7]<- 0
      W=as.matrix(as.numeric(effTemp),nrow=n)
      Std.Dev=sqrt(t(W)%*%cov(returns)%*%W)[1,1]
      Exp.Return=(t(W)%*%colMeans(returns))[1,1]
      effTemp=c(effTemp,i,Std.Dev,Exp.Return)
      names(effTemp)<- c(colnames(returns),"riskAversion","Std.Dev","Exp.Return")
      return(effTemp)
    }
  }
  rownames(eff)<- NULL
  return(as.data.frame(eff))
}

