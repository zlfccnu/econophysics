### minimum variance portfolio optimization with CVXR
## Problem data
efficient_frontier_optimization=function(Returns,SAMPLES=100,gammas= 10^seq(-6, 3, length.out = SAMPLES)){
  require("CVXR")
  set.seed(10)
  n <- dim(Returns)[2]
  mu <- Returns %>% colSums(na.rm = TRUE)
  Sigma <- cov(Returns)
  
  ## Form problem
  w <- Variable(n)
  ret <- t(mu) %*% w
  risk <- quad_form(w, Sigma)
  constraints <- list(w >= 0, sum(w) == 1)
  
  ## Risk aversion parameters
  ret_data <- rep(0, SAMPLES)
  risk_data <- rep(0, SAMPLES)
  w_data <- matrix(0, nrow = SAMPLES, ncol = n)
  
  ## Compute trade-off curve
  for(i in seq_along(gammas)) {
    gamma <- gammas[i]
    objective <- ret - gamma * risk
    prob <- Problem(Maximize(objective), constraints)
    result <- solve(prob)
    ## Evaluate risk/return for current solution
    risk_data[i] <- result$getValue(sqrt(risk))
    ret_data[i] <- result$getValue(ret)
    w_data[i,] <- result$getValue(w)
  }
  return(tibble(risk = risk_data,Returns = ret_data))
}


mean_var_optimization=function(Returns,shorting=FALSE,shorting_limit=-1,concentration=NULL){
  require("CVXR")
  set.seed(10)
  n <- dim(Returns)[2]
  mu <- Returns %>% colSums(na.rm = TRUE)
  Sigma <- cov(Returns)
  ticker_names = colnames(Returns)
  ## Form problem
  w <- Variable(n)
  ret <- t(mu) %*% w
  mean_return=mean(mu)
  risk <- quad_form(w, Sigma)
  ## setup the constraints
  if(isFALSE(shorting)){
    if(is.null(concentration)){
      constraints <- list(w >= 0, sum(w) == 1,ret==mean_return)
    }else{
      constraints <- list(w >= 0,w<=concentration, sum(w) == 1,ret==mean_return)
    }
  }else{
    if(is.null(concentration)){
      constraints <- list(w>= shorting_limit, sum(w) == 1,ret==mean_return)
    }else{
      constraints <- list(w>= shorting_limit,w<=concentration, sum(w) == 1,ret==mean_return)
    }
  }
  objective <- risk
  prob <- Problem(Minimize(objective), constraints)
  result <- solve(prob)
  ## Evaluate risk/return for current solution
  risk_data <- result$getValue(sqrt(risk))
  ret_data <- result$getValue(ret)
  w_data <- result$getValue(w)
  w_data = as.vector(w_data)
  names(w_data) = ticker_names
  return(w_data)
}


global_min_var_portfolio = function(Returns,shorting=FALSE,shorting_limit=-1,concentration=NULL){
  require("CVXR")
  set.seed(10)
  n <- dim(Returns)[2]
  mu <- Returns %>% colSums(na.rm = TRUE)
  Sigma <- cov(Returns)
  ticker_names = colnames(Returns)
  ## Form problem
  w <- Variable(n)
  ret <- t(mu) %*% w
  risk <- quad_form(w, Sigma)
  ## setup the constraints
  if(isFALSE(shorting)){
    if(is.null(concentration)){
      constraints <- list(w >= 0, sum(w) == 1)
    }else{
      constraints <- list(w >= 0,w<=concentration, sum(w) == 1)
    }
  }else{
    if(is.null(concentration)){
      constraints <- list(w>= shorting_limit, sum(w) == 1)
    }else{
      constraints <- list(w>= shorting_limit,w<=concentration, sum(w) == 1)
    }
  }
  objective <- risk
  prob <- Problem(Minimize(objective), constraints)
  result <- solve(prob)
  
  ## Evaluate risk/return for current solution
  risk_data<- result$getValue(sqrt(risk))
  ret_data<- result$getValue(ret)
  w_data <- result$getValue(w) %>% as.vector()
  names(w_data)<- ticker_names
  return(w_data)
}



global_max_ret_portfolio = function(Returns,shorting=FALSE,shorting_limit=-1,concentration=NULL){
  require("CVXR")
  set.seed(10)
  n <- dim(Returns)[2]
  mu <- Returns %>% colSums(na.rm = TRUE)
  Sigma <- cov(Returns)
  ticker_names = colnames(Returns)
  ## Form problem
  w <- Variable(n)
  ret <- t(mu) %*% w
  risk <- quad_form(w, Sigma)
  ## setup the constraints
  if(isFALSE(shorting)){
    if(is.null(concentration)){
      constraints <- list(w >= 0, sum(w) == 1)
    }else{
      constraints <- list(w >= 0,w<=concentration, sum(w) == 1)
    }
  }else{
    if(is.null(concentration)){
      constraints <- list(w>= shorting_limit, sum(w) == 1)
    }else{
      constraints <- list(w>= shorting_limit,w<=concentration, sum(w) == 1)
    }
  }
  objective <- ret
  prob <- Problem(Minimize(objective), constraints)
  result <- solve(prob)
  ## Evaluate risk/return for current solution
  risk_data<- result$getValue(sqrt(risk))
  ret_data<- result$getValue(ret)
  w_data <- result$getValue(w) %>% as.vector()
  names(w_data)<- ticker_names
  return(w_data)
}



