#' This function is aiming at converting author names to standard format: family name, Short. Short.(Zhu, Z. W.;Keller, A. Z.), remove blank space
#' @param x a string represents a name or a vector
#' @return a string 
#' @export
correctNames=function(x){
  tmp=function(x){
    if(is.na(x)!=TRUE){
      x=gsub('[[:punct:] ]+',' ',x)
      x=strsplit(x,split = " ")[[1]]
      x=paste(paste0(x[1],",",collapse = ""),paste(paste0(x[-1],"."),collapse = " "))
    }
    return(x)
  }
  x=sapply(x,tmp)
  return(x)
}
