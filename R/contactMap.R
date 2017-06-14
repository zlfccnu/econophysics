#'calculate the contact map from the interaction strength matrix
#'@param fileName the input files with external field and interaction strength
#'@param n the system size, skip the first n lines, external field
#'@export
contactMap=function(fileName,n){
  learn_j_norm=matrix(data = 0,nrow = n,ncol = n)
  dat=scan(file = fileName,what = "",sep="\n",skip = n)
  dat=strsplit(dat,split ="[[:space:]]+")
  dat=lapply(dat,as.numeric)
  dat=lapply(dat,as.matrix)
  dat=sapply(dat,norm,type="F")
  learn_j_norm[lower.tri(learn_j_norm,diag = FALSE)]=dat
  learn_j_norm=t(learn_j_norm)
  learn_j_norm[lower.tri(learn_j_norm,diag = FALSE)]=dat
  apc=average_product_correction(learn_j_norm)
  return(apc)
}