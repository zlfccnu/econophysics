#' resolve the function conflict for one pkg over any other pkgs
#' @param pkg the name of the package that wants to win
#' @return invisible
#' @export
conflict_resolve<- function(pkg){
  library("conflicted")
  library(pkg,character.only = TRUE)
  ls(paste0("package:",pkg)) %>% lapply(conflict_prefer,winner=pkg) %>% invisible()
}
