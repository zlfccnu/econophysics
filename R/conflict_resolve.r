conflict_resolve<- function(pkg){
  library("conflicted")
  library(pkg,character.only = TRUE)
  ls(paste0("package:",pkg)) %>% lapply(conflict_prefer,winner=pkg) %>% invisible()
}
