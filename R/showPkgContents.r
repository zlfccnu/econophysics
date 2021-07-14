#' Show the functions and objects in a pkg
#' @param packageName a character
#' @return a list
#' @export

showPackageContents <- function (packageName) {
  #library(conflicted)
  # conflict_prefer("as_data_frame", "tibble")
  # conflict_prefer("between", "dplyr")
  filter<- dplyr::filter
  # Get a list of things contained in a particular package
  funlist <- objects(packageName)
   # Remove things that don't start with a letter
  idx <- grep('^[a-zA-Z][a-zA-Z0-9._]*', funlist)
  funlist <- funlist[idx]
  
  # Remove things that contain arrow <-
  idx <- grep('<-', funlist)
  if (length(idx)!=0)
    funlist <- funlist[-idx]
  
  # Make a data frame to keep track of status
  objectlist <- data.frame(name=funlist,
                           primitive=FALSE,
                           func=FALSE,
                           object=FALSE,
                           constant=FALSE,
                           stringsAsFactors=F)
  
  for (i in 1:nrow(objectlist)) {
    fname <- objectlist$name[i]
    if (exists(fname)) {
      obj <- get(fname,pos = as.environment(packageName))
      if (is.primitive(obj)) {
        objectlist$primitive[i] <- TRUE
      }
      if (is.function(obj)) {
        objectlist$func[i] <- TRUE
      }
      if (is.object(obj)) {
        objectlist$object[i] <- TRUE
      }
      
      # I think these are generally constants
      if (is.vector(obj)) {
        objectlist$constant[i] <- TRUE
      }
      
      
    }  
  }
  
  cat(packageName)
  
  cat("\n================================================\n")
  cat("Primitive functions: \n")
  cat(objectlist$name[objectlist$primitive])
  cat("\n")
  
  cat("\n================================================\n")
  cat("Non-primitive functions: \n")
  cat(objectlist$name[objectlist$func  &  !objectlist$primitive])
  cat("\n")
  
  cat("\n================================================\n")
  cat("Constants: \n")
  cat(objectlist$name[objectlist$constant])
  cat("\n")
  
  cat("\n================================================\n")
  cat("Objects: \n")
  cat(objectlist$name[objectlist$object])
  cat("\n")
  return(list(PrimitiveFunctions=objectlist$name[objectlist$primitive],Non_primitiveFunctions=objectlist$name[objectlist$func  &  !objectlist$primitive],Constants=objectlist$name[objectlist$constant],Objects=objectlist$name[objectlist$object]))
}
