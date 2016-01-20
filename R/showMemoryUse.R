#'Function used to show the memory useage in a readable format
#'@export
#'@param sort sort by which attribute
#'@param decreasing sort by decreasing order or not
#'@param limit the maximum objects listed
showMemoryUse <- function(sort="size", decreasing=TRUE, limit=20) {
  objectList <- ls(parent.frame())

  oneKB <- 1024
  oneMB <- 1048576
  oneGB <- 1073741824
  
  memoryUse <- sapply(objectList, function(x) as.numeric(object.size(eval(parse(text=x)))))

  memListing <- sapply(memoryUse, function(size) {
        if (size >= oneGB) return(paste(round(size/oneGB,2), "GB"))
        else if (size >= oneMB) return(paste(round(size/oneMB,2), "MB"))
        else if (size >= oneKB) return(paste(round(size/oneKB,2), "kB"))
        else return(paste(size, "bytes"))
      })

  memListing <- data.frame(objectName=names(memListing),memorySize=memListing,row.names=NULL)

  if (sort=="alphabetical") memListing <- memListing[order(memListing$objectName,decreasing=decreasing),] 
  else memListing <- memListing[order(memoryUse,decreasing=decreasing),] #will run if sort not specified or "size"

  if(!missing(limit)) memListing <- memListing[1:limit,]

  print(memListing, row.names=FALSE)
  return(invisible(memListing))
}