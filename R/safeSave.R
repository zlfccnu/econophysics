#'safe save for objects
#'@param ... the objects
#'@param file the file name to store the objects
#'@param overwrite overwrite the existed file or not
#'@param the function used to save the objects 
#'@export

safeSave <- function( ..., file=stop("'file' must be specified"), overwrite=FALSE, save.fun=save.image) {
  if ( file.exists(file) & !overwrite ) stop("'file' already exists")
  save.fun(..., file=file)
}