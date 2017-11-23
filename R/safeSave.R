

safeSave <- function( ..., file=stop("'file' must be specified"), overwrite=FALSE, save.fun=save) {
  if ( file.exists(file) & !overwrite ) stop("'file' already exists")
  save.fun(..., file=file)
}