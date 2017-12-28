#' safe save for objects
#' @param ... the objects
#' @param file the file name to store the objects
#' @param overwrite overwrite the existed file or not
#' @param the function used to save the objects 
#' @return invisible
safeSave <- function( ..., file=stop("'file' must be specified"), overwrite=FALSE, save.fun=save.image) {
  if ( file.exists(file) & !overwrite ) stop("'file' already exists")
  if (file.exists(file) & overwrite) readline(prompt="Are you really want to over write? 
Press [enter] to continue OR press [ESC] to escape")
  save.fun(..., file=file)
}