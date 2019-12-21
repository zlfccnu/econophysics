#' Function used to create a simple project subdirectories
#' @param projectName the name of the project, relative path or absolute path
#' @return invisible
#' @export
createProject=function(projectName){
  ## create project root directory
  dir.create(path = projectName)
  setwd(projectName)
  ## create project sub directories
  # source code for c/c++/F
  dir.create("./src")
  # source code for R
  dir.create("./R")
  # raw data
  dir.create("./rawData")
  # R processed data
  dir.create("./rData")
  # figures
  dir.create("./figures")
  # report files
  dir.create("./reports")
  # documents
  dir.create("./doc")
  ## create project related text files
  # REAME file
  file.create("./README")
  file.create("./LICENSE")
  file.create("./NOTES")
  write("This package is under MIT License.","./LICENSE")
}