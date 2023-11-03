#' @title getDirectory
#' @description support function to properly merge deprecated spamfiledirectory and dir input
#' 
#' @param dir new directory input
#' @param spamfiledirectory old directory input
#' @return a directory
getDirectory <- function(dir,spamfiledirectory) {
  if(spamfiledirectory!="") {
    warning("Argument \"spamfiledirectory\" is deprecated. Please use \"dir\" instead!")
    if(dir==".") dir <- spamfiledirectory
  }
  return(dir)
}
