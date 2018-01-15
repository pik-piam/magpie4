#' @title modelstat
#' @description Function to check if the library functions work with the newest magpie version
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @return A list with three entries:\itemize{\item{\code{notest}} Testing these functions was impossible because there wer missing arguments.\item{\code{error}} These functions could not be executed properly.\item{\code{fine}} Everything was fine with these functions.}
#' @details This function simply tries to run all functions in the magpie library on the provided gdx file.
#' @author Markus Bonsch
#' @importFrom utils ls.str
#' @examples
#' 
#'   \dontrun{
#'     x <- modelstat(gdx)
#'   }
#'

checkLibrary <- function(gdx,level=NULL) {
  out<-list()
  functions<-as.vector(ls.str("package:magpieflexreg",mode="function"))
  functions<-functions[-which(functions=="checkLibrary")]
  tmpfunc<-function(x){
    if(!is.null(x)){
      if(x==""){
        return(TRUE)
      } else{
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  }
  cat("\n\n")
  for(func in functions){
    cat("Testing function ",which(functions==func)," of ",length(functions),": ",func,"\n")
    arguments<-as.list(formals(func))
    arguments$gdx<-gdx
    arguments[["..."]]<-NULL
    if(level %in% names(arguments) && !is.null(level)){
      arguments$level<-level
    }
    if(any(unlist(lapply(arguments,tmpfunc)))){
      out$notest<-c(out$notest,func)
    } else {
      tmp<-try(do.call(func,args=arguments))
      if(is(tmp,"try-error")){
        out$error<-c(out$error,func)
      } else{
        out$fine<-c(out$fine,func)
      }
    }
  }
  return(out)
} 
 