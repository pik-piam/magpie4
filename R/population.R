#' @title population
#' @description reads population out of a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @return population as MAgPIE object (million people)
#' @author Florian Humpenoeder, Benjamin Bodirsky
#' @importFrom magclass colSums
#' @seealso \code{\link{reportPopulation}}
#' @examples
#' 
#'   \dontrun{
#'     x <- population(gdx)
#'   }
#' 

population <- function(gdx, file=NULL, level="reg") {
  pop <- readGDX(gdx, "im_pop_iso", format="first_found", react="warning")
  if (!is.null(pop)) {
    if (level=="reg"){
      mapping<-readGDX(gdx,"i_to_iso")
      pop<-speed_aggregate(x=pop,rel = mapping,from = "iso",to="i",dim = 1)
    } else if(level=="glo"){
      pop<-colSums(pop)
    } else if (level=="regglo"){
      mapping<-readGDX(gdx,"i_to_iso")
      pop<-speed_aggregate(x=pop,rel = mapping,from = "iso",to="i",dim = 1)
      pop<-mbind(pop,colSums(pop))
    } else if (level!="iso"){stop("level does not exist (yet)")}
  } 
  out(pop,file)
}