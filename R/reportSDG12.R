#' @title reportSDG12
#' @description reports all SDG indicators relevant for SD12 - Sustainable Production and Consumption
#' @import magpiesets
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return MAgPIE object
#' @author Benjamin Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- reportSDG12(gdx)
#'   }
#' 

reportSDG12 <- function(gdx) {
  x <- NULL
  
  indicatorname="SDG|SDG12|Material footprint"
  unit="tDM/capita/yr"
  warning("better backcaclulation of footprint would be nice!")
  out <- demand(gdx,level="regglo")
  out <- out[,,findset("kcr")]
  out <- dimSums(out)
  pop <- population(gdx,level="regglo")
  out <- out/pop
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG12|Food waste"
  unit="kcal/cap/day (?)"
  out <- Kcal(gdx,level="regglo")
  tmp <- Intake(gdx,level = "regglo")
  out<-out-tmp
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
    
  indicatorname="SDG|SDG12|Food loss"
  unit="Mt"
  out <- demand(gdx,level="regglo")
  out <- out[,,findset("kall")][,,"waste"]
  out <- dimSums(out)
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  #x <- x[,,sort(getNames(x))]  
  return(x)
}

