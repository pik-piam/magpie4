#' @title yields
#' @description Calculates crop yields based on a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr"), also including "pasture"
#' @param product_aggr aggregate over products or not (boolean)
#' @param attributes dry matter: Mt/ha ("dm"), gross energy: PJ/ha ("ge"), reactive nitrogen: Mt/ha ("nr"), phosphor: Mt/ha ("p"), potash: Mt/ha ("k"), wet matter: Mt/ha ("wm"). Can also be a vector.
#' @param water_aggr aggregate irrigated and non-irriagted production or not (boolean).
#' @return crop yield as MAgPIE object (unit depends on attributes)
#' @author Florian Humpenoeder
#' @seealso \code{\link{reportYields}}
#' @examples
#' 
#'   \dontrun{
#'     x <- yields(gdx)
#'   }
#' 

yields <- function(gdx,file=NULL,level="reg",products="kcr",product_aggr=F,attributes="dm",water_aggr=T) {
  prod<-production(gdx,level=level,products=products,product_aggr=product_aggr,attributes=attributes,water_aggr=water_aggr)
  if(is.null(prod)) {
    warning("Yields cannot be calculated as production function returned NULL! NULL is returned!")
    return(NULL)
  }
  if(products=="pasture"){
    area <- setNames(land(gdx, level=level, types="past"), "pasture") 
  } else {
    area <- croparea(gdx,level=level,products=products,product_aggr=product_aggr,water_aggr=water_aggr)
  }
  if(is.null(area)) {
    warning("Yields cannot be calculated as croparea function returned NULL! NULL is returned!")
    return(NULL)
  }
  x<-prod/area
  x[is.nan(x)]<-NA
  out(x,file)
}
