#' @title croparea
#' @description reads croparea out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param product_aggr aggregate over products or not (boolean)
#' @param water_aggr aggregate irrigated and non-irriagted production or not (boolean).
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' @return production as MAgPIE object (unit depends on attributes)
#' @author Jan Philipp Dietrich, Florian Humpenoeder
#' @seealso \code{\link{reportCroparea}}
#' @examples
#' 
#'   \dontrun{
#'     x <- croparea(gdx)
#'   }
#' 

croparea <- function(gdx, file=NULL, level="reg", products="kcr", product_aggr=TRUE, water_aggr=TRUE,spamfiledirectory="") {

  x <- readGDX(gdx,"ov_area",format="first_found",select = list(type="level"))  
  
  if(is.null(x)) {
    warning("Crop area cannot be calculated as area data could not be found in GDX file! NULL is returned!")
    return(NULL)
  }
  if(water_aggr) x <- dimSums(x,dim=3.2)
  if (!all(products%in%findset("kcr"))){
    products<-readGDX(gdx,products)
  }
  x <- x[,,products]
  if(product_aggr) x <- dimSums(x,dim=3.1)
  out <- gdxAggregate(gdx,x,to=level,weight="land",type="crop",absolute = T,
                      spamfiledirectory = spamfiledirectory)
  out(out,file)
}


