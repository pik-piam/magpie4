#' @title reportLivestockDemStructure
#' @description reports the share of different livestock products (excluding fish) in total livestock calorie food supply
#' 
#' @export
#'
#' @param gdx GDX file
#' @return livestock demand structure as MAgPIE object (kcal/kcal)
#' @author Isabelle Weindl
#' @importFrom magpiesets findset
#' @examples
#' 
#'   \dontrun{
#'     x <- reportLivestockDemStructure(gdx)
#'   }
#' 

reportLivestockDemStructure<-function(gdx){
  out<-LivestockDemStructure(gdx,level = "regglo",attributes="kcal",fish=FALSE)
  
  group="kli"
  products=findset("kli")
  level_zero_name <- "Nutrition|Dietary Composition|Livestock Demand Structure"
  out<-reporthelper(x=out,level_zero_name = level_zero_name,detail=T)
  
  nosum  <- out[,,paste(level_zero_name,reportingnames(group),sep="|"),invert=TRUE]
  out <- summationhelper(nosum)
  getNames(out) <- paste(getNames(out),"(kcal/kcal)",sep=" ")
  
  return(out)
}

