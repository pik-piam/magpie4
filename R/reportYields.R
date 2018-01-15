#' @title reportYields
#' @description reports yields
#' 
#' @importFrom magpiesets reporthelper summationhelper
#' @export
#' 
#' @param gdx GDX file
#' @param detail if detail=FALSE, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @return yield as MAgPIE object (Mt DM/ha)
#' @author Florian Humpenoeder, Xiaoxi Wang, Kristine karstens, Abhijeet Mishra
#' @examples
#' 
#'   \dontrun{
#'     x <- reportYields(gdx)
#'   }
#' 

reportYields <- function(gdx,detail=FALSE) {
  yieldWaterAgg <- function(water_aggr =TRUE, sum_sep="+"){
    
    prod<-production(gdx,level="regglo",products=readGDX(gdx,"kcr"),product_aggr=FALSE,water_aggr=water_aggr)
    prod<-reporthelper(x=prod,dim=3.1,level_zero_name = "Productivity|Yield",detail = detail)
    
    area<-croparea(gdx,level="regglo",products=readGDX(gdx,"kcr"),product_aggr=FALSE,water_aggr=water_aggr)
    area<-reporthelper(x=area,dim=3.1,level_zero_name = "Productivity|Yield",detail = detail)
    out <- prod/area
    getNames(out) <- paste(gsub("\\.","|",getNames(out)),"(t DM/ha)",sep=" ")
    if(length(sum_sep)!=0){out <- summationhelper(out, sep=sum_sep)}
    return(out)
  }
  x <- mbind(yieldWaterAgg(water_aggr=TRUE, sum_sep="+"),yieldWaterAgg(water_aggr = FALSE, sum_sep=NULL))
  return(x)
} 
