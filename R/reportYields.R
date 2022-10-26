#' @title reportYields
#' @description reports yields
#'
#' @importFrom magpiesets reporthelper summationhelper
#' @export
#'
#' @param gdx GDX file
#' @param detail if detail=FALSE, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @param physical if true (default) physical area (croparea) used for yield calculation; if false harvested area used for yield calculation
#' @return yield as MAgPIE object (Mt DM/ha)
#' @author Florian Humpenoeder, Xiaoxi Wang, Kristine karstens, Abhijeet Mishra, Felicitas Beier
#' @examples
#'
#'   \dontrun{
#'     x <- reportYields(gdx)
#'   }
#'

reportYields <- function(gdx, detail=FALSE, physical=TRUE) {

  yieldWaterAgg <- function(water_aggr =TRUE, sum_sep="+"){

    prod<-production(gdx,level="regglo",products=readGDX(gdx,"kcr"),product_aggr=FALSE,water_aggr=water_aggr)
    prod<-reporthelper(x=prod,dim=3.1,level_zero_name = "Productivity|Yield",detail = detail)

    area<-croparea(gdx,level="regglo",products=readGDX(gdx,"kcr"),product_aggr=FALSE,water_aggr=water_aggr)
    area<-reporthelper(x=area,dim=3.1,level_zero_name = "Productivity|Yield",detail = detail)

    if(!physical){
      # Read in multicropping (ratio between area harvested and physical cropland area)
      multicropping <- readGDX(gdx,level="regglo","fm_multicropping",types="parameters")[,getYears(area),]
      # Correct regions
      area_reg <- area[getRegions(multicropping),,]
      # Transform crop area (physical area) into harvested area
      area_reg <- area_reg * multicropping
      # Global sum and regions
      area[,,] <- NA
      area[getRegions(multicropping),,] <- area_reg
      area["GLO",,] <- dimSums(area_reg, dim=1)
    }

    out <- prod/area
    getNames(out) <- paste(gsub("\\.","|",getNames(out)),"(t DM/ha)",sep=" ")
    if(length(sum_sep)!=0){out <- summationhelper(out, sep=sum_sep)}
    return(out)
  }

  x <- mbind(yieldWaterAgg(water_aggr=TRUE, sum_sep="+"),yieldWaterAgg(water_aggr = FALSE, sum_sep=NULL))

  pasture <- yields(gdx,level="regglo",products="pasture",attributes="dm")
  pasture <- summationhelper(reporthelper(x=pasture,dim=3.1,level_zero_name = "Productivity|Yield",detail = detail), sep="+")
  getNames(pasture) <- paste(getNames(pasture),"(t DM/ha)",sep=" ")

  x <- mbind(x,pasture)

  return(x)
}
