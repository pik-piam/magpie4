#' @title water_EFexceedance
#' @description calculation of volume of environmental flow exceedance from MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param level Level of aggregation; "cell" (cluster), "reg" (regional), "glo" (global), "regglo" (regional and global)
#' 
#' @return A MAgPIE object containing environmental flow exceedance (percentage of agricultural area)
#' @author Felicitas Beier
#' @examples
#' 
#'   \dontrun{
#'     x <- water_EFexceedance(gdx)
#'   }
#' 

water_EFexceedance <- function(gdx,level="regglo") {
  # Water availability (km^3)
  W_AV <- water_avail(gdx,level="cell",sources=NULL,sum=TRUE)
  
  # Water usage (sum over sectors: "agriculture", "industry", "electricity", "domestic") (km^3/yr)
  W_USE <- water_usage(gdx,users=c("agriculture", "industry", "electricity", "domestic"),level="cell",sum=FALSE)

  # Environmental flow requirements from LPJmL and Smakhtin algorithm ((mio. m^3)/1000 = km^3)
  EFR <- readGDX(gdx,"f42_env_flows",types="parameters",field="l",format="first_found")/1000
  EFR <- EFR[,getYears(W_AV),]
  names(dimnames(EFR)) <- c("j", "t", "")
  
  # Environmental flow violation
  EFV <- W_AV - EFR - dimSums(W_USE,dim=3) # if <0: violation of EFR
  
  tmp <- EFV
  tmp[EFV>0] <-0 # Cells where EFRs are not exceeded
  tmp[EFV<0] <-1 # Cells that exceed EFRs
  
  cluster_area <- land(gdx,level="cell",types=NULL,subcategories=NULL,sum=TRUE)
  region_area <- land(gdx,level=level,types=NULL,subcategories=NULL,sum=TRUE)
  EFV_area <- cluster_area*tmp # area affected by environmental flow violation at cluster level 
  EFV_area <- superAggregate(EFV_area, aggr_type="sum",level=level,crop_aggr=TRUE) # area affected by environmental flow violation at regional level 
  EFV_area <- EFV_area/region_area # percentage of regional area affected by EFV relative to total regional area
  
  return(EFV_area)

} 
