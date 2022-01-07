#' @title water_EFexceedance
#' @description calculation of volume of environmental flow exceedance from MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param level Level of aggregation; "cell" (cluster), "reg" (regional), "glo" (global), "regglo" (regional and global)
#' @param outputdir output directory
#' @param users all sectors used in MAgPIE ("all") or only agricultural sector ("agr")
#' 
#' @return A MAgPIE object containing environmental flow exceedance
#' @author Felicitas Beier
#' @examples
#' 
#'   \dontrun{
#'     x <- water_EFexceedance(gdx)
#'   }
#' 

#### UNDER DEVELOPMENT

water_EFexceedance <- function(gdx,level="cell",users="all",outputdir=".") {
  
  cfg <- NULL
  load(paste0(outputdir, "/config.Rdata"))
  
  # water use from MAgPIE (in km^3/yr)
  if (users=="all") {
    wateruse  <- water_usage(gdx,level=level,users=c("agriculture", "manufacturing", "electricity", "domestic"),digits=10000) # unit: km^3/yr
    nonaguses <- dimSums(wateruse[,,c("manufacturing","electricity","domestic")],dim=3)
    wateruse  <- dimSums(wateruse,dim=3)    
  } else if (users=="agr") {
    wateruse  <- water_usage(gdx,level=level,users="agriculture",sum=TRUE,digits=10) # unit: km^3/yr
  } else if (users=="kcr") {
    wateruse  <- water_usage(gdx,level=level,users="kcr",sum=TRUE,digits=10000) # unit: km^3/yr
  }

  # total water availability and EFRs (km^3)
  waterav   <- read.magpie(paste0(outputdir,"/lpj_watavail_total_c200.mz"))/1000 
  #waterav   <- dimSums(readGDX(gdx,"ov43_watavail")[,,"surface.level"],dim=3)/1000
  names(dimnames(waterav)) <- c("i.j", "t", "")
  EFR       <- read.magpie(paste0(outputdir,"/lpj_envflow_total_c200.mz"))/1000 
  #EFR       <- dimSums(readGDX(gdx,"ov_watdem")[,,"ecosystem.level"],dim=3)/1000
  names(dimnames(EFR)) <- c("i.j", "t", "")
  years     <- intersect(getYears(wateruse),getYears(waterav))
  
  if (cfg$gms$c43_watavail_scenario=="nocc") {
    waterav[,years,] <- waterav[,"y1995",]  
    EFR[,years,]     <- EFR[,"y1995",]  
  }

  # Environmental flow violation volume
  EFV <- (waterav[,years,] - EFR[,years,]) - wateruse[,years,] # if <0: violation of EFR
  EFV[nonaguses[,years,]>waterav[,years,]] <- 0
  
  return(EFV)

} 
