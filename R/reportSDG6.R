#' @title reportSDG6
#' @description reports all SDG indicators relevant for SDG6 - Access to Water
#'
#' @export
#'
#' @param gdx GDX file
#' @param level level of aggregation (cluster: "cell", regional: "regglo")
#'
#' @return MAgPIE object
#' @author Felicitas Beier, Isabelle Weindl
#' @import magclass
#' @examples
#'
#'   \dontrun{
#'     x <- reportSDG6(gdx)
#'   }
#'
#'
#' @section SDG6 Water variables:
#' Name | Unit | Meta
#' ---|---|---
#' SDG\|SDG06\|Fertilizer use | Mt N/yr | Nitrogen fertilizer application on cropland
#' SDG\|SDG06\|Nitrogen surplus on cropland | Mt N/yr | Nitrogen surplus from cropland budget
#' SDG\|SDG06\|Agricultural water use | km3/yr | Agricultural water withdrawal during growing period
#' @md


reportSDG6 <- function(gdx, level = "regglo") {
  x <- NULL
  #cfg <- NULL
  #load(paste0(dirname(normalizePath(gdx)), "/config.Rdata"))

  indicatorname <- "SDG|SDG06|Safe sanitation"
  unit          <- "fraction"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)

  indicatorname <- "SDG|SDG06|Safe wastewater"
  unit          <- "fraction"
  #missing (retrieve from moinput?)
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)

  indicatorname <- "SDG|SDG06|Water quality"
  unit          <- "fraction"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)

  indicatorname <- "SDG|SDG06|N water loading"
  unit          <- "Mt N/yr"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # runoff
  # x <- mbind(x,out)

  indicatorname <- "SDG|SDG06|P water loading"
  unit          <- "Mt P/yr"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)

  budget <- NitrogenBudget(gdx, level = "regglo")

  indicatorname <- "SDG|SDG06|Fertilizer use"
  unit          <- "Mt N/yr"
  # Def.: Nitrogen fertilizer use
  out <- budget[,,"fertilizer"]
  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x <- mbind(x,out)

  indicatorname <- "SDG|SDG06|Nitrogen surplus on cropland"
  unit          <- "Mt N/yr"
  # Def.: Nitrogen surplus on cropland
  out <- budget[,,"surplus"]
  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x <- mbind(x, out)

  indicatorname <- "SDG|SDG06|Nitrate concentration in water"
  unit          <- "tN/km3"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)

  indicatorname <- "SDG|SDG06|Water use efficiency"
  unit          <- "rate"
  #missing
  # Def.: a country's total gross domestic product (GDP) divided by total freshwater withdrawals (OurWorldInData: sdg-tracker.org????)
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)

  # indicatorname="SDG|SDG06|Water stress"
  # unit="fraction"
  # # Def.: total quantity of freshwater withdrawals (agriculture, manufacturing, domestic; km^3) as a share of total available freshwater resources (km^3)
  # # water use from MAgPIE
  # wateruse  <- water_usage(gdx,level=level,users=c("agriculture", "manufacturing", "electricity", "domestic"),digits=10) # unit: km^3/yr
  # nonaguses <- dimSums(wateruse[,,c("manufacturing","electricity","domestic")],dim=3)
  # wateruse  <- dimSums(wateruse,dim=3)
  # # total water availability (km^3)
  # waterav   <- read.magpie(paste0(dirname(normalizePath(gdx)),"/lpj_watavail_total_c200.mz"))/1000
  # years     <- intersect(getYears(wateruse),getYears(waterav))
  # if (cfg$gms$c43_watavail_scenario=="nocc") {
  #   waterav[,years,] <- waterav[,"y1995",]
  # }
  # waterav   <- gdxAggregate(gdx, waterav, weight="sum", to=level, absolute=TRUE)
  # # water scarcity
  # out           <- wateruse[,years,]/waterav[,years,]
  # # where non-agricultural uses exceed water availability (special rule in MAgPIE): scarcity indicator capped to 1
  # out[nonaguses[,years,]>waterav[,years,]] <- 1
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
  #
  # indicatorname="SDG|SDG06|Agricultural water stress"
  # unit="fraction"
  # # Def.: quantity of agricultural freshwater withdrawals as a share of total available freshwater resources (km^3)
  # # water use from MAgPIE
  # wateruse  <- water_usage(gdx,level=level,users="agriculture",digits=10,sum=TRUE) # unit: km^3/yr
  # # total water availability (km^3)
  # waterav   <- read.magpie(paste0(dirname(normalizePath(gdx)),"/lpj_watavail_total_c200.mz"))/1000
  # years     <- intersect(getYears(wateruse),getYears(waterav))
  # if (cfg$gms$c43_watavail_scenario=="nocc") {
  #   waterav[,years,] <- waterav[,"y1995",]
  # }
  # waterav   <- gdxAggregate(gdx, waterav, weight="sum", to=level, absolute=TRUE)
  # # water scarcity
  # out           <- wateruse[,years,]/waterav[,years,]
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)

  indicatorname <- "SDG|SDG06|People under water stress"
  unit          <- "million"
  #missing (Def.: number of people living in water stressed region)
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)

  indicatorname <- "SDG|SDG06|Environmental flow exceedance"
  unit          <- "ratio"
  # Def.: ratio of violated environmental flows over total environmental flows
  #EFV <- water_EFexceedance(gdx,level=level,users=c("agriculture", "manufacturing", "electricity", "domestic"))

  #tmp <- EFV
  #tmp[EFV>0] <-0 # Cells where EFRs are not exceeded
  #tmp[EFV<0] <-(-1) # Cells that exceed EFRs

  #EFV_volume <- EFV*tmp # area affected by environmental flow violation
  #ratio_EFV  <- EFV_volume[,years,]/EFR[,years,]  # ratio of violation volume to total EFV
  #global_ratio <- dimSums(EFV_volume[,years,],dim=1)/dimSums(EFR[,years,],dim=1)
  #EFV_area <- superAggregate(EFV_area, aggr_type="sum",level=level,crop_aggr=TRUE) # area affected by environmental flow violation at reporting level


  #getNames(out) <- paste0(indicatorname, " (",unit,")")
  #x <- mbind(x,out)

  indicatorname <- "SDG|SDG06|Agricultural water use"
  unit          <- "km3/yr"
  # Def.: water usage in agriculture in the growing period
  out <- water_usage(gdx, level = "regglo", users = "agriculture", seasonality = "grper",
                     sum = TRUE, digits = 3)
  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x <- mbind(x,out)

  indicatorname <- "SDG|SDG06|Water-related ecosystems"
  unit          <- "million ha"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)

  #x <- x[,,sort(getNames(x))]
  return(x)
}
