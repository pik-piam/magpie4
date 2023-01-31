#' @title reportBiodiversityHotspotIntactness
#' @description reports biodiversity hotspot intactness intactness index
#'
#' @export
#'
#' @param gdx GDX file
#' @param dir magpie output directory that contains gridded BII data
#' @return Biodiversity hotspot intactness index as MAgPIE object
#' @author Patrick v. Jeetze
#' @examples
#' \dontrun{
#' x <- reportBiodiversityHotspotIntactness(gdx)
#' }
#'
reportBiodiversityHotspotIntactness <- function(gdx, dir = ".") {
  consvPrio <- c("input/consv_prio_areas_0.5.mz",
                 "modules/22_land_conservation/input/consv_prio_areas_0.5.mz",
                 "../input/consv_prio_areas_0.5.mz",
                 "../modules/22_land_conservation/input/consv_prio_areas_0.5.mz",
                 "../../input/consv_prio_areas_0.5.mz",
                 "../../modules/22_land_conservation/input/consv_prio_areas_0.5.mz")
  consvPrio <- suppressWarnings(consvPrio[min(which(file.exists(consvPrio)))])
  BHArea <- dimSums(read.magpie(consvPrio)[,, "BH"], dim = 3)

  a <- BII(gdx, level = "regglo", mode = "from_grid",
           adjusted = TRUE, spatialWeight = BHArea, dir = dir)
  if (!is.null(a)) getNames(a) <- "Biodiversity|Biodiversity hotspot intactness (unitless)" else cat("No Biodiversity hotspot intactness reporting possible")
  out <- a
  return(out)
}

