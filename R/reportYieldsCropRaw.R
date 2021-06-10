#' @title reportYieldsCropRaw
#' @description reports potential yields after calibration
#'
#' @importFrom magpiesets reporthelper summationhelper
#' @export
#'
#' @param gdx GDX file
#' @param detail if detail=FALSE, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @return yield as MAgPIE object (Mt DM/ha)
#' @author Edna J. Molina Bacca
#' @examples
#' \dontrun{
#' x <- reportYieldsCropRaw(gdx)
#' }
#'
reportYieldsCropRaw <- function(gdx, detail = FALSE) {



  yieldWaterAgg <- function(gdx, water_aggr = TRUE, sum_sep = "+") {
    out <- YieldsCropRaw(gdx, file = NULL, level = "regglo")
    if (water_aggr == TRUE) {

      weight <- croparea(gdx, level = "regglo", products = "kcr", product_aggr = FALSE, water_aggr = FALSE)
      mapping <- as.data.frame(getNames(out))
      colnames(mapping) <- "Crops_w"
      mapping$Crops <- gsub("\\..*", "", mapping$Crops_w)

      out <- speed_aggregate(out, rel = mapping, from = "Crops_w", to = "Crops", weight = weight, dim = 3)

    } else {
      out <- out
    }

    out <- reporthelper(x = out, dim = 3.1, 
                        level_zero_name = "Productivity|Potential Yield (before calibration)", detail = detail)

    getNames(out) <- paste(gsub("\\.", "|", getNames(out)), "(t DM/ha)", sep = " ")
    if (length(sum_sep) != 0) {
      out <- summationhelper(out, sep = sum_sep)
    }
    return(out)
  }

  x <- mbind(yieldWaterAgg(gdx, water_aggr = TRUE, sum_sep = "+"), 
             yieldWaterAgg(gdx, water_aggr = FALSE, sum_sep = NULL))


  return(x)
}
