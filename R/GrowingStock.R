#' @title GrowingStock
#' @description reads woody growing stock out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param indicator If the reported numbers are relative (mio m3/ha) or absolute (mio. m3). Default is relative.
#' @details Growing stock for producing woody materials consist of growing stock from plantations (forestry), secondary and primary forest as well as other land (natveg)
#' @return Growing stock in m3 per ha
#' @author Abhijeet Mishra
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @examples
#'
#' \dontrun{
#' x <- GrowingStock(gdx)
#' }
#'
GrowingStock <- function(gdx, file = NULL, level = "regglo", indicator = "relative") {
  if (level %in% c("reg", "glo", "regglo") || isCustomAggregation(level)) {
    ac_sub <- readGDX(gdx, "ac")

    wood_density <- readGDX(gdx, "im_vol_conv", "f73_volumetric_conversion", react = "silent", format = "first_found")

    ## Read timber yield and convert from tDM/ha to m3/ha
    pm_growing_stock <- readGDX(gdx, "pm_growing_stock", "pm_timber_yield", react = "silent", format = "first_found") / wood_density

    ## Land information - cluster level
    land_plantations       <- collapseNames(readGDX(gdx, "ov32_land", "ov_land_fore", select = list(type = "level"),
                                                    react = "silent")[, , "plant"][, , ac_sub])
    land_afforest          <- collapseNames(dimSums(readGDX(gdx, "ov32_land", "ov_land_fore", select = list(type = "level"),
                                                            react = "silent")[, , "plant", invert = TRUE][, , ac_sub], dim = "type32"))
    land_secdforest        <- collapseNames(readGDX(gdx, "ov35_secdforest", select = list(type = "level"))[, , ac_sub])
    land_primforest        <- setNames(collapseNames(readGDX(gdx, "ov_land", select = list(type = "level"))[, , "primforest"]), "acx")
    land_other             <- collapseNames(readGDX(gdx, "ov_land_other", "ov35_other", select = list(type = "level"),
                                                    format = "first_found")[, , ac_sub])
    if (getSets(land_other, fulldim = FALSE)[[3]] == "othertype35.ac") land_other <- dimSums(land_other, dim = "othertype35")
    land_natfor            <- land_secdforest
    land_natfor[, , "acx"] <- land_natfor[, , "acx"] + land_primforest
    land_forest_total      <- land_plantations + land_afforest + land_natfor

    ## Only use ac_sub
    land_plantations       <- land_plantations[, , ac_sub]
    land_afforest          <- land_afforest[, , ac_sub]
    land_secdforest        <- land_secdforest[, , ac_sub]
    land_natfor            <- land_natfor[, , ac_sub]
    land_other             <- land_other[, , ac_sub]
    land_forest_total      <- land_forest_total[, , ac_sub]
    pm_growing_stock        <- pm_growing_stock[, , ac_sub]

    ## Yields x land = standing growing stocks (mio. m3)

    growing_stock_main <- superAggregateX(
      data = mbind(
        setNames(dimSums(land_plantations * pm_growing_stock[, , "forestry"],              dim = 3), "plantations"),
        setNames(dimSums(land_afforest    * pm_growing_stock[, , "secdforest"],            dim = 3), "afforestation"),
        setNames(dimSums(land_secdforest  * pm_growing_stock[, , "secdforest"],            dim = 3), "secdforest"),
        setNames(dimSums(land_primforest  * pm_growing_stock[, , "primforest"][, , "acx"], dim = 3), "primforest"),
        setNames(dimSums(land_other       * pm_growing_stock[, , "other"],                 dim = 3), "other")
      ),
      aggr_type = "sum", level = level
    )

    ## Create GS for natural and total forests, as this is absolute amount still it can be added up
    growing_stock_natfor <- setNames(growing_stock_main[, , "secdforest"] + growing_stock_main[, , "primforest"], "natfor")

    growing_stock_forest_total <- setNames(growing_stock_natfor + growing_stock_main[, , "plantations"] + growing_stock_main[, , "afforestation"], "forest")

    a <- mbind(growing_stock_forest_total, growing_stock_natfor, growing_stock_main)

    ## In case relative numbers are needed, divide by aggregated land information

    if (indicator == "relative") {
      growing_stock_forest_total  <- growing_stock_forest_total              / superAggregateX(data = dimSums(land_forest_total, dim = "ac"), aggr_type = "sum", level = level)
      growing_stock_natfor        <- growing_stock_natfor                    / superAggregateX(data = dimSums(land_natfor, dim = "ac"),       aggr_type = "sum", level = level)
      growing_stock_plantations   <- growing_stock_main[, , "plantations"]   / superAggregateX(data = dimSums(land_plantations, dim = "ac"),  aggr_type = "sum", level = level)
      growing_stock_afforestation <- growing_stock_main[, , "afforestation"] / superAggregateX(data = dimSums(land_afforest, dim = "ac"),     aggr_type = "sum", level = level)
      growing_stock_primforest    <- growing_stock_main[, , "primforest"]    / superAggregateX(data = dimSums(land_primforest, dim = 3),      aggr_type = "sum", level = level)
      growing_stock_secdforest    <- growing_stock_main[, , "secdforest"]    / superAggregateX(data = dimSums(land_secdforest, dim = "ac"),   aggr_type = "sum", level = level)
      growing_stock_other         <- growing_stock_main[, , "other"]         / superAggregateX(data = dimSums(land_other, dim = "ac"),        aggr_type = "sum", level = level)
      growing_stock_main          <- mbind(growing_stock_plantations, growing_stock_afforestation, growing_stock_secdforest, growing_stock_primforest, growing_stock_other)
      a <- mbind(growing_stock_forest_total, growing_stock_natfor, growing_stock_main)
    }
  } else {
    message("ERROR - aggregation level not supported")
    a <- NULL
  }

  out(a, file)
}
