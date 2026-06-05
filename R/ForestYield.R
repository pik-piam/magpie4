#' @title ForestYield
#' @description Calculates the average growing stock density of harvested forest areas (m3/ha).
#' This is the ratio of timber production volume to harvested area, i.e. the average standing
#' volume removed per hectare harvested. Values are comparable to growing stock (typically
#' 100-300 m3/ha) and should NOT be confused with forestry yield in the MAI sense (5-20 m3/ha/yr).
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @details Average growing stock of harvested areas = timber production (m3) / harvested area (ha).
#' Differences to overall growing stock arise from selective harvesting of specific age classes.
#' @return Average growing stock density of harvested areas in m3/ha
#' @author Abhijeet Mishra, Florian Humpenoeder
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @examples
#'
#'   \dontrun{
#'     x <- ForestYield(gdx)
#'   }


ForestYield <- function(gdx, file = NULL, level = "cell") {
  a <- NULL

  if (!is.null(readGDX(gdx, "s32_hvarea")) && !is.null(readGDX(gdx, "s35_hvarea"))) {
    ov73_prod_forestry <- readGDX(gdx, "ov_prod_forestry", "ov73_prod_forestry", select = list(type = "level"), format = "first_found")
    ov73_prod_forestry <- add_dimension(ov73_prod_forestry, add = "land", nm = "Forestry")

    ov73_prod_natveg <- readGDX(gdx, "ov_prod_natveg", "ov73_prod_natveg", select = list(type = "level"), react = "silent")
    names(dimnames(ov73_prod_natveg))[3] <- "land.kforestry"
    getNames(ov73_prod_natveg, dim = 1) <- c("Primary forest", "Secondary forest", "Other land")

    ov73_prod <- mbind(ov73_prod_forestry, ov73_prod_natveg)
    ov73_prod_total <- dimSums(ov73_prod, dim = "land")
    ov73_prod_total <- add_dimension(ov73_prod_total, add = "land", nm = "Total")
    ov73_prod <- mbind(ov73_prod, ov73_prod_total)

    #### unit conversion: tDM to m3
    density <- readGDX(gdx, "im_vol_conv", "f73_volumetric_conversion", react = "silent", format = "first_found")
    ov73_prod <- ov73_prod / density

    #### Aggregate
    ov73_prod <- dimSums(ov73_prod, dim = "kforestry")
    ov73_prod <- gdxAggregate(gdx, ov73_prod, to = level)

    #### Yield calculations
    ov73_hvarea <- harvested_area_timber(gdx, level = level, annualized = TRUE)
    yield <- ov73_prod / ov73_hvarea
    yield[is.na(yield) | is.infinite(yield)] <- 0

    a <- yield

  } else {
    message("Disabled (no dynamic forestry) ", appendLF = FALSE)
  }

  out(a, file)
}
