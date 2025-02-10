#' @title ForestYield
#' @description calculates timber yield out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @details Forest yield for timber production
#' @return Forest yield for timber production in m3 per ha per year
#' @author Abhijeet Mishra, Florian Humpenoeder
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @importFrom luscale superAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- ForestYield(gdx)
#'   }

ForestYield <- function(gdx, file=NULL, level="cell"){
  a <- NULL

  if(as.numeric(readGDX(gdx, "s32_hvarea")) > 0 & as.numeric(readGDX(gdx, "s35_hvarea")) > 0) {

    #### get annual production and annual harvested area
    ov73_prod_forestry <- setNames(dimSums(readGDX(gdx,"ov_prod_forestry","ov73_prod_forestry",
                                                   select = list(type="level"), format = "first_found"),dim=3), "Forestry")
    ov73_prod_natveg <- dimSums(readGDX(gdx,"ov_prod_natveg","ov73_prod_natveg",select = list(type="level"), react = "silent"),dim="kforestry")
    ov73_prod_natveg <- setNames(ov73_prod_natveg,c("Primary forest","Secondary forest","Other land"))
    ov73_prod <- mbind(ov73_prod_forestry, ov73_prod_natveg)
    ov73_prod <- gdxAggregate(gdx, ov73_prod, to = level)
    ov73_prod <- mbind(ov73_prod, setNames(dimSums(ov73_prod, dim=3),"Total"))

    ov73_hvarea <- harvested_area_timber(gdx, level = level)

    #### unit conversion
    f73_volumetric_conversion <- readGDX(gdx,"f73_volumetric_conversion")
    f73_volumetric_conversion <- add_columns(x = f73_volumetric_conversion, addnm = "constr_wood")
    f73_volumetric_conversion[,,"constr_wood"] <- f73_volumetric_conversion[,,"wood"]
    ov73_prod <- ov73_prod / f73_volumetric_conversion

    #### Yield calculations
    yield <- ov73_prod / ov73_hvarea
    if(any(is.na(range(yield)))){
      yield[is.na(yield)] <- 0
    }

    if(any(is.infinite(range(yield)))){
      div0 <- where(ov73_prod != 0 & ov73_hvarea == 0)$true$regions
      yield[is.infinite(yield)] <- 0
    }

    a <- yield

  } else {message("Disabled (no dynamic forestry) ", appendLF = FALSE)}

  out(a,file)
}
