#' @title protectedArea
#' @description reads protectedArea out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "grid, "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param sum sum over land pools (default = FALSE)
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @details protected areas in primforest, secdforest and other land
#' @return protected area in Mha
#' @author Florian Humpenoeder, Patrick v. Jeetze
#' @importFrom gdx readGDX out
#' @importFrom magclass dimSums mbind getNames getCells new.magpie
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- protectedArea(gdx)
#' }
#'
protectedArea <- function(gdx, file = NULL, level = "cell", sum = FALSE, dir = ".") {
  map_file <- Sys.glob(file.path(dir, "clustermap_*.rds"))
  mapping <- readRDS(map_file)

  # read in protected areas

  a <- readGDX(gdx, "pm_land_conservation", react = "silent")
  # sum protection and restoration area
  if (!is.null(a)) {
    a <- dimSums(a, dim = 3.2)
  }

  if (is.null(a)) {
    a <- readGDX(gdx, "p35_save_natveg", react = "silent")
    if (!is.null(a)) {
      a <- mbind(a, new.magpie(getCells(a), getYears(a), c("crop", "past", "forestry", "urban"), fill = 0))
    }
  }

  if (is.null(a)) {
    primforest <- setNames(readGDX(gdx, "p35_save_primforest", react = "silent"), "primforest")
    secdforest <- setNames(readGDX(gdx, "p35_save_secdforest", react = "silent"), "secdforest")
    other <- setNames(readGDX(gdx, "p35_save_other", react = "silent"), "other")
    a <- mbind(
      primforest,
      secdforest,
      other
    )
    a <- mbind(a, new.magpie(getCells(a), getYears(a), c("crop", "past", "forestry", "urban"), fill = 0))
  }

  names(dimnames(a))[1] <- "j"

  # sum
  if (sum) a <- dimSums(a, dim = 3.1)

  # aggregate over regions
  if (level != "cell" & level != "grid") {
    a <- superAggregate(a, aggr_type = "sum", level = level, na.rm = FALSE)
  } # disaggregate to grid level
  else if (level == "grid") {
    # protected area as share of respective land type
    b <- land(gdx, level = "cell")[, , getNames(a)]
    shr <- a / b
    shr[is.nan(shr)] <- 0
    shr[shr > 1] <- 1
    # downscale share from cluster to grid level
    shr <- toolAggregate(shr, mapping, to = "cell")
    # multiply with grid level land type area
    x <- land(gdx, level = "grid", dir = dir)[, , getNames(a)]
    a <- shr * x
  }

  out(a, file)
}
