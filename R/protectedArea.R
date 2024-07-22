#' @title protectedArea
#' @description reads protectedArea out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "grid", "iso, "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param sum sum over land pools (default = FALSE)
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds) for disaggregation
#' @details protected areas in primforest, secdforest and other land
#' @return protected area in Mha
#' @author Florian Humpenoeder, Patrick v. Jeetze
#' @importFrom madrat toolAggregate
#' @importFrom magclass dimSums mbind getNames setNames getCells getYears new.magpie
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- protectedArea(gdx)
#' }
#'
protectedArea <- function(gdx, file = NULL, level = "cell", sum = FALSE, dir = "."){

  # read in protected areas
  if (level %in% c("grid","iso")) {
    a <- read.magpie(file.path(dir, "cell.conservation_land_0.5.mz"))
     if (length(getCells(yields)) == "59199") {
      mapfile <- system.file("extdata", "mapping_grid_iso.rds", package="magpie4")
      map_grid_iso <- readRDS(mapfile)
      yields <- setCells(yields, map_grid_iso$grid)
    }

    if(level == "iso") a <- gdxAggregate(gdx, a , to = "iso", dir = dir)
  } else {
    a <- readGDX(gdx, "p22_conservation_area", react = "silent")

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
    a <- gdxAggregate(gdx, a, to = level, absolute = TRUE, dir = dir)
  }

  names(dimnames(a))[1] <- "j"

  # sum
  if (sum) a <- dimSums(a, dim = 3.1)


  out(a, file)
}
