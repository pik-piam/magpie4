#' @title landConservation
#' @description reads land conservation information out of a MAgPIE gdx file.
#' Land restoration \code{'restore'} is reported in Mha/yr by default but can be
#' also reported both over the time step length and cumulatively.
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "grid", "iso, "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param sum sum over land pools (default = FALSE)
#' @param cumuRestor Logical; Whether function should report cumulative land restoration.
#' @param baseyear Base year used for cumulative land restoration reporting (default = 1995)
#' @param annualRestor Logical; Whether function should report annual land restoration.
#' @details protected areas in primforest, secdforest and other land
#' @return protected area in Mha
#' @author Florian Humpenoeder, Patrick v. Jeetze
#' @importFrom madrat toolAggregate
#' @importFrom magclass dimSums mbind getNames setNames getCells getYears new.magpie
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- landConservation(gdx)
#' }
#'
landConservation <- function(gdx, file = NULL, level = "cell",
                             cumuRestor = FALSE, baseyear = 1995,
                             annualRestor = FALSE, sum = FALSE) {
  if (cumuRestor && annualRestor) {
    stop("cumuRestor and annualRestor cannot both be TRUE.")
  }

  # read in protected areas
  if (level %in% c("grid", "iso")) {
    a <- read.magpie(file.path(dirname(normalizePath(gdx)), "cell.conservation_land_0.5.mz"))
    if (length(getCells(yields)) == "59199") {
      mapfile <- system.file("extdata", "mapping_grid_iso.rds", package = "magpie4")
      map_grid_iso <- readRDS(mapfile)
      yields <- setCells(yields, map_grid_iso$grid)
    }

    if (level == "iso") a <- gdxAggregate(gdx, a, to = "iso")
  } else {
    a <- readGDX(gdx, "pm_land_conservation", react = "silent")

    if (is.null(a)) {
      landTypes <- readGDX(gdx, "land", react = "silent", type = "Set")
      consvTypes <- c("protect", "restore")
      allNames <- c(outer(landTypes, consvTypes, FUN = paste, sep = "."))
      a <- new.magpie(getCells(a), getYears(a), all, fill = 0)

      saveNatveg <- readGDX(gdx, "p35_save_natveg", react = "silent")
      if (!is.null(saveNatveg)) {
        a[, , c(
          "primforest.protect",
          "secdforest.protect",
          "other.protect"
        )] <- setNames(saveNatveg, NULL)
      }
    }

    if (is.null(a)) {
      primforest <- setNames(readGDX(gdx, "p35_save_primforest", react = "silent"), "primforest")
      secdforest <- setNames(readGDX(gdx, "p35_save_secdforest", react = "silent"), "secdforest")
      other <- setNames(readGDX(gdx, "p35_save_other", react = "silent"), "other")
      saveNatveg <- mbind(
        primforest,
        secdforest,
        other
      )
      a[, , c(
        "primforest.protect",
        "secdforest.protect",
        "other.protect"
      )] <- setNames(saveNatveg, NULL)
    }
    a <- gdxAggregate(gdx, a, to = level, absolute = TRUE)
  }

  names(dimnames(a))[1] <- "j"

  # --- cumulative restoration?
  if (cumuRestor) {
    a[, "y1995", "restore"] <- 0
    a <- as.magpie(apply(a[, , "restore"], c(1, 3), cumsum))
    a <- a - setYears(a[, baseyear, ], NULL)
    a[a < 0] <- 0
  }

  # --- annual restoration?
  if (annualRestor) {
    years <- m_yeardiff(gdx)
    a[, , "restore"] <- a[, , "restore"] / years
  }

  # sum
  if (sum) a <- dimSums(a, dim = 3.1)

  out(a, file)
}
