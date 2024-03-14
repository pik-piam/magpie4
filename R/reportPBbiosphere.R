#' @title reportPBbiosphere
#' @description reports biosphere planetary boundary:
#'              Share of intact land relative to total land area (unitless)
#'              Share of intact land covered by areas within Global Safety Net (unitless)
#'              Share of land area that satisfies landscape target (unitless)
#' @export
#' @param gdx GDX file
#' @param level level of aggregation (regglo: regions and global)
#' @param dir directory with required spatial data
#'
#' @return MAgPIE object
#' @author Patrick von Jeetze, Felicitas Beier
#' @import magclass
#' @examples
#' \dontrun{
#' x <- reportPBbiosphere(gdx)
#' }
#'
reportPBbiosphere <- function(gdx, level = "regglo", dir = ".") {
  # -------------------------------------------------
  # Intact land relative to total land
  # -------------------------------------------------

  land <- read.magpie(file.path(dir, "cell.land_0.5.mz"))
  if (length(getCells(land)) == "59199") {
    mapfile <- system.file("extdata", "mapping_grid_iso.rds", package = "magpie4")
    map_grid_iso <- readRDS(mapfile)
    land <- setCells(landSplit, map_grid_iso$grid)
  }
  land <- land[, "y1985", , invert = TRUE]
  intactLand <- dimSums(
    land[, , c("primforest", "secdforest", "other")],
    dim = 3
  )
  totLand <- setYears(dimSums(land[, "y1995", ], dim = 3), NULL)

  landSplit <- read.magpie(file.path(dir, "cell.land_split_0.5.mz"))
  if (length(getCells(landSplit)) == "59199") {
    mapfile <- system.file("extdata", "mapping_grid_iso.rds", package = "magpie4")
    map_grid_iso <- readRDS(mapfile)
    landSplit <- setCells(landSplit, map_grid_iso$grid)
  }
  landSplit <- landSplit[, "y1985", , invert = TRUE]

  plantation <- readGDX(gdx, "s32_aff_plantation")

  if (plantation) {
    intactPlantedForest <- landSplit[, , "PlantedForest_NPiNDC"]
  } else {
    intactPlantedForest <- dimSums(
      landSplit[, , c("PlantedForest_NPiNDC", "PlantedForest_Afforestation")],
      dim = 3
    )
  }

  intactLand <- intactLand + intactPlantedForest

  x1 <- intactLand / totLand
  x1[is.na(x1)] <- 0

  if (!is.null(x1)) {
    if (level == "grid") {
      getNames(x1) <- "Planetary Boundary|Biosphere|Share of intact land relative to total land area (unitless)"
    } else {
      x1 <- gdxAggregate(gdx, x1, to = level, weight = totLand, absolute = FALSE, dir = dir)
      getNames(x1) <- "Planetary Boundary|Biosphere|Share of intact land covered by areas within Global Safety Net (unitless)"
    }
    message("Finished calculating share of intact land relative to total land area")
  }
  # -------------------------------------------------
  # Intact land covered by the Global Safety Net
  # -------------------------------------------------

  consvPrio <- c(
    file.path(dir, "consv_prio_areas_0.5.mz"),
    "input/consv_prio_areas_0.5.mz",
    "modules/22_land_conservation/input/consv_prio_areas_0.5.mz",
    "../input/consv_prio_areas_0.5.mz",
    "../modules/22_land_conservation/input/consv_prio_areas_0.5.mz",
    "../../input/consv_prio_areas_0.5.mz",
    "../../modules/22_land_conservation/input/consv_prio_areas_0.5.mz"
  )
  consvPrio <- suppressWarnings(consvPrio[min(which(file.exists(consvPrio)))])
  if (!is.na(consvPrio)) {
    GSNArea <- dimSums(read.magpie(consvPrio)[, , "GSN_HalfEarth"], dim = 3)


    x2 <- intactLand / GSNArea
    # share cannot be higher than 1
    x2[x2 > 1] <- 1
    x2[GSNArea == 0] <- 0


    if (!is.null(x2)) {
      if (level == "grid") {
        getNames(x2) <- "Planetary Boundary|Biosphere|Share of intact land covered by areas within Global Safety Net (unitless)"
      } else {
        x2 <- gdxAggregate(gdx, x2, to = level, weight = GSNArea, absolute = FALSE, dir = dir)
        getNames(x2) <- "Planetary Boundary|Biosphere|Share of intact land covered by areas within Global Safety Net (unitless)"
      }
      message("Finished calculating share of intact land covered by areas within Global Safety Net")
    }
  } else {
    cat("No reporting of share of intact land covered by areas within Global Safety Net possible")
  }


  # -------------------------------------------------
  # Landscape target
  # -------------------------------------------------

  avlCropland <- read.magpie(file.path(dir, "avl_cropland_0.5.mz")) # available cropland (at high resolution)
  cfg <- gms::loadConfig(file.path(dir, "config.yml"))
  marginalLand <- cfg$gms$c30_marginal_land # marginal land scenario
  avlCropland <- avlCropland[, , marginalLand]

  # The landscape boundary is defines by the available cropland.
  # Actual cropland cannot be larger than 80 % of the potential
  # cropland so that 20 % remains under (semi-)natural vegetation.
  landscapeBoundary <- avlCropland * (1 - 0.2)

  boundaryCheck <- land[, , "crop"] / landscapeBoundary
  boundaryCheck[boundaryCheck < 1] <- 1
  boundaryCheck[boundaryCheck > 1] <- 0
  boundaryCheck[is.na(boundaryCheck)] <- 1

  x3 <- totLand * boundaryCheck / totLand
  x3[is.na(x3)] <- 0

  if (!is.null(x3)) {
    if (level == "grid") {
      getNames(x3) <- "Planetary Boundary|Biosphere|Share of land area that satisfies landscape target (unitless)"
    } else {
      x3 <- gdxAggregate(gdx, x3, to = level, weight = totLand, absolute = FALSE, dir = dir)
      getNames(x3) <- "Planetary Boundary|Biosphere|Share of land area that satisfies landscape target (unitless)"
    }
    message("Finished calculating share of land area that satisfies landscape target (unitless)")
  }
  out <- mbind(x1, x2, x3)
  return(out)
}
