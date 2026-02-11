#' @title reportPBbiosphere
#' @description reports biosphere planetary boundary:
#'              Share of intact land relative to total land area (unitless)
#'              Share of intact land covered by areas within Global Safety Net (unitless)
#'              Share of land area that satisfies landscape target (unitless)
#' @export
#' @param gdx GDX file
#' @param level level of aggregation (regglo: regions and global)
#' @param intactnessRule rule for intact land can be based on percentage of potential carbon
#'                       density reached or on age classes for secondary forests,
#'                       planted forest and other natural land.
#'                       The argument is split into two components:
#'                       rule: carbon or ageclass
#'                       threshold: share of carbon density reached to be classified as intact
#'                                  or threshold in years can be set via this argument
#'                       Example: "carbon:0.95" or "ageclass:70"
#'
#' @return MAgPIE object
#' @author Patrick von Jeetze, Felicitas Beier
#' @import magclass
#' @importFrom mstools toolHoldConstantBeyondEnd
#' @examples
#' \dontrun{
#' x <- reportPBbiosphere(gdx)
#' }
#'
#' @section Biosphere planetary boundary variables:
#' Name | Unit | Meta
#' ---|---|---
#' Planetary Boundary\|Biosphere\|Share of intact land relative to total land area | unitless | Fraction of intact land (primary forest, mature secondary forest, other natural land)
#' Planetary Boundary\|Biosphere\|Share of intact land covered by areas within Global Safety Net | unitless | Share of intact land in priority conservation areas
#' Planetary Boundary\|Biosphere\|Share of land area that satisfies landscape target | unitless | Share of land where cropland does not exceed 80% of available cropland
#' @md

#'
reportPBbiosphere <- function(gdx, level = "regglo",
                              intactnessRule = "carbon:0.95") {

  # -------------------------------------------------
  # Largely intact land relative to total land
  # -------------------------------------------------

  # land area by land cover type
  land <- read.magpie(file.path(dirname(normalizePath(gdx)), "cell.land_0.5.mz"))
  if (length(getCells(land)) == "59199") {
    mapfile <- system.file("extdata", "mapping_grid_iso.rds", package = "magpie4")
    map_grid_iso <- readRDS(mapfile)
    land <- setCells(land, map_grid_iso$grid)
  }
  land <- land[, "y1985", , invert = TRUE]

  # extract age-class-specific areas of secondary forest, planted forest and other land:
  p35_secdforest <- collapseNames(readGDX(gdx, "p35_secdforest"))
  forestry       <- dimSums(readGDX(gdx, "ov32_land", "ov_land_fore", react = "silent",
                                    select = list(type = "level"))[, , c("aff", "ndc")],
                            dim = "type32")
  p35_land_other <- readGDX(gdx, c("p35_land_other", "p35_other"), format = "first_found")

  cmYrs <- intersect(getItems(p35_secdforest, dim = 2), getItems(land, dim = 2))

  # Depending on intactness rule, different areas are excluded as they are not yet intact
  if (strsplit(intactnessRule, split = ":")[[1]][1] == "ageclass") {

    # potential forest area
    if (file.exists(file.path(dirname(normalizePath(gdx)), "pot_forest_area_0.5.mz"))) {
      potForest <- read.magpie(file.path(dirname(normalizePath(gdx)), "pot_forest_area_0.5.mz"))
      potForest <- mstools::toolHoldConstantBeyondEnd(potForest)[, getItems(land, dim = 2), ]
      potForest[potForest != 0] <- 1
    } else {
      potForest <- new.magpie(cells_and_regions = getItems(land, dim = 1),
                              years = getItems(land, dim = 2),
                              names = NULL,
                              fill = 0)
      warning("For calculation of Biosphere Planetary Boundary file `pot_forest_area_0.5.mz` is required.
            This file exists from magpie v.4.8.2 onwards.
            For earlier versions, this warning can be ignored.
            For later versions, please ensure that the file is returned in files2export.")
    }

    # extract argument
    intactnessThreshold <- as.numeric(strsplit(intactnessRule, split = ":")[[1]][2])

    # other natural land consisting of young secondary forest and other land
    if (any(grepl("othernat", getItems(p35_land_other, dim = 3)))) {
      p35_land_other <- dimSums(p35_land_other, dim = "othertype35")
    }

    # age classes below certain threshold are classified as not yet intact
    ageclasses <- getItems(forestry, dim = "ac")
    ageclasses <- c(ageclasses[as.integer(gsub("ac", "", ageclasses[-length(ageclasses)])) >= intactnessThreshold], tail(ageclasses, 1))

    # calculate share of land classes that are intact
    shrIntactOther <- ifelse(dimSums(p35_land_other, dim = "ac") > 0,
                             dimSums(p35_land_other[, , ageclasses], dim = "ac") / dimSums(p35_land_other, dim = "ac"),
                             1)
    shrIntactSecdf <- ifelse(dimSums(p35_secdforest, dim = "ac") > 0,
                             dimSums(p35_secdforest[, , ageclasses], dim = "ac") / dimSums(p35_secdforest, dim = "ac"),
                             1)
    shrIntactForestry <- ifelse(dimSums(forestry, dim = "ac") > 0,
                                dimSums(forestry[, , ageclasses], dim = "ac") / dimSums(forestry, dim = "ac"),
                                1)

  } else if (strsplit(intactnessRule, split = ":")[[1]][1] == "carbon") {

    if (!any(grepl("othernat", getItems(p35_land_other, dim = 3)))) {
      stop("The carbon method of reportPBbiosphere can only be calculated for MAgPIE versions >4.8.0")
    }

    # extract argument
    intactnessThreshold <- as.numeric(strsplit(intactnessRule, split = ":")[[1]][2])

    # carbon density per age class in vegetation carbon
    pm_carbon_density_secdforest_ac <- readGDX(gdx, "pm_carbon_density_secdforest_ac")[, , "vegc"]
    pm_carbon_density_other_ac      <- readGDX(gdx, "pm_carbon_density_other_ac")[, , "vegc"]
    pm_carbon_density_plantation_ac <- readGDX(gdx, "pm_carbon_density_plantation_ac")[, , "vegc"]

    # if carbon density is below threshold, area is not classified as intact
    pm_carbon_density_secdforest_ac[pm_carbon_density_secdforest_ac <= intactnessThreshold * pm_carbon_density_secdforest_ac[, , "acx"]] <- 0
    pm_carbon_density_secdforest_ac[pm_carbon_density_secdforest_ac != 0] <- 1
    pm_carbon_density_other_ac[pm_carbon_density_other_ac <= intactnessThreshold * pm_carbon_density_other_ac[, , "acx"]] <- 0
    pm_carbon_density_other_ac[pm_carbon_density_other_ac != 0] <- 1
    pm_carbon_density_plantation_ac[pm_carbon_density_plantation_ac <= intactnessThreshold * pm_carbon_density_plantation_ac[, , "acx"]] <- 0
    pm_carbon_density_plantation_ac[pm_carbon_density_plantation_ac != 0] <- 1

    # other natural land without young secondary forest
    p35_land_other <- readGDX(gdx, "p35_land_other")
    ratioRealOther <- ifelse(dimSums(p35_land_other[, , "othernat"], dim = c("ac", "othertype35")) > 0,
                                 dimSums(p35_land_other[, , "othernat"], dim = "ac") / dimSums(p35_land_other[, , "othernat"], dim = c("ac", "othertype35")),
                             1)
    # disaggregate from cluster to 0.5 degree
    ratioRealOther <- gdxAggregate(gdx = gdx, x = ratioRealOther, weight = NULL,
                                   to = "grid", absolute = FALSE)
    p35_land_other <- dimSums(p35_land_other[, , "othernat"], dim = "othertype35")

    # calculate share of land classes that are intact
    shrIntactOther <- ifelse(dimSums(p35_land_other, dim = "ac") > 0,
                              dimSums(p35_land_other * pm_carbon_density_other_ac[, cmYrs, ], dim = "ac") / dimSums(p35_land_other, dim = "ac"),
                             1)
    shrIntactSecdf <- ifelse(dimSums(p35_secdforest, dim = "ac") > 0,
                              dimSums(p35_secdforest * pm_carbon_density_secdforest_ac[, cmYrs, ], dim = "ac") / dimSums(p35_secdforest, dim = "ac"),
                             1)
    shrIntactForestry <- ifelse(dimSums(forestry, dim = "ac") > 0,
                                 dimSums(forestry * pm_carbon_density_plantation_ac[, cmYrs, ], dim = "ac") / dimSums(forestry, dim = "ac"),
                                1)
  } else {
    stop("Please set intactnessRule argument in reportPBbiosphere.")
  }

  # disaggregate from cluster to 0.5 degree
  shrIntactOther <- gdxAggregate(gdx = gdx, x = shrIntactOther, weight = NULL,
                                 to = "grid", absolute = FALSE)
  shrIntactSecdf <- gdxAggregate(gdx = gdx, x = shrIntactSecdf, weight = NULL,
                                 to = "grid", absolute = FALSE)
  shrIntactForestry <- gdxAggregate(gdx = gdx, x = shrIntactForestry, weight = NULL,
                                    to = "grid", absolute = FALSE)

  # Largely intact land contains all of primary forest, parts of secondary forest,
  # parts of other natural land (and parts of planted forests (added below))
  if (strsplit(intactnessRule, split = ":")[[1]][1] == "ageclass") {
    intactLand <- collapseNames(land[, cmYrs, "primforest"] +
                                 land[, cmYrs, "secdforest"] * shrIntactSecdf[, cmYrs, ] +
                                 land[, cmYrs, "other"] * (1 - potForest[, cmYrs, ]) * shrIntactOther[, cmYrs, ])
  } else if (strsplit(intactnessRule, split = ":")[[1]][1] == "carbon") {
    intactLand <- collapseNames(land[, cmYrs, "primforest"] +
                                 land[, cmYrs, "secdforest"] * shrIntactSecdf[, cmYrs, ] +
                                 land[, cmYrs, "other"] * shrIntactOther[, cmYrs, ] * ratioRealOther[, cmYrs, ])
  }

  # total land area
  totLand <- setYears(dimSums(land[, "y1995", ], dim = 3), NULL)

  landSplit <- read.magpie(file.path(dirname(normalizePath(gdx)), "cell.land_split_0.5.mz"))
  if (length(getCells(landSplit)) == "59199") {
    mapfile <- system.file("extdata", "mapping_grid_iso.rds", package = "magpie4")
    map_grid_iso <- readRDS(mapfile)
    landSplit <- setCells(landSplit, map_grid_iso$grid)
  }
  landSplit <- landSplit[, "y1985", , invert = TRUE]

  plantation <- readGDX(gdx, "s32_aff_plantation")

  # If forestry realization is activated, parts of planted forest (NPiNDC and
  # CO2-price driven afforested area) are counted towards largely intact land
  if (plantation) {
    intactPlantedForest <- landSplit[, cmYrs, "PlantedForest_NPiNDC"] * shrIntactForestry[, cmYrs, ]
  } else {
    intactPlantedForest <- dimSums(landSplit[, cmYrs, c("PlantedForest_NPiNDC",
                                                   "PlantedForest_Afforestation")],
                                   dim = 3) * shrIntactForestry[, cmYrs, ]
  }

  intactLand <- collapseNames(intactLand + intactPlantedForest)

  x1 <- intactLand / totLand
  x1[is.na(x1)] <- 0

  if (!is.null(x1)) {
    if (level == "grid") {
      getNames(x1) <- "Planetary Boundary|Biosphere|Share of intact land relative to total land area (unitless)"
    } else {
      x1 <- gdxAggregate(gdx, x1, to = level, weight = totLand, absolute = FALSE)
      getNames(x1) <- "Planetary Boundary|Biosphere|Share of intact land relative to total land area (unitless)"
    }
    message("Finished calculating share of intact land relative to total land area")
  }
  # -------------------------------------------------
  # Intact land covered by the Global Safety Net
  # -------------------------------------------------

  consvPrio <- c(
    file.path(dirname(normalizePath(gdx)), "consv_prio_areas_0.5.mz"),
    "input/consv_prio_areas_0.5.mz",
    "modules/22_land_conservation/input/consv_prio_areas_0.5.mz",
    "../input/consv_prio_areas_0.5.mz",
    "../modules/22_land_conservation/input/consv_prio_areas_0.5.mz",
    "../../input/consv_prio_areas_0.5.mz",
    "../../modules/22_land_conservation/input/consv_prio_areas_0.5.mz"
  )
  consvPrio <- suppressWarnings(consvPrio[min(which(file.exists(consvPrio)))])
  if (!is.na(consvPrio)) {
    # Global saftey net areas
    areaGSN <- dimSums(read.magpie(consvPrio)[, , "GSN_HalfEarth"], dim = 3)

    x2 <- intactLand / areaGSN
    # share cannot be higher than 1
    x2[x2 > 1] <- 1
    x2[areaGSN == 0] <- 0

    if (!is.null(x2)) {
      if (level == "grid") {
        getNames(x2) <- "Planetary Boundary|Biosphere|Share of intact land covered by areas within Global Safety Net (unitless)"
      } else {
        x2 <- gdxAggregate(gdx, x2, to = level, weight = areaGSN, absolute = FALSE)
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

  avlCropland <- read.magpie(file.path(dirname(normalizePath(gdx)), "avl_cropland_0.5.mz")) # available cropland (at high resolution)
  cfg <- gms::loadConfig(file.path(dirname(normalizePath(gdx)), "config.yml"))
  marginalLand <- cfg$gms$c29_marginal_land # marginal land scenario
  avlCropland <- avlCropland[, , marginalLand]

  # The landscape boundary is defined by the available cropland.
  # Actual cropland cannot be larger than 80 % of the potential
  # cropland so that 20 % remains under (semi-)natural vegetation.
  landscapeBoundary <- avlCropland * (1 - 0.2)

  boundaryCheck <- collapseNames(land[, , "crop"]) / landscapeBoundary
  boundaryCheck <- round(boundaryCheck, 6)
  boundaryCheck[boundaryCheck < 1] <- 1
  boundaryCheck[boundaryCheck > 1] <- 0
  boundaryCheck[is.na(boundaryCheck)] <- 1

  x3 <- boundaryCheck
  x3[is.na(x3)] <- 0

  if (!is.null(x3)) {
    if (level == "grid") {
      getItems(x3, dim = 3) <- "Planetary Boundary|Biosphere|Share of land area that satisfies landscape target (unitless)"
    } else {
      x3 <- gdxAggregate(gdx, x3, to = level, weight = totLand, absolute = FALSE)
      getItems(x3, dim = 3) <- "Planetary Boundary|Biosphere|Share of land area that satisfies landscape target (unitless)"
    }
    message("Finished calculating share of land area that satisfies landscape target (unitless)")
  }

  out <- mbind(x1[, cmYrs, ], x2[, cmYrs, ], x3[, cmYrs, ])
  return(out)
}
