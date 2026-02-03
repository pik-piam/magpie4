#' @title reportBII
#' @description reports biodiversity intactness index
#'
#' @export
#'
#' @param gdx GDX file
#' @param level An aggregation level for the spatial dimension. Can be any level
#' available via superAggregateX.
#' @return Biodiversity intactness index as MAgPIE object
#' @author Patrick v. Jeetze, Florian Humpenoeder
#' @examples
#' \dontrun{
#' x <- reportBII(gdx)
#' }
#'
#' @section Biodiversity Intactness Index variables:
#' Name | Unit | Meta
#' ---|---|---
#' Biodiversity\|BII | unitless | Terrestrial biodiversity measured with Biodiversity Intactness Index (BII). The BII summarises the change in ecological communities in response to human pressures. It is an estimated percentage of the original number of species that remain and their abundance in any given area.
#' Biodiversity\|Cropland Landscapes BII | unitless | Terrestrial biodiversity in landscapes containing cropland measured with Biodiversity Intactness Index (BII)
#' Biodiversity\|BII in 30x30 Landscapes | unitless | Terrestrial biodiversity in 30 by 30 conservation target landscapes measured with Biodiversity Intactness Index (BII)
#' Biodiversity\|Biodiversity Hotspot BII | unitless | Terrestrial biodiversity in biodiversity hotspot landscapes measured with Biodiversity Intactness Index (BII)
#' Biodiversity\|Biodiversity Hotspot and Intact Forest Landscapes BII | unitless | Terrestrial biodiversity in biodiversity hotspot and intact forest landscapes measured with Biodiversity Intactness Index (BII)
#' Biodiversity\|BII in areas outside Biodiversity Hotspots, Intact Forest & Cropland Landscapes | unitless | Terrestrial biodiversity in areas outside biodiversity hotspot, intact forest and cropland landscapes measured with Biodiversity Intactness Index (BII)
#' Biodiversity\|Key Biodiversity Area BII | unitless | Terrestrial biodiversity in key biodiversity areas measured with Biodiversity Intactness Index (BII)
#' @md

#'
reportBII <- function(gdx, level = "regglo") {
  out <- NULL

  # ==========================================================
  # Global BII
  # ==========================================================
  x1 <- BII(gdx, level = level)
  if (!is.null(x1)) {
    getNames(x1) <- "Biodiversity|BII (unitless)"
    message("Finished calculating global BII (unitless)")
  } else {
    cat("No BII reporting possible")
  }
  out <- mbind(out, x1)

  # ==========================================================
  # BII in cropland landscapes
  # ==========================================================
  cropland <- land(gdx = gdx, level = "grid", types = "crop")
  # Set minuscule values of cropland (< 10 ha per grid cell) to zero
  cropland[cropland < 0.0001] <- 0
  x2 <- BII(gdx,
    level = level, mode = "from_grid",
    adjusted = TRUE, spatialWeight = cropland
  )
  if (!is.null(x2)) {
    getNames(x2) <- "Biodiversity|Cropland Landscapes BII (unitless)"
    message("Finished calculating Cropland Landscapes BII (unitless)")
  } else {
    cat("No cropland landscapes BII reporting possible")
  }
  out <- mbind(out, x2)

  # ==========================================================
  # BII in Conservation priority areas
  # ==========================================================
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
    consvPrioData <- read.magpie(consvPrio)
    BHArea <- dimSums(consvPrioData[, , "BH"], dim = 3)
    BHIFLArea <- dimSums(consvPrioData[, , "BH_IFL"], dim = 3)
    thirtyArea <- dimSums(consvPrioData[, , "30by30"], dim = 3)
    KBAarea <- dimSums(consvPrioData[, , "KBA"], dim = 3)


    # -----------------------------------
    # BII in 30 by 30 Areas
    # -----------------------------------
    x3 <- BII(gdx,
      level = level, mode = "from_grid",
      adjusted = TRUE, spatialWeight = thirtyArea
    )
    if (!is.null(x3)) {
      getNames(x3) <- "Biodiversity|BII in 30x30 Landscapes (unitless)"
      message("Finished calculating BII in 30x30 Landscapes (unitless)")
    } else {
      cat("No BII in 30x30 Landscapes reporting possible")
    }
    # -----------------------------------
    # BII in Biodiversity Hotspots
    # -----------------------------------
    x4 <- BII(gdx,
      level = level, mode = "from_grid",
      adjusted = TRUE, spatialWeight = BHArea
    )
    if (!is.null(x4)) {
      getNames(x4) <- "Biodiversity|Biodiversity Hotspot BII (unitless)"
      message("Finished calculating Biodiversity Hotspot BII (unitless)")
    } else {
      cat("No Biodiversity Hotspot BII reporting possible")
    }

    # -----------------------------------------------------------
    # BII in Biodiversity Hotspots & Intact Forest Landscapes
    # -----------------------------------------------------------
    x5 <- BII(gdx,
      level = level, mode = "from_grid",
      adjusted = TRUE, spatialWeight = BHIFLArea
    )
    if (!is.null(x5)) {
      getNames(x5) <- "Biodiversity|Biodiversity Hotspot and Intact Forest Landscapes BII (unitless)"
      message("Finished calculating Biodiversity Hotspot and Intact Forest Landscapes BII (unitless)")
    } else {
      cat("No Biodiversity Hotspot and Intact Forest Landscapes BII reporting possible")
    }

    # ---------------------------------------------------------------------------------------------
    # BII in areas outside cropland landscapes, Biodiversity Hotspots & Intact Forest Landscapes
    # ---------------------------------------------------------------------------------------------
    landArea <- dimSums(land(gdx = gdx, level = "grid", types = NULL), dim = 3)
    diffLand <- landArea - BHIFLArea - cropland
    diffLand[diffLand < 0] <- 0

    x6 <- BII(gdx,
      level = level, mode = "from_grid",
      adjusted = TRUE, spatialWeight = diffLand
    )
    if (!is.null(x6)) {
      getNames(x6) <- paste(
        "Biodiversity|BII in areas outside Biodiversity Hotspots,",
        "Intact Forest & Cropland Landscapes (unitless)"
      )
      message(paste(
        "Finished calculating",
        "Biodiversity|BII in areas outside Biodiversity Hotspots,",
        "Intact Forest & Cropland Landscapes (unitless)"
      ))
    } else {
      cat("No BII reporting possible in areas outside Biodiversity Hotspots, Intact Forest & Cropland Landscapes")
    }

    # -----------------------------------
    # BII in Key Biodiversity Areas
    # -----------------------------------
    x7 <- BII(gdx,
      level = level, mode = "from_grid",
      adjusted = TRUE, spatialWeight = KBAarea
    )
    if (!is.null(x7)) {
      getNames(x7) <- "Biodiversity|Key Biodiversity Area BII (unitless)"
      message("Finished calculating Key Biodiversity Area BII (unitless)")
    } else {
      cat("No Key Biodiversity Area BII reporting possible")
    }

    out <- mbind(out, x3, x4, x5, x6, x7)
  } else {
    cat("No BII reporting in conservation priority areas possible")
  }

  return(out)
}
