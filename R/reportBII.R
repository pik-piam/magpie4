#' @title reportBII
#' @description reports biodiversity intactness index
#'
#' @export
#'
#' @param gdx GDX file
#' @param dir magpie output directory that contains gridded BII data
#' @return Biodiversity intactness index as MAgPIE object
#' @author Patrick v. Jeetze, Florian Humpenoeder
#' @examples
#' \dontrun{
#' x <- reportBII(gdx)
#' }
#'
reportBII <- function(gdx, dir = ".") {
  out <- NULL

  # ==========================================================
  # Global BII
  # ==========================================================
  x <- BII(gdx, level = "regglo")
  if (!is.null(x)) {
    getNames(x) <- "Biodiversity|BII (unitless)"
    message("Finished calculating global BII (unitless)")
  } else {
    cat("No BII reporting possible")
  }
  out <- mbind(out, x)

  # ==========================================================
  # BII in cropland landscapes
  # ==========================================================
  cropland <- land(gdx = gdx, level = "grid", types = "crop", dir = dir)
  # Set minuscule values of cropland (< 10 ha per grid cell) to zero
  cropland[cropland < 0.0001] <- 0
  x <- BII(gdx,
    level = "regglo", mode = "from_grid",
    adjusted = TRUE, spatialWeight = cropland, dir = dir
  )
  if (!is.null(x)) {
    getNames(x) <- "Biodiversity|Cropland Landscapes BII (unitless)"
    message("Finished calculating Cropland Landscapes BII (unitless)")
  } else {
    cat("No cropland landscapes BII reporting possible")
  }
  out <- mbind(out, x)

  # ==========================================================
  # BII in Biodiversity Hotspots & Intact Forest Landscapes
  # ==========================================================
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
    BHArea <- dimSums(read.magpie(consvPrio)[, , "BH"], dim = 3)
    BHIFLArea <- dimSums(read.magpie(consvPrio)[, , "BH_IFL"], dim = 3)

    # -----------------------------------
    # BII in Biodiversity Hotspots
    # -----------------------------------
    x1 <- BII(gdx,
      level = "regglo", mode = "from_grid",
      adjusted = TRUE, spatialWeight = BHArea, dir = dir
    )
    if (!is.null(x1)) {
      getNames(x1) <- "Biodiversity|Biodiversity Hotspot BII (unitless)"
      message("Finished calculating Biodiversity Hotspot BII (unitless)")
    } else {
      cat("No Biodiversity Hotspot BII reporting possible")
    }

    # -----------------------------------------------------------
    # BII in Biodiversity Hotspots & Intact Forest Landscapes
    # -----------------------------------------------------------
    x2 <- BII(gdx,
      level = "regglo", mode = "from_grid",
      adjusted = TRUE, spatialWeight = BHIFLArea, dir = dir
    )
    if (!is.null(x2)) {
      getNames(x2) <- "Biodiversity|Biodiversity Hotspot and Intact Forest Landscapes BII (unitless)"
      message("Finished calculating Biodiversity Hotspot and Intact Forest Landscapes BII (unitless)")
    } else {
      cat("No Biodiversity Hotspot and Intact Forest Landscapes BII reporting possible")
    }

    # ---------------------------------------------------------------------------------------------
    # BII in areas outside cropland landscapes, Biodiversity Hotspots & Intact Forest Landscapes
    # ---------------------------------------------------------------------------------------------
    landArea <- dimSums(land(gdx = gdx, level = "grid", types = NULL, dir = dir), dim = 3)
    diffLand <- landArea - BHIFLArea - cropland
    diffLand[diffLand < 0] <- 0

    x3 <- BII(gdx,
      level = "regglo", mode = "from_grid",
      adjusted = TRUE, spatialWeight = diffLand, dir = dir
    )
    if (!is.null(x3)) {
      getNames(x3) <- paste(
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

    out <- mbind(out, x1, x2, x3)
  } else {
    cat("No Biodiversity Hotspot and Intact Forest Landscapes BII reporting possible")
  }

  # ==========================================================
  # BII in Key Biodiversity Areas
  # ==========================================================
  KBAarea <- c(
    file.path(dir, "kba_land_0.5.mz"),
    "input/kba_land_0.5.mz",
    "../input/kba_land_0.5.mz",
    "../../input/kba_land_0.5.mz"
  )
  KBAarea <- suppressWarnings(KBAarea[min(which(file.exists(KBAarea)))])
  if (!is.na(KBAarea)) {
    KBAarea <- read.magpie(KBAarea)

    x <- BII(gdx,
      level = "regglo", mode = "from_grid",
      adjusted = TRUE, spatialWeight = KBAarea, dir = dir
    )
    if (!is.null(x)) {
      getNames(x) <- "Biodiversity|Key Biodiversity Area BII (unitless)"
      message("Finished calculating Key Biodiversity Area BII (unitless)")
    } else {
      cat("No Key Biodiversity Area BII reporting possible")
    }
    out <- mbind(out, x)
  } else {
    cat("No Key Biodiversity Area BII reporting possible")
  }

  return(out)
}
