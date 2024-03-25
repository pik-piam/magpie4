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
  x1 <- BII(gdx, level = "regglo")
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
  cropland <- land(gdx = gdx, level = "grid", types = "crop", dir = dir)
  # Set minuscule values of cropland (< 10 ha per grid cell) to zero
  cropland[cropland < 0.0001] <- 0
  x2 <- BII(gdx,
    level = "regglo", mode = "from_grid",
    adjusted = TRUE, spatialWeight = cropland, dir = dir
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
    thirtyArea <- dimSums(read.magpie(consvPrio)[, , "30by30"], dim = 3)
    KBAarea <- dimSums(read.magpie(consvPrio)[, , "KBA"], dim = 3)


    # -----------------------------------
    # BII in 30 by 30 Areas
    # -----------------------------------
    x3 <- BII(gdx,
      level = "regglo", mode = "from_grid",
      adjusted = TRUE, spatialWeight = thirtyArea, dir = dir
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
      level = "regglo", mode = "from_grid",
      adjusted = TRUE, spatialWeight = BHArea, dir = dir
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
      level = "regglo", mode = "from_grid",
      adjusted = TRUE, spatialWeight = BHIFLArea, dir = dir
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
    landArea <- dimSums(land(gdx = gdx, level = "grid", types = NULL, dir = dir), dim = 3)
    diffLand <- landArea - BHIFLArea - cropland
    diffLand[diffLand < 0] <- 0

    x6 <- BII(gdx,
      level = "regglo", mode = "from_grid",
      adjusted = TRUE, spatialWeight = diffLand, dir = dir
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
      level = "regglo", mode = "from_grid",
      adjusted = TRUE, spatialWeight = KBAarea, dir = dir
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
