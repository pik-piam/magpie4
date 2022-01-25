#' @title Biodiversity intactness index
#' @description calculates the area weighted biodiversity intactness index (BII) out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx         GDX file
#' @param file        a file name the output should be written to using write.magpie
#' @param level       level of regional aggregation; "cell" (magpie cluster level),
#'                    "reg" (regional), "glo" (global), "regglo" (regional and global).
#' @param mode        "auto" (default), "MAgPIE" or "postprocessing".
#'                    "MAgPIE" reports the BV based on values from the MAgPIE biodiversity module.
#'                    "postprocessing" calculates the BV based on land information from MAgPIE
#'                    (for versions where biodiversity module was not available yet).
#'                    "auto" uses "MAgPIE" if available and falls back to "postprocessing" otherwise.
#' @param landClass   "all" returns average BII values for all land classes of ov_bv,
#'                    "sum" returns the weighted BII over all land classes of ov44_bv_weighted.
#' @param bii_coeff   file containing BII coefficients. Only needed for mode = "postprocessing". NULL tries to automatically detected the file.
#' @param side_layers file containing LUH2 side layers.
#'                    NULL tries to automatically detected the file.
#' @details Calculates global, regional and cluster-level biodiversity intactness index (BII)
#' @return Biodiversity intactness index (unitless)
#' @author Patrick v. Jeetze, Florian Humpenoeder, Felicitas Beier
#' @importFrom gdx readGDX out
#' @importFrom magclass dimSums
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- BII(gdx)
#' }
#'
BII <- function(gdx, file = NULL, level = "glo", mode = "auto", landClass = "sum",
                bii_coeff = NULL, side_layers = NULL) {

  if (mode == "auto") {

    # read in "biodiversity value" for different land cover classes (unweighted) (in Mha)
    ov_bv <- readGDX(gdx, "ov_bv", select = list(type = "level"), react = "silent")
    if (is.null(ov_bv)) {
      ov_bv <- readGDX(gdx, "ov44_bii", select = list(type = "level"), react = "silent")
    }

    if (!is.null(ov_bv)) {
      mode <- "MAgPIE"
    } else if (all(is.null(bii_coeff), is.null(side_layers))) {

      bii_coeff   <- c("input/f44_bii_coeff.cs3", "modules/44_biodiversity/bii_btc_apr20/input/f44_bii_coeff.cs3", "modules/44_biodiversity/bv_btc_mar21/input/f44_bii_coeff.cs3")
      side_layers <- c("input/luh2_side_layers_c200.mz", "modules/44_biodiversity/bii_btc_apr20/input/luh2_side_layers.cs3", "modules/10_land/input/luh2_side_layers.cs3")
      bii_coeff   <- suppressWarnings(bii_coeff[min(which(file.exists(bii_coeff)))])
      side_layers <- suppressWarnings(side_layers[min(which(file.exists(side_layers)))])
      ov32_land   <- readGDX(gdx, "ov32_land", "ov_land_fore", select = list(type = "level"), react = "silent")

      if (!is.null(ov32_land)) {
        if (names(dimnames(ov32_land))[3] == "type32.ac") ac <- TRUE else ac <- FALSE
      } else {
        ac <- FALSE
      }

      if (all(!is.na(bii_coeff), !is.na(side_layers), ac)) {
        mode <- "postprocessing"
      } else {
        mode <- "off"
      }
    } else {
      stop("In automatic mode, input files must be NULL to allow for automatic detection")
    }
  }

  if (mode == "MAgPIE") {

    # read in "biodiversity value" for different land cover classes (unweighted) (in Mha)
    ov_bv <- readGDX(gdx, "ov_bv", select = list(type = "level"), react = "silent")
    if (is.null(ov_bv)) {
      ov_bv <- readGDX(gdx, "ov44_bii", select = list(type = "level"), react = "silent")
    }
    if (is.null(ov_bv)) stop("No Biodiversity Module in MAgPIE")

    # differentiation of land classes
    if (landClass == "all") {

      # calculate average BII values for different land classes
      # read in land areas for different land cover classes
      land        <- land(gdx, level = "cell", types = NULL, subcategories = NULL, sum = FALSE)
      forestArea  <- collapseNames(land(gdx, level = "cell", types = NULL, subcategories = "secdforest", sum = FALSE)[, , "secdforest"])
      secdYoung   <- setNames(dimSums(forestArea[, , paste0("ac",  seq(from = 0, to = 30, by = 5))], dim = 3), nm = "secd_young")
      secdMature  <- setNames(dimSums(forestArea[, , paste0("ac",  seq(from = 0, to = 30, by = 5)), invert = TRUE], dim = 3), nm = "secd_mature")
      forestArea  <- mbind(secdYoung, secdMature)
      rm(secdYoung, secdMature)

      # split pasture into rangeland and managed pastureland
      if (is.null(side_layers)) {
        side_layers <- c("input/luh2_side_layers_c200.mz", "modules/44_biodiversity/bii_btc_apr20/input/luh2_side_layers.cs3", "modules/10_land/input/luh2_side_layers.cs3")
        side_layers <- suppressWarnings(side_layers[min(which(file.exists(side_layers)))])
      }
      side_layers <- read.magpie(side_layers)

      # split pasture into rangeland and managed pasture
      pasture  <- side_layers[, , c("manpast", "rangeland")] * collapseNames(land[, , "past"])

      # calculate average BIIs per land class
      avgForestryBII <- add_dimension(ifelse((collapseDim(land[, , "forestry"]) * side_layers[, , c("forested", "nonforested")]) > 0,
                                                dimSums(ov_bv[, , c("aff_co2p", "aff_ndc", "plant")], dim = "landcover44") /
                                                (collapseDim(land[, , "forestry"]) * side_layers[, , c("forested", "nonforested")]),
                                               0),
                                         nm = "forestry", add = "land")
      avgCropBII     <- add_dimension(ifelse((collapseDim(land[, , c("crop")]) * side_layers[, , c("forested", "nonforested")]) > 0,
                                                dimSums(ov_bv[, , c("crop_ann", "crop_per")], dim = "landcover44") /
                                                (collapseDim(land[, , c("crop")]) * side_layers[, , c("forested", "nonforested")]),
                                               0),
                                        nm = "crop", add = "land")
      pastureBII     <- ifelse(pasture  * side_layers[, , c("forested", "nonforested")] > 0,
                                 ov_bv[, , c("manpast", "rangeland")] /
                                   (pasture  * side_layers[, , c("forested", "nonforested")]),
                               0)
      othersBII      <- ifelse((land[, , c("urban", "primforest", "secdforest", "other")] * side_layers[, , c("forested", "nonforested")]) > 0,
                                  ov_bv[, , c("urban", "primforest", "secdforest", "other")] /
                                    (land[, , c("urban", "primforest", "secdforest", "other")] * side_layers[, , c("forested", "nonforested")]),
                                0)

      # Combine to one indicator
      bii <- mbind(avgForestryBII, avgCropBII, pastureBII, othersBII)
      rm(avgForestryBII, avgCropBII, pastureBII, othersBII)

      # Spatial aggregation
      cell <- bii

      if (level != "cell") {
        stop("Regional resolution not implemented for case of landClass=all")
      }

    } else {

      # aggregation over land classes
      ov_bv <- dimSums(ov_bv, dim = 3)

      cell_area <- land(gdx, level = "cell", sum = TRUE)

      cell <- ov_bv / (cell_area)
      reg  <- superAggregate(ov_bv, level = "reg", aggr_type = "sum") / superAggregate(cell_area, level = "reg", aggr_type = "sum")
      glo  <- superAggregate(ov_bv, level = "glo", aggr_type = "sum") / superAggregate(cell_area, level = "glo", aggr_type = "sum")
    }

    if (level == "reg") {
      x <- reg
    } else if (level == "glo") {
      x <- glo
    } else if (level == "regglo") {
      x <- mbind(reg, glo)
    } else if (level == "cell") {
      x <- cell
    }

  } else if (mode == "postprocessing") {

    # check if postprocessing is possible
    if (any(is.null(bii_coeff), is.null(side_layers))) stop("In postprocessing mode, input files are needed!")

    ov32_land <- readGDX(gdx, "ov32_land", "ov_land_fore", select = list(type = "level"), react = "silent")
    if (!is.null(ov32_land)) {
      if (names(dimnames(ov32_land))[3] == "type32.ac") ac <- TRUE else ac <- FALSE
    } else {
      ac <- FALSE
    }
    if (!ac) stop("In postprocessing mode, age-classes are needed")

    # input files
    bii_coeff <- read.magpie(bii_coeff)

    # add timber if not included in input file; only added for intermediate compatability;
    if (!"timber" %in% getNames(bii_coeff, dim = 1)) {
      timber                    <- bii_coeff[, , "secd_mature"]
      getNames(timber, dim = 1) <- "timber"
      timber[, , "forested"]    <- 0.734401034
      timber[, , "nonforested"] <- 0.539010935
      bii_coeff <- mbind(bii_coeff, timber)
    }

    side_layers <- read.magpie(side_layers)

    # magpie outputs
    ov_land         <- land(gdx, level = "cell")
    ov_area         <- croparea(gdx, level = "cell", product_aggr = FALSE)
    ov32_land       <- readGDX(gdx, "ov32_land", "ov_land_fore", select = list(type = "level"))
    ov35_secdforest <- readGDX(gdx, "ov35_secdforest", "ov_natveg_secdforest", select = list(type = "level"))
    ov35_other      <- readGDX(gdx, "ov35_other", "ov_natveg_other", select = list(type = "level"))

    # sets
    crop_ann44 <- c("tece", "maiz", "trce", "rice_pro", "rapeseed", "sunflower", "potato", "cassav_sp", "sugr_beet", "others", "cottn_pro", "foddr", "soybean", "groundnut", "puls_pro")
    crop_per44 <- c("oilpalm", "begr", "sugr_cane", "betr")
    ac         <- readGDX(gdx, "ac")
    ac_young   <- paste0("ac", seq(0, 30, by = 5))
    ac_mature  <- setdiff(ac, ac_young)

    # calc ov44_bv
    ov44_bv <- new.magpie(getCells(ov_land), getYears(ov_land), getNames(bii_coeff), fill = 0)
    ov44_bv[, , "crop_ann"]    <- dimSums(ov_area[, , crop_ann44], dim = 3) * bii_coeff[, , "crop_ann"] * side_layers[, , c("forested", "nonforested")]
    ov44_bv[, , "crop_per"]    <- dimSums(ov_area[, , crop_per44], dim = 3) * bii_coeff[, , "crop_per"] * side_layers[, , c("forested", "nonforested")]
    ov44_bv[, , "manpast"]     <- collapseNames(ov_land[, , "past"]) * side_layers[, , "manpast"] * bii_coeff[, , "manpast"] * side_layers[, , c("forested", "nonforested")]
    ov44_bv[, , "rangeland"]   <- collapseNames(ov_land[, , "past"]) * side_layers[, , "rangeland"] * bii_coeff[, , "rangeland"] * side_layers[, , c("forested", "nonforested")]
    ov44_bv[, , "urban"]       <- collapseNames(ov_land[, , "urban"]) * bii_coeff[, , "urban"] * side_layers[, , c("forested", "nonforested")]
    ov44_bv[, , "primary"]     <- collapseNames(ov_land[, , "primforest"]) * bii_coeff[, , "primary"] * side_layers[, , c("forested", "nonforested")]
    ov44_bv[, , "secd_young"]  <- dimSums(collapseNames(ov32_land[, , "ndc"])[, , ac_young] + ov35_secdforest[, , ac_young] + ov35_other[, , ac_young], dim = 3) * bii_coeff[, , "secd_young"] * side_layers[, , c("forested", "nonforested")]
    ov44_bv[, , "secd_mature"] <- dimSums(collapseNames(ov32_land[, , "ndc"])[, , ac_mature] + ov35_secdforest[, , ac_mature] + ov35_other[, , ac_mature], dim = 3) * bii_coeff[, , "secd_mature"] * side_layers[, , c("forested", "nonforested")]

    # Afforestation can be based on natveg or plantation growth curves
    s32_aff_plantation <- readGDX(gdx, "s32_aff_plantation", react = "silent")
    if (is.null(s32_aff_plantation)) s32_aff_plantation <- 0 # default
    if (s32_aff_plantation == 0) {
      ov44_bv[, , "secd_young"]  <- ov44_bv[, , "secd_young"] + dimSums(collapseNames(ov32_land[, , "aff"])[, , ac_young], dim = 3) * bii_coeff[, , "secd_young"] * side_layers[, , c("forested", "nonforested")]
      ov44_bv[, , "secd_mature"] <- ov44_bv[, , "secd_mature"] + dimSums(collapseNames(ov32_land[, , "aff"])[, , ac_mature], dim = 3) * bii_coeff[, , "secd_mature"] * side_layers[, , c("forested", "nonforested")]
    } else if (s32_aff_plantation == 1) {
      ov44_bv[, , "timber"]      <- ov44_bv[, , "timber"] + dimSums(collapseNames(ov32_land[, , "aff"]), dim = 3) * bii_coeff[, , "timber"] * side_layers[, , c("forested", "nonforested")]
    }

    # Timber plantations can be based on natveg or plantation growth curves
    s32_timber_plantation <- readGDX(gdx, "s32_timber_plantation", react = "silent")
    if (is.null(s32_timber_plantation)) s32_timber_plantation <- 1 # default
    if (s32_timber_plantation == 0) {
      ov44_bv[, , "secd_young"]  <- ov44_bv[, , "secd_young"] + dimSums(collapseNames(ov32_land[, , "plant"])[, , ac_young], dim = 3) * bii_coeff[, , "secd_young"] * side_layers[, , c("forested", "nonforested")]
      ov44_bv[, , "secd_mature"] <- ov44_bv[, , "secd_mature"] + dimSums(collapseNames(ov32_land[, , "plant"])[, , ac_mature], dim = 3) * bii_coeff[, , "secd_mature"] * side_layers[, , c("forested", "nonforested")]
    } else if (s32_timber_plantation == 1) {
      ov44_bv[, , "timber"]      <- ov44_bv[, , "timber"] + dimSums(collapseNames(ov32_land[, , "plant"]), dim = 3) * bii_coeff[, , "timber"] * side_layers[, , c("forested", "nonforested")]
    }

    ov44_bv   <- dimSums(ov44_bv, dim = 3)
    cell_area <- land(gdx, level = "cell", sum = TRUE)

    # conversion from area weighted biodiversity value (BV) to area weighted biodiversity intactness (BII)
    cell <- ov44_bv / cell_area
    reg  <- superAggregate(ov44_bv, level = "reg", aggr_type = "sum") / superAggregate(cell_area, level = "reg", aggr_type = "sum")
    glo  <- superAggregate(ov44_bv, level = "glo", aggr_type = "sum") / superAggregate(cell_area, level = "glo", aggr_type = "sum")

    if (level == "reg") {
      x <- reg
    } else if (level == "glo") {
      x <- glo
    } else if (level == "regglo") {
      x <- mbind(reg, glo)
    } else if (level == "cell") {
      x <- cell
    }
  } else if (mode == "off") {
    x <- NULL
  }

  out(x, file)
}
