#' @title MapLandUseForDownscaling
#' @description Modifies magpie land use output so that it can be fed into a downscaling model for generating high resolution land use maps.
#'
#' @importFrom magclass mbind read.magpie dimSums
#' @importFrom gdx out
#' @importFrom magpiesets findset
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param dir output directory which contains cellular magpie output.
#' @param downscaling_model The only current option is "SEALS" (Spatial Economic Allocation Landscape Simulator).
#' @param selectyears Years to provide data for.
#' @param grid Whether gridded output should be used. Otherwise output will be returned at cluster level.
#' @return Shares of different land use classes per grid sell as magpie object.
#' @author Patrick v. Jeetze
#' @examples
#' \dontrun{
#' x <- MapLandUseForDownscaling(dir = ".", downscaling_model = "SEALS")
#' }
#'
MapLandUseForDownscaling <- function(gdx, dir = ".", file = NULL, downscaling_model = "SEALS", selectyears = c("y2015", "y2050"), grid = TRUE) {
  downscaling_model_list <- c( # List of available models for downscaling:
    "SEALS"
  )

  if (!any(grepl(downscaling_model, downscaling_model_list))) {
    stop(paste(
      "No available model for downscaling specified. Current options for 'downscaling_model' are",
      paste(gsub("(\\w+)", '"\\1"', downscaling_model_list), collapse = " ")
    ))
  }

  selectyears <- sort(findset(selectyears, noset = "original"))

  if (downscaling_model == "SEALS") {
    if (grid == TRUE) {
      landShr <- read.magpie(file.path(dir, "cell.land_0.5_share.mz"))[, selectyears, ]
    } else {
      landUse <- land(gdx, level = "cell")[, selectyears, ]
      landShr <- landUse / dimSums(landUse, dim = 3)
    }

    out_seals <- mbind(
      setNames(landShr[, , "crop"], "crop"),
      setNames(landShr[, , "past"], "past"),
      setNames(landShr[, , "primforest"] + landShr[, , "secdforest"] + landShr[, , "forestry"], "forest"),
      setNames(landShr[, , "urban"], "urban"),
      setNames(landShr[, , "other"], "other")
    )

    out(out_seals, file)
  }
}
