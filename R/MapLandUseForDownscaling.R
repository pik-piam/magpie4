#' @title MapLandUseForDownscaling
#' @description Calculates share of primary and secondary non-forest vegetation for different aggregation levels based on gridded magpie output and luh2v2 initial shares of primary and secondary non-forest vegetation.
#'
#' @importFrom magclass mbind read.magpie dimSums
#' @importFrom luscale superAggregate
#' @importFrom gdx out
#' @export
#'
#' @param file a file name the output should be written to using write.magpie
#' @param dir output directory which contains cellular magpie output and the the luh2v2 initialisation data for primary and secondary vegetation ("cell.luh2v2initial_primsecdother_0.5_share.mz").
#' @param downscaling_model The only current option is "SEALS" (Spatial Economic Allocation Landscape Simulator).
#' @return Area of primary and secondary non-forested vegetation in MAgPIE (other land) as MAgPIE object; either as unit of area (Mha) or as fraction of total land per grid cell (share).
#' @author Patrick v. Jeetze
#' @seealso \code{\link{PrimSecdOtherLand}}
#' @examples
#'
#' \dontrun{
#' x <- MapLandUseForDownscaling(dir = ".", downscaling_model = "SEALS")
#' }
#'
MapLandUseForDownscaling <- function(dir = ".", file = NULL, downscaling_model = "SEALS") {
  
  downscaling_model_list <- c( # List of available models for downscaling:
                              "SEALS"
                              )

  if (!any(grepl(downscaling_model, downscaling_model_list))) {
    warning(paste("No available model for downscaling specified. Current options for 'downscaling_model' are", 
                  paste(gsub("(\\w+)", '"\\1"', downscaling_model_list), collapse = " ")))
  }

  if (downscaling_model == "SEALS") {
    primSecOther <- PrimSecdOtherLand(level = "grid", dir = dir, unit_gridded = "share")
    landShr <- read.magpie(file.path(dir, "cell.land_0.5_share.mz"))
    out_seals <- landShr[, , "other", invert = TRUE]
    out_seals <- mbind(out_seals, primSecOther)
    out_seals <- out_seals[, "y1985", , invert = TRUE]
    out(out_seals, file)
  }
}
