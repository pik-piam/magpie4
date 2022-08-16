#' @title getReportFSECCropDiversityGrid
#' @description Reports grid cell level crop diversity for the FSEC project
#'
#' @export
#'
#' @param gdx a GDX file
#' @param reportOutputDir a folder name for the output to be written to. If NULL the report is not saved to
#' disk, and only returned to the calling function.
#' @param magpieOutputDir a magpie output directory which contains a mapping file (clustermap*.rds) for the
#' disaggregation of grid output
#' @param scenario the name of the scenario used. If NULL the report is not saved to disk, and only returned to the
#' calling function.
#' @return A list of MAgPIE objects containing the reports
#' @author Patrick v. Jeetze
#' @importFrom magclass write.magpie
#' @examples
#' \dontrun{
#' x <- getReportFSECCropDiversityGrid(gdx, magpieOutputDir)
#' }
#'
getReportFSECCropDiversityGrid <- function(gdx, reportOutputDir = NULL, magpieOutputDir, scenario = NULL) {

  # Functions -------------------------------------------------------------------------------------------------------

  .formatReport <- function(.x, .name = NULL) {
    getSets(.x)[c("d1.1", "d1.2")] <- c("iso", "cell")
    getSets(.x, fulldim = FALSE)[3] <- "variable"
    if (!is.null(.name)) {
      getNames(.x) <- .name
    }
    return(.x)
  }

  .saveNetCDFReport <- function(.x, .file, .comment = NULL) {
    if (!is.null(reportOutputDir) && !is.null(scenario)) {
      write.magpie(.x,
        file_name = file.path(reportOutputDir, paste0(scenario, "-", .file, ".nc")),
        comment = .comment
      )
    }
  }

  # Crop diversity -------------------------------------------------

  gridLand <- reportGridLand(gdx, dir = magpieOutputDir)

  cropland <- gridLand[, , "Cropland"]
  # Set minuscule values of cropland (< 10 ha per grid cell) to zero
  cropland[cropland < 0.0001] <- 0
  # Set values >= 10 ha of cropland to 1
  cropland[cropland != 0] <- 1

  # get crop diversity
  cropDiv <- reportCropDiversity(gdx, grid = TRUE)

  # Remove minuscule values of cropland (< 10 ha per grid cell)
  cropDiv <- cropland * cropDiv

  cropDiv <- .formatReport(cropDiv, c("ShannonCropDiversity", "InvSimpsonCropDiversity"))
  .saveNetCDFReport(cropDiv, .file = "CropDiversityGridded", .comment = "unitless")

  # Return list -----------------------------------------------------------------------------------------------------

  return(list("CropDiversityGridded" = cropDiv))
}
