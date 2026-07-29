#' @title croplandSCM
#' @description reads cropland area under soil carbon management (SCM) out of a
#'              MAgPIE gdx file (Mha)
#'
#' @export
#'
#' @param gdx   GDX file
#' @param level aggregation level, "reg", "glo", "regglo", or "cell"
#' @param crop_aggr aggregate over crop types (TRUE) or report by crop (FALSE)
#' @return SCM area as MAgPIE object (mio. ha)
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' x <- croplandSCM(gdx)
#' }
#'
croplandSCM <- function(gdx, level = "reg", crop_aggr = TRUE) {

  # Try new area-based implementation (cellpool_feb26)
  scmArea <- readGDX(gdx, "ov59_area_scm", react = "silent",
                     select = list(type = "level"))

  if (!is.null(scmArea)) {
    # cellpool_feb26: explicit v59_area_scm(j,kcr,w,scmtype59)
    scmArea <- scmArea[, , "scm"]
    if (crop_aggr) {
      scmArea <- dimSums(scmArea, dim = 3)
      scmArea <- setNames(scmArea, "crop_scm")
    } else {
      scmArea <- dimSums(scmArea, dim = "w")
      scmArea <- collapseDim(scmArea, dim = "scmtype59")
    }
  } else {
    # Fallback: share-based implementation (cellpool_jan23)
    scmShare <- readGDX(gdx, "i59_scm_target", react = "silent")
    if (!is.null(scmShare)) {
      croparea <- readGDX(gdx, "ov_area", select = list(type = "level"))
      if (crop_aggr) {
        cropareaTotal <- dimSums(croparea, dim = 3)
        scmArea <- cropareaTotal * scmShare
        scmArea <- setNames(scmArea, "crop_scm")
      } else {
        scmArea <- croparea * scmShare
        scmArea <- dimSums(scmArea, dim = "w")
      }
    } else {
      # No SCM in this run
      scmArea <- land(gdx, types = "crop", level = "cell") * 0
      scmArea <- setNames(scmArea, "crop_scm")
    }
  }

  out <- gdxAggregate(gdx = gdx, x = scmArea, weight = "land",
                      types = "crop", to = level, absolute = TRUE)

  return(out)
}
