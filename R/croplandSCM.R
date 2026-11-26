#' @title croplandSCM
#' @description reads cropland area under soil carbon management (SCM) out of a
#'              MAgPIE gdx file (Mha)
#'
#' @export
#'
#' @param gdx   GDX file
#' @param level aggregation level, "reg", "glo", "regglo", or "cell"
#' @param crop_aggr aggregate over crop types (TRUE) or report by crop (FALSE)
#' @param fallow handling of the SCM area on fallow land: "sum" (add it to the
#'               crop area), "separate" (return crop and fallow area as
#'               separate names "crop_scm" and "fallow_scm") or "off" (crop
#'               area only). Fallow land under SCM is reported separately
#'               (ov59_area_fallow_scm) only by newer MAgPIE versions and is
#'               ignored for older ones. Only supported together with
#'               crop_aggr = TRUE.
#' @return SCM area as MAgPIE object (mio. ha)
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' x <- croplandSCM(gdx)
#' }
#'
croplandSCM <- function(gdx, level = "reg", crop_aggr = TRUE, fallow = "sum") {

  # Try new area-based implementation (cellpool_feb26 and merged cellpool_jan23)
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

  # SCM area on fallow land, reported separately only by newer MAgPIE versions
  if (!identical(fallow, "off") && !is.null(readGDX(gdx, "ov59_area_fallow_scm",
                                                    react = "silent",
                                                    select = list(type = "level")))) {
    fallowScm <- readGDX(gdx, "ov59_area_fallow_scm", react = "silent",
                         select = list(type = "level"))
    if ("scm" %in% getNames(fallowScm)) {
      fallowScm <- fallowScm[, , "scm"]
    }
    fallowScm <- gdxAggregate(gdx = gdx, x = fallowScm, weight = "land",
                              types = "crop", to = level, absolute = TRUE)
    if (!crop_aggr) {
      warning("SCM area on fallow land is only supported together with ",
              "crop_aggr = TRUE, fallow area is ignored.")
    } else if (identical(fallow, "separate")) {
      out <- mbind(out, setNames(fallowScm, "fallow_scm"))
    } else {
      out <- out + setNames(fallowScm, getNames(out))
    }
  }

  return(out)
}
