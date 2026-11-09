#' @title reportPBland
#' @description reports land planetary boundary: forest area
#'
#' @export
#'
#' @param gdx   GDX file
#' @param level level of aggregation (regglo: regions and global)
#' @param foresttype managed forest types that are included in the calculation of the
#'                   forest area (all: all managed forests,
#'                                noTimber: timber plantations are not counted)
#'
#' @return MAgPIE object
#' @author Felicitas Beier, Patrick von Jeetze
#' @import magclass
#' @examples
#'
#'   \dontrun{
#'     x <- reportPBland(gdx)
#'   }
#'
#'
#' @section Land planetary boundary variables:
#' Name | Unit | Meta
#' ---|---|---
#' Planetary Boundary\|Land\|Forest cover | Mha | Total forest area (natural and managed forests)
#' @md


reportPBland <- function(gdx, level = "regglo", foresttype = "all") {

  # gridded land use by land type, sub-type and water supply (land.type.water)
  landSplit <- read.magpie(file.path(dirname(normalizePath(gdx)), "cell.land_split_0.5.mz"))

  ### Land Boundary ###
  # (1) Def.: Area of forested land (compared to original forest cover):
  # Richardson et al. (2023): 75% of original forest cover
  # Note: we calculate in Mha and compare to a PB translated to Mha (4790 Mha globally)
  indicatorname <- "Planetary Boundary|Land|Forest cover"
  unit          <- "Mha"
  variable      <- paste0(indicatorname, " (", unit, ")")

  # Select forest categories that count towards forested land:
  # natural forests (primary and secondary) and managed forests (NPI/NDC, Afforestation)
  # plantations are not counted towards forests for land PB
  naturalForests <- c("primforest.total.total", "secdforest.total.total")

  if (foresttype == "all") {
    # all managed forests are included in the PB (incl. timber and other planted)
    managedForests <- c("forestry.aff.total", "forestry.ndc.total",
                        "forestry.plant.total", "forestry.other_planted.total")
  } else if (foresttype == "noTimber") {
    # harvested planted forest (timber plantations and other planted) is excluded here;
    # only afforestation (NPiNDC and CO2-price driven) counts towards forests
    plantation <- readGDX(gdx, "s32_aff_plantation")

    if (plantation) {
      managedForests <- "forestry.ndc.total"
    } else {
      managedForests <-  c("forestry.ndc.total", "forestry.aff.total")
    }
  }

  # runs without other planted forest lack the category
  forestCats <- intersect(c(naturalForests, managedForests), getItems(landSplit, dim = 3))
  x <- dimSums(landSplit[, , forestCats], dim = 3)

  if (!is.null(x)) {
    if (level != "grid") {
      x <- gdxAggregate(gdx, x, to = level, weight = NULL, absolute = TRUE)
    }
    message("Finished calculating Land PB: Forest cover")
  }

  getItems(x, dim = 3) <- variable

  return(x)
}
