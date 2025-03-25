#' @title reportPBland
#' @description reports land planetary boundary: forest area
#'
#' @export
#'
#' @param gdx   GDX file
#' @param level level of aggregation (regglo: regions and global)
#' @param dir   directory with required spatial data
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

reportPBland <- function(gdx, level = "regglo", dir = ".", foresttype = "all") {

  # gridded land use with detailed categories
  landSplit <- read.magpie(file.path(dir, "cell.land_split_0.5.mz"))

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
  naturalForests <- c("primforest", "secdforest")

  if (foresttype == "all") {
    # all managed forests are included in the PB
    managedForests <- c("PlantedForest_Afforestation", "PlantedForest_NPiNDC",
                        "PlantedForest_Timber")
  } else if (foresttype == "noTimber") {
    # If forestry realization is activated, parts of planted forest (NPiNDC and
    # CO2-price driven afforested area) are counted towards forests
    plantation <- readGDX(gdx, "s32_aff_plantation")

    if (plantation) {
      managedForests <- "PlantedForest_NPiNDC"
    } else {
      managedForests <-  c("PlantedForest_NPiNDC", "PlantedForest_Afforestation")
    }
  }

  x <- dimSums(landSplit[, , c(naturalForests, managedForests)], dim = 3)

  if (!is.null(x)) {
    if (level != "grid") {
      x <- gdxAggregate(gdx, x, to = level, weight = NULL, absolute = TRUE, dir = dir)
    }
    message("Finished calculating Land PB: Forest cover")
  }

  getItems(x, dim = 3) <- variable

  return(x)
}
