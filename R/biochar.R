#' @title biochar
#' @description Calculates biochar-related indicators from a MAgPIE gdx file. This
#' function provides all indicators for biochar-related material flows and conversion,
#' including biochar production, feedstock demand, long-term stable carbon storage in soils,
#' and the cumulative biochar stock applied to cropland soils.
#'
#' @param gdx GDX file
#' @param indicator Indicator types: bc_production, bc_feedstock_dem, bc_stable_carbon,
#'   or bc_soil_stock
#' @param level Spatial aggregation: "reg", "glo", or "regglo"
#' @param feedstockAggr If TRUE, aggregates over feedstock types. If not applicable
#' for the selected indicator, set to FALSE (default).
#' @param systemAggr If TRUE, aggregates over bc_tech63. If not applicable
#' for the selected indicator, set to FALSE (default).
#' @param attributes Available output attributes: dry matter: Mt ("dm"), gross energy:
#' PJ ("ge"), carbon: Mt C ("c"). Can also be a vector. The availability of attributes
#' depends on the selected indicator.
#' @param file File name the output should be written to using write.magpie
#'
#' @return Selected biochar-related indicator as MAgPIE object:
#' \itemize{
#'   \item \code{bc_production}: Biochar production (unit depends on \code{attributes}).
#'   Available attributes are "dm", "ge", and "c".
#'   \item \code{bc_feedstock_dem}: Biomass feedstock demand for biochar passed to
#'   the 60_bioenergy module (unit depends on \code{attributes}). In rem-mag mode
#'   this value is zero because REMIND already includes biochar feedstock in the
#'   bioenergy demand. Available attributes are "dm", "ge", and "c".
#'   \item \code{bc_stable_carbon}: Stable C in soil from applied biochar after
#'   100 years (mio. tC per yr). Available attribute is "c".
#'   \item \code{bc_soil_stock}: Cumulative biochar stock applied to cropland soils
#'   (t DM per ha).
#' }
#' @author Isabelle Weindl, Kristine Karstens
#'
#' @importFrom magclass dimSums collapseNames
#' @importFrom magpiesets findset
#' @export
#' @examples
#' \dontrun{
#'   x <- biochar(gdx, indicator = "bc_production", level = "regglo", feedstockAggr = TRUE)
#' }
#'
biochar <- function(gdx, indicator, level = "reg", feedstockAggr = FALSE, systemAggr = FALSE, attributes = "c", file = NULL) { #nolint: cyclocomp
  weightedAggrDone <- FALSE

  if (is.null(readGDX(gdx, "ov63_biochar_prod", react = "silent"))) {
    return(NULL)
  }

  #### Biochar production ###
  if (indicator == "bc_production") {
    avlAttributes <- c("dm", "ge", "c")
    if (!all(attributes %in% avlAttributes)) {
      stop("Invalid attribute(s) specified. Choose from: 'dm', 'ge', 'c'.")
    }

    x <- readGDX(gdx, "ov63_biochar_prod", select = list(type = "level")) # in mio. GJ per yr

    if (any(attributes != "ge")) {
      att <- readGDX(gdx, "f63_biochar_attributes")
      #!!! TODO: delete/modify following line if the status of KonTiki is clarified, e.g. if values for KonTiki are available
      sys <- grep("kontiki", getNames(att, dim = 2), value = TRUE, invert = TRUE, ignore.case = TRUE)
      x <- collapseNames(x[, , sys] / att[, , sys][, , "ge"]) * att[, , sys][, , attributes]
    }
    if (feedstockAggr == TRUE) x <- dimSums(x, dim = "feedstock63")
    if (systemAggr == TRUE) x <- dimSums(x, dim = "bc_tech63")

    x <- collapseNames(x)

    #### Biomass feedstock demand for biochar ###
  } else if (indicator == "bc_feedstock_dem") {
    avlAttributes <- c("dm", "ge", "c")
    if (!all(attributes %in% avlAttributes)) {
      stop("Invalid attribute(s) specified. Choose from: 'dm', 'ge', 'c'.")
    }

    x <- readGDX(gdx, "ov_biochar_feedstock_mag", select = list(type = "level"))
    x <- collapseNames(x)

    if (any(attributes != "ge")) {
      # Derive attributes for 2nd gen biomass feedstock, aggregated to feedstock groups (residues versus dedicated)
      BEres_2nd <- readGDX(gdx, "ov60_2ndgen_bioenergy_dem_residues",
                           select = list(type = "level")) # in mio. GJ per yr
      BEcrops_2nd <- readGDX(gdx, "ov60_2ndgen_bioenergy_dem_dedicated",
                             select = list(type = "level")) # in mio. GJ per yr

      res <- findset("kres")
      becrops <- findset("bioenergycrops")
      attAll <- readGDX(gdx, "fm_attributes")[, , union(attributes, "ge")]

      attBEres_2nd <- collapseNames(BEres_2nd / attAll[, , "ge"]) * attAll[, , attributes]
      attBEcrops_2nd <- collapseNames(BEcrops_2nd / attAll[, , "ge"]) * attAll[, , attributes]

      # ratio of 'attribute' content relative to energy content
      att2ge_res_2nd <-
        dimSums(attBEres_2nd[, , res], dim = 3.1) /
        setNames(dimSums(BEres_2nd[, , res], dim = 3), "residues")
      att2ge_crops_2nd <-
        dimSums(attBEcrops_2nd[, , becrops], dim = 3.1) /
        setNames(dimSums(BEcrops_2nd[, , becrops], dim = 3), "dedicated")
      att2ge_2nd <- mbind(att2ge_res_2nd, att2ge_crops_2nd)
      att2ge_2nd[is.nan(att2ge_2nd)] <- 0

      # check whether feedstock demand can be converted to the requested attributes
      zeroAtt <- collapseNames(att2ge_2nd[, , attributes[1]]) == 0
      feedstockDemand <- collapseNames(x)
      inconsistValues <- feedstockDemand[zeroAtt & feedstockDemand != 0]

      if (length(inconsistValues) > 0) {
        warning("Found biochar feedstock demand where attributes derived from 2nd generation biomass demand are zero.")
      }

      x <- x * att2ge_2nd
    }

    if (feedstockAggr == TRUE) x <- dimSums(x, dim = "feedstock63")
    if (systemAggr == TRUE) x <- dimSums(x, dim = "bc_tech63")

    x <- collapseNames(x)

    #### Stable C in soil from biochar ###
  } else if (indicator == "bc_stable_carbon") {

    if (any(attributes != "c")) {
      stop("Invalid 'attributes' specified. Available attribute is \"c\".")
    }

    x <- readGDX(gdx, "ov63_c_stable_biochar_fs", select = list(type = "level"))

    if (feedstockAggr == TRUE) x <- dimSums(x, dim = "feedstock63")
    if (systemAggr == TRUE) x <- dimSums(x, dim = "bc_tech63")

    x <- collapseNames(x)

    #### Cumulative biochar stock in cropland soils ###
  } else if (indicator == "bc_soil_stock") {

    x <- readGDX(gdx, "ov63_biochar_stock_area", select = list(type = "level"))
    x <- collapseNames(x[, , "crop"])

    if (level != "cell") {
      x <- gdxAggregate(gdx, x, to = level, weight = "land", types = "crop",
                        absolute = FALSE)
      weightedAggrDone <- TRUE
    }

  } else {
    stop("Invalid 'indicator' specified. Choose from: bc_production, ",
         "bc_feedstock_dem, bc_stable_carbon, bc_soil_stock.")
  }

  #aggregate over regions
  if (!weightedAggrDone && level != "reg") {
    x <- superAggregateX(x, aggr_type = "sum", level = level)
  }

  out(x, file)
}
