#' @title biochar
#' @description Calculates biochar-related indicators from a MAgPIE gdx file. This
#' function provides all indicators for biochar-related material flows and conversion,
#' including biochar production, feedstock demand, and long-term stable carbon storage in soils.
#'
#' @param gdx GDX file
#' @param indicator Indicator types: bc_production, bc_feedstock_dem or bc_stable_carbon
#' @param level Spatial aggregation: "reg", "glo", or "regglo"
#' @param feedstockAggr If TRUE, aggregates over feedstock types. If not applicable
#' for the selected indicator, set to FALSE (default).
#' @param systemAggr If TRUE, aggregates over bc_sys63 or biopyr_all63. If not applicable
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
#'   \item \code{bc_feedstock_dem}: Biomass feedstock demand for biochar (unit depends
#'   on \code{attributes}). Available attributes are "dm", "ge", and "c".
#'   \item \code{bc_stable_carbon}: Stable C in soil from biochar after 100 years
#'   (mio. tC per yr). Available attribute is "c".
#' }
#' @author Isabelle Weindl
#'
#' @importFrom magclass dimSums collapseNames
#' @importFrom magpiesets findset
#' @export
#' @examples
#' \dontrun{
#'   x <- biochar(gdx, indicator = "bc_production", level = "regglo", feedstockAggr = TRUE)
#' }
#'

biochar <- function(gdx, indicator, level = "reg", feedstockAggr = FALSE, systemAggr = FALSE, attributes = "c", file = NULL) {
  out <- NULL

  biochar <- readGDX(gdx, "ov63_biochar_prod", react = "silent")
  if (is.null(biochar)) {
    return(out)
  }

  #### Biochar production ###
  if (indicator == "bc_production") {
    avlAttributes <- c("dm", "ge", "c")
    if (!all(attributes %in% avlAttributes)) {
      stop("Invalid attribute(s) specified. Choose from: 'dm', 'ge', 'c'.")
    }

    x <- readGDX(gdx, "ov63_biochar_prod", select = list(type = "level"), react = "silent") # in mio. GJ per yr

    if (!is.null(x)) {

      if (any(attributes != "ge")) {

        att <- readGDX(gdx, "f63_biochar_attributes")
        #!!! TODO: delete/modify following line if the status of Kontiki is clarified, e.g. if values for Kontiki are available
        sys <- setdiff(getNames(att, dim = 2), "P_Kontiki")
        x <- collapseNames(x[, , sys] / att[, , sys][, , "ge"]) * att[, , sys][, , attributes]
      }
      if (feedstockAggr == TRUE) x <- dimSums(x, dim = "feedstock63")
      if (systemAggr == TRUE) x <- dimSums(x, dim = "bc_sys63")

      x <- collapseNames(x)
    }


    #### Biomass feedstock demand for biochar ###
  } else if (indicator == "bc_feedstock_dem") {
    avlAttributes <- c("dm", "ge", "c")
    if (!all(attributes %in% avlAttributes)) {
      stop("Invalid attribute(s) specified. Choose from: 'dm', 'ge', 'c'.")
    }

    bcProd <- biochar(gdx, indicator = "bc_production", level = "reg",
                      feedstockAggr = FALSE, systemAggr = FALSE,
                      attributes = "ge")
    bcYield <- readGDX(gdx, "f63_biochar_efficiency", react = "silent")

    if (!is.null(bcProd) && !is.null(bcYield)) {
      #!!! TODO: delete following line (and "sys") if the status of Kontiki is clarified,
      # e.g. if values for Kontiki are available
      sys <- setdiff(getNames(bcYield, dim = 2), "P_Kontiki")

      # Calculate back the required biomass feedstock from biochar production (both expressed in mio. GJ per yr)
      bcFeedstock <- bcProd[, , sys] / collapseNames(bcYield[, , "en_yield"][, , sys])

      x <- bcFeedstock

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

        # check consistency between biochar production and 2nd gen biomass demand
        zeroAtt <- collapseNames(att2ge_2nd[, , attributes[1]]) == 0
        bcProdFeedstock <- collapseNames(dimSums(bcProd, dim = 3.1))
        inconsistValues <- bcProdFeedstock[zeroAtt & bcProdFeedstock != 0]

        if (length(inconsistValues) > 0) {
          warning("Found biochar production where attributes derived from 2nd generation biomass demand are zero.")
        }

        x <- bcFeedstock * att2ge_2nd

      }
      if (feedstockAggr == TRUE) x <- dimSums(x, dim = "feedstock63")
      if (systemAggr == TRUE) x <- dimSums(x, dim = "bc_sys63")

      x <- collapseNames(x)
    }


    #### Stable C in soil from biochar ###
  } else if (indicator == "bc_stable_carbon") {

    if (any(attributes != "c")) {
      stop("Invalid 'attributes' specified. Available attribute is \"c\".")
    }

    bcProdC <- biochar(gdx, indicator = "bc_production", level = "reg",
                       feedstockAggr = FALSE, systemAggr = FALSE,
                       attributes = "c")
    bc100 <- readGDX(gdx, "s63_BC100", react = "silent")


    if (!is.null(bcProdC) && !is.null(bc100)) {

      x <- setNames(bc100, "c") * bcProdC

      if (feedstockAggr == TRUE) x <- dimSums(x, dim = "feedstock63")
      if (systemAggr == TRUE) x <- dimSums(x, dim = "bc_sys63")

      x <- collapseNames(x)
    }


  } else {
    stop("Invalid 'indicator' specified. Choose from: bc_production, bc_feedstock_dem, bc_stable_carbon.")
  }


  #aggregate over regions
  if (level != "reg") {
    x <- superAggregateX(x, aggr_type = "sum", level = level)
  }

  out(x, file)
}
