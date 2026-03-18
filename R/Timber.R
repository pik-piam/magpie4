#' @title Timber
#' @description reads timber demand out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "regglo" (regional and global), custom region aggregation
#' or any secdforest aggregation level defined in superAggregateX
#' @details Forest demandfor timber production
#' @return Forest demandfor timber production
#' @author Abhijeet Mishra
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie setCells
#' @examples
#'
#'   \dontrun{
#'     x <- Timber(gdx)
#'   }

Timber <- function(gdx, file = NULL, level = "regglo") {
  a <- NULL
  kforestry <- readGDX(gdx, "kforestry")
  if (level %in% c("reg", "regglo") || isCustomAggregation(level)) {

    # Read volumetric conversion factor (basic wood density for tDM to m3 conversion)
    density <- readGDX(gdx, "pm_vol_conv", "f73_volumetric_conversion", react = "silent", format = "first_found")
    if (!is.null(getNames(density)) && "wood" %in% getNames(density)) {
      density <- add_columns(x = density, addnm = "constr_wood")
      density[, , "constr_wood"] <- density[, , "wood"]
    }

    # Aggregate to reg first (constr_wood splitting needs regional level, density conversion
    # must happen before adding GLO to avoid dimension mismatch with regional density)
    ov_supply <- readGDX(gdx, "ov_supply", select = list(type = "level"))[, , kforestry]
    originalRegions <- getItems(ov_supply, 1.1)
    ov_supply <- superAggregateX(data = ov_supply, aggr_type = "sum", level = "reg")
    ov_supply <- add_columns(x = ov_supply, addnm = "constr_wood")

    ov_prod <- readGDX(gdx, "ov_prod", select = list(type = "level"))[, , kforestry]
    ov_prod <- superAggregateX(data = ov_prod, aggr_type = "sum", level = "reg")
    ov_prod <- add_columns(x = ov_prod, addnm = "constr_wood")

    v73_prod_heaven_timber <- readGDX(gdx, "ov73_prod_heaven_timber", select = list(type = "level"))[, , kforestry]
    v73_prod_heaven_timber <- superAggregateX(data = v73_prod_heaven_timber, aggr_type = "sum", level = "reg")
    v73_prod_heaven_timber <- add_columns(x = v73_prod_heaven_timber, addnm = "constr_wood")

    p73_demand_constr_wood <- readGDX(gdx, "p73_demand_constr_wood", react = "silent")
    if (is.null(p73_demand_constr_wood)) {
      p73_demand_constr_wood <- 0
      ov_supply[, , "constr_wood"] <- 0
      ov_prod[, , "constr_wood"] <- 0
    } else {
      p73_demand_constr_wood <- superAggregateX(data = p73_demand_constr_wood, level = "reg", aggr_type = "sum")
      ov_supply[, , "constr_wood"] <- p73_demand_constr_wood[, getYears(ov_supply), ]
      ov_supply[, , "wood"] <- ov_supply[, , "wood"] - ov_supply[, , "constr_wood"]

      constr_wood_share <- ov_supply[, , c("wood", "constr_wood")] / dimSums(ov_supply[, , c("wood", "constr_wood")], dim = c(1, 3))

      ov_prod[, , "constr_wood"] <- dimSums(ov_prod[, , "wood"], dim = 1) * constr_wood_share[, , "constr_wood"]
      ov_prod[, , "wood"] <- ov_prod[, , "wood"] - ov_prod[, , "constr_wood"]
    }

    # Convert from tDM to m3
    ov_supply <- ov_supply / density
    ov_prod <- ov_prod / density
    v73_prod_heaven_timber <- v73_prod_heaven_timber / density
    v73_prod_heaven_timber[is.na(v73_prod_heaven_timber)] <- 0

    netTrade <- ov_prod - ov_supply

    exports <- netTrade
    exports[exports < 0] <- 0
    exports <- gdxAggregate(gdx, exports, to = level)

    imports <- netTrade
    imports[imports > 0] <- 0
    imports <- -1 * imports
    imports <- gdxAggregate(gdx, imports, to = level)

    # Aggregate to requested level (adds GLO for regglo)
    ov_supply <- gdxAggregate(gdx, ov_supply, to = level)
    ov_prod <- gdxAggregate(gdx, ov_prod, to = level)
    v73_prod_heaven_timber <- gdxAggregate(gdx, v73_prod_heaven_timber, to = level)
    netTrade <- gdxAggregate(gdx, netTrade, to = level)

    a <- mbind(add_dimension(x = ov_supply, dim = 3.1, nm = "Demand"),
               add_dimension(x = ov_prod, dim = 3.1, nm = "Production"),
               add_dimension(x = v73_prod_heaven_timber, dim = 3.1, nm = "Heaven"),
               add_dimension(x = netTrade, dim = 3.1, nm = "Net-Trade"),
               add_dimension(x = exports, dim = 3.1, nm = "Exports"),
               add_dimension(x = imports, dim = 3.1, nm = "Imports"))
  } else if (level == "cell") {
    stop("Resolution not recognized. Select reg or regglo as level. NULL returned.")
  }

  out(a, file)
}
