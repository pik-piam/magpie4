#' @title tradeValue
#' @description Calculates the value of traded goods based on a gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation ("reg", "glo", "regglo")
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param product_aggr aggregate over products or not (boolean)
#' @param type exports-imports ("net-exports"), gross imports ("imports") or gross exports ("exports"); only valid if relative=FALSE
#' @param glo_weight Decides the calculation of global prices. Weighting schemes are applied for estimation of global producer price.
#' If \code{"export"} prices are calculated as average of regional exporters' prices, weighted by the export volumes. If \code{"production"} (default),
#' prices are calculated as average of regional prices weighted by regional production. Alternatively, if \code{"free_trade"},
#'  the global prices are directly taken from the shadow prices of the global trade constraint, and no averaging is performed.
#'  Alternatively, if \code{"constant_glo_vop"} constant 1995 global prices for each commodity are used as weight.
#' @param relative if relative=TRUE, self sufficiencies are reported (the amount of production divided by domestic demand)
#' @return A MAgPIE object containing the value of trade flows in Million of US dollars
#' @author Misko Stevanovic, Florian Humpenoeder, Edna J. Molina Bacca
#' @examples
#' \dontrun{
#' x <- tradeValue(gdx)
#' }
#'
#' @importFrom gdx expand.set
#' @importFrom magpiesets findset

tradeValue <- function(gdx, file = NULL, level = "reg", products = "k_trade", product_aggr = FALSE, type = "net-exports", glo_weight = "export", relative = FALSE) {

# Global prices use for calculation of absolute trade value or as weight for the relative case
  if (glo_weight == "constant_glo_vop") {

    glo_p <- readGDX(gdx, "f15_prices_initial")

  } else if  (glo_weight %in% c("production", "export", "free_trade")) {

    glo_p <- prices(gdx, level = "glo", products = products, glo_weight = glo_weight)

  } else {
    stop("Price not supported")
  }


  if (relative) {

    production <- production(gdx, level = "reg", products = products, product_aggr = FALSE, attributes = "dm")
    demand <- dimSums(demand(gdx, level = "reg", products = products, product_aggr = FALSE, attributes = "dm"), dim = 3.1)

    # Self-sufficiency per item and region
    out <- production / demand
    out[!is.finite(out)] <- 0

    if (product_aggr) {

    # weight for aggregation
    glo_agg <- new.magpie(cells_and_regions = getCells(out),
                       years = getYears(out),
                       names = getNames(out), )
    # intersection of products in costs and volume
    items <- intersect(getNames(out), findset(products))
    glo_agg[, , ] <- glo_p[, , items]

    # weighted aggregation of self-sufficiency for all items
    mapping <- as.data.frame(getNames(glo_agg))
    colnames(mapping) <- "items"
    mapping$total <- "total"
    out <- setNames(toolAggregate(out, rel = mapping, weight = glo_agg, from = "items", to = "total", dim = 3), paste0("Self-Sufficiency ratio ", products))
    }

    } else {

    # regional trade flows
    volume <- trade(gdx, level = "reg", products = products, type = type)

    # intersection of products in prices  and volume
    items <- intersect(getNames(volume), getNames(glo_p))
    glo_agg <- glo_p[, , items]

    # value of trade flows
    out <- volume * glo_agg

    # aggregate
    out <- superAggregate(out, level = level, aggr_type = "sum", crop_aggr = product_aggr)
  }


  out(out, file)
}
