#' @title prices
#' @description calcluates prices based on a MAgPIE gdx file
#'
#' @importFrom magclass getRegions
#' @importFrom stats weighted.mean
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#' "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param product_aggr aggregate over products or not (boolean)
#' @param attributes USD05MER per ton X (dm,nr,p,k,wm) except gross energy (ge) where it is USD05MER per GJ
#' @param type "consumer" or "producer" prices. Producers' prices are calculated on the regional
#' level as a sum of regional trade equation marginal values and respective global trade equation
#' marginal values.For the non traded commodities, both global and regional producers prices are
#' set to zero instead of NaN.
#' @param glo_weight Decides the calculation of global prices. Weighting schemes are applied for
#' estimation of global producer price. If \code{"export"} prices are calculated as average of regional
#' exporters' prices, weighted by the export volumes. If \code{"production"} (default), prices are
#' calculated as average of regional prices weighted by regional production. If
#' \code{"free_trade"}, the global prices are directly taken from the shadow prices of the global
#' trade constraint, and no averaging is performed.
#' @return A MAgPIE object containing the consumer's or producers' prices (unit depends on attributes)
#' @author Misko Stevanovic, Florian Humpenoeder, Jan Philipp Dietrich, Xiaoxi Wang, Edna J. Molina Bacca
#' @examples
#' \dontrun{
#' x <- prices(gdx)
#' }
#'
#' @importFrom magpiesets findset

prices <- function(gdx, file = NULL, level = "reg", products = "kall", product_aggr = FALSE, attributes = "dm", # nolint
                   type = "consumer", glo_weight = "production") {                                              # nolint
  if (!glo_weight %in% c("production", "export", "free_trade")) {
    stop("Weighting scheme not supported. Available options: ~export~, ~production~ and ~free_trade~")
  }
  productCheck <- products
  if (!all(products %in% readGDX(gdx, "kall"))) products <- readGDX(gdx, products)

  # consumer prices are based on supply/demand constraints
  if (type == "consumer") {

    p <- mbind(readGDX(gdx, "oq16_supply_crops", select = list(type = "marginal"), react = "warning"),
      readGDX(gdx, "oq16_supply_livestock", select = list(type = "marginal"), react = "warning"),
      readGDX(gdx, "oq16_supply_secondary", select = list(type = "marginal"), react = "warning"),
      readGDX(gdx, "oq16_supply_residues", select = list(type = "marginal"), react = "warning"),
      setNames(readGDX(gdx, "oq16_supply_pasture", select = list(type = "marginal"), react = "warning"), "pasture"))

    # add forest products
    forestry <- suppressWarnings(readGDX(gdx, "oq16_supply_forestry", select = list(type = "marginal"),
                                         react = "warning"))
    if (is.null(forestry)) {
      forestry <- setNames(p[, , seq_along(readGDX(gdx, "kforestry"))] * 0, readGDX(gdx, "kforestry"))
    }
    p <- mbind(p, forestry)

    d <- readGDX(gdx, "ov_supply", select = list(type = "level"), react = "warning")
    # unit conversion
    if (length(attributes) == 1) {
      att <- collapseNames(readGDX(gdx, "fm_attributes")[, , attributes])
      p <- p[, , products] / att[, , products]
    } else stop("Only one unit attribute is possible here!")
    # subset products
    p <- p[, , products]
    d <- d[, , products]
    # regional and product aggregation
    p <- superAggregateX(p, aggr_type = "weighted_mean", level = level, weight = d, crop_aggr = product_aggr)
  } else if (type == "producer") {

    pTradeReg <- readGDX(gdx, "oq21_trade_reg", select = list(type = "marginal"), react = "silent")
    pTradeGlo <- readGDX(gdx, "oq21_trade_glo", "oq_trade_glo", select = list(type = "marginal"), react = "silent")

    if(!is.null(pTradeReg) && !is.null(pTradeGlo)) {
      # producer prices are based on trade constraints
      # regional shadow price for traded goods (k_trade)
      pTradeReg <- readGDX(gdx, "oq21_trade_reg", select = list(type = "marginal"), react = "warning")
      # regional shadow price for non-traded goods (k_notrade)
      pTradeRegNt <- readGDX(gdx, "oq21_notrade", select = list(type = "marginal"), react = "warning")
      # global shadow price for all traded goods (k_trade)
      pTradeGlo <- readGDX(gdx, "oq21_trade_glo", "oq_trade_glo", select = list(type = "marginal"), react = "warning")
      # glue together regional prices: "kall"
      if (!is.null(pTradeReg)) {
        pTradeReg <- mbind(pTradeReg, pTradeRegNt)
      } else {
        pTradeReg <- mbind(pTradeRegNt, new.magpie(getRegions(pTradeRegNt), getYears(pTradeRegNt),
                                                   getNames(pTradeGlo), 0))
      }
      # extend pTradeGlo by k_notrade; global prices prices for non traded goods are 0.
      pTradeGlo <- mbind(pTradeGlo, new.magpie(getRegions(pTradeGlo), getYears(pTradeGlo), getNames(pTradeRegNt), 0))
      # unit conversion
      if (length(attributes) == 1) {
        if (suppressWarnings(is.null(readGDX(gdx, "fcostsALL"))) && attributes == "dm" && productCheck == "kall") {
          att <- collapseNames(readGDX(gdx, "fm_attributes")[, , attributes])
          pTradeReg <- pTradeReg * att[, , getNames(pTradeReg)]
          pTradeGlo <- pTradeGlo * att[, , getNames(pTradeGlo)]
        } else {
          att <- collapseNames(readGDX(gdx, "fm_attributes")[, , attributes])
          pTradeReg <- pTradeReg * att[, , getNames(pTradeReg)]
          pTradeGlo <- pTradeGlo * att[, , getNames(pTradeGlo)]
        }
      } else stop("Only one unit attribute is possible here!")
      # regional producer price: sum of regional and global prices from trade constraints
      pTrade <- pTradeGlo + pTradeReg
    } else { #case for highres runs without trade
      pTrade <- readGDX(gdx, "oq21_notrade", select = list(type = "marginal"), react = "warning")
      # replace 0 with min price as proxy for global price
      pTrade[pTrade==0]<-NA
      # min as proxy for global price
      minVal<-suppressWarnings(as.magpie(apply(pTrade,c(2,3),min,na.rm=TRUE)))
      minVal[is.infinite(minVal)]<-NA
      #replace NA with min value for each region, time step and item
      for (reg in getItems(pTrade, 1)) pTrade[reg, , ][is.na(pTrade[reg, , ])] <- minVal["GLO", , ][is.na(pTrade[reg, , ])]

      # unit conversion
      if (length(attributes) == 1) {
        if (suppressWarnings(is.null(readGDX(gdx, "fcostsALL"))) && attributes == "dm" && productCheck == "kall") {
          att <- collapseNames(readGDX(gdx, "fm_attributes")[, , attributes])
          pTrade <- pTrade * att[, , getNames(pTrade)]
        } else {
          att <- collapseNames(readGDX(gdx, "fm_attributes")[, , attributes])
          pTrade <- pTrade * att[, , getNames(pTrade)]
        }
      } else stop("Only one unit attribute is possible here!")
    }

    # subset products
    if (length(setdiff(products, getNames(pTrade))) != 0) {
      products <- getNames(pTrade)
    }
    pTrade <- pTrade[, , products]

    # bring superregional data back to regional level, if necessary
    supreg <- readGDX(gdx, "supreg", react = "silent")
    if (!is.null(supreg) && any(supreg$h != supreg$i)) {
      pTrade <- toolAggregate(pTrade, supreg)
    }

    # production as weight
    q <- production(gdx, level = "reg", products = products, product_aggr = FALSE)
    # regional and product aggregation
    p <- superAggregateX(pTrade, aggr_type = "weighted_mean", level = level, weight = q, crop_aggr = product_aggr)

    # Global prices can be calculated based on different weights
    if ("GLO" %in% getRegions(p)) {
      if (glo_weight == "production") p["GLO", , ] <- p["GLO", , ] # keep as is
      else if (glo_weight == "export") {
        exports <- trade(gdx, level = "reg", products = "kall", product_aggr = FALSE, type = "exports")
        years <- getYears(exports)
        mnames <- getNames(exports)
        # global price
        pGLO <- new.magpie(cells_and_regions = "GLO", years = years, names = mnames)
        for (year in years) {
          for (name in mnames) {
            pGLO[1, year, name] <- weighted.mean(x = pTrade[, year, name], w = exports[, year, name], na.rm = TRUE)
          }
        }
        pGLO[is.nan(pGLO)] <- 0
        p["GLO", , ] <- superAggregateX(pGLO, aggr_type = "weighted_mean", level = "glo", weight = dimSums(q, dim = 1),
                                        crop_aggr = product_aggr)
      } else if (glo_weight == "free_trade") {
        if (product_aggr) {
          p["GLO", , ] <- superAggregateX(pTradeGlo, aggr_type = "weighted_mean", level = "glo",
                                          weight = dimSums(q, dim = 1), crop_aggr = TRUE)
        } else {
          p["GLO", , ] <- pTradeGlo[, , products]
        }
      }
      # set nan prices to zero
      p[is.nan(p)] <- 0
    }
  } else stop("Price type not not supported. Available options: ~consumer~ and ~producer~")
  # set nan prices to zero
  p[is.nan(p)] <- 0
  out(p, file)
}
