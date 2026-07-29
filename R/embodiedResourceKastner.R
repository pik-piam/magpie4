#' @title embodiedResourceKastner
#' @description Generic consumption-based (embodied) resource footprint using a
#'   column-normalised Kastner (2011) allocation. Each producing region's
#'   resource total (e.g. cropland, water, emissions, labour) is distributed to
#'   consuming regions using the bilateral commodity consumption shares from the
#'   Kastner \eqn{(I-A)^{-1}} solution, normalised so that each origin's column
#'   sums exactly to its actual resource. This guarantees, by construction:
#'   \itemize{
#'     \item non-negativity of the consumption footprint (if the resource is non-negative);
#'     \item production[r] == resource[r];
#'     \item additivity: production - exports + imports == consumption;
#'     \item global closure: sum(consumption) == sum(resource).
#'   }
#'   Crop/pasture products are allocated through primary-equivalent (feed-traced)
#'   trade; livestock products (\code{kli}) through direct bilateral trade.
#'   The prim/secd/feed pathway split is derived from demand-category shares
#'   (processed -> secd, feed -> feed, remainder -> prim), as in
#'   \code{\link{embodiedLand}}/\code{\link{embodiedWater}}.
#'
#' @export
#'
#' @param gdx GDX file
#' @param resource MAgPIE object (region, year, product) of the resource total
#'   per product (production basis). Products may include crop/pasture and
#'   livestock (kli) products; the function routes them to the correct trade
#'   matrix automatically.
#' @param file optional file name to write the result with \code{write.magpie}
#' @param level regional aggregation level (only "reg" supported for now)
#' @param type "production", "consumption", "trade", or "all" (default); ignored
#'   when \code{bilateral = TRUE}
#' @param bilateral logical; if TRUE, returns the bilateral consumption
#'   allocation with dimensions (exporter.importer, year, pathway.product) where
#'   exporter = production origin and importer = consumer. Default FALSE.
#' @param secdToFeed logical; if TRUE, the share of each primary product that is
#'   processed and then fed to livestock (e.g. soybean -> oilcake -> feed) is moved
#'   from the secd pathway to the feed pathway. The processed-then-fed share is
#'   sSecd * phi, where phi[primary] is the feed fraction of the secondary products
#'   derived from that primary (mass allocation via \code{primaryPerSecondary}).
#'   Default TRUE (processed soybean etc. is moved to feed, keyed on its immediate
#'   demand category).
#' @param reassignLivestock logical; if TRUE (default) every livestock (\code{kli})
#'   product's whole footprint (all pathways) is moved into the \code{feed}
#'   (Livestock) pathway via \code{\link{reassignLivestockPathway}}, so the feed
#'   pathway carries the FULL livestock footprint (feed crops PLUS the livestock
#'   products' own enteric/manure/labour/water footprint) rather than only the
#'   feed crops. Orthogonal to \code{secdToFeed} (which re-routes a crop's
#'   processed-then-fed share); a no-op for resources without \code{kli} products
#'   (e.g. land). Per-product totals are conserved either way.
#'
#' @return MAgPIE object. When \code{bilateral = FALSE}: (region, year,
#'   accounting.pathway.product) with accounting in \{production, consumption,
#'   export, import, net-trade\} and pathway in \{prim, secd, feed\}. When
#'   \code{bilateral = TRUE}: (exporter.importer, year, pathway.product).
#' @author David M Chen
#' @importFrom magclass getItems getYears getRegions dimSums mbind setNames collapseNames add_dimension new.magpie
#' @importFrom gdx2 readGDX

embodiedResourceKastner <- function(gdx, resource, file = NULL, level = "reg",
                                    type = "all", bilateral = FALSE, secdToFeed = TRUE,
                                    reassignLivestock = TRUE) {

  if (level != "reg") stop("embodiedResourceKastner currently supports level = 'reg' only.")

  # local masked-multiply helper (factor keyed on the UNmasked spatial subdim)
  arf <- function(x, factor, maskDim) {
    getItems(x, dim = maskDim) <- paste0(getItems(x, dim = maskDim), "_tmp")
    r <- x * factor
    getItems(r, dim = maskDim) <- sub("_tmp$", "", getItems(r, dim = maskDim))
    r
  }

  kli       <- readGDX(gdx, "kli")
  rProducts <- getItems(resource, dim = 3)
  liProds   <- intersect(rProducts, kli)
  cropProds <- setdiff(rProducts, kli)
  regions   <- getItems(resource, dim = 1)
  selfself  <- paste(regions, regions, sep = ".")

  # ---------------------------------------------------------------------------
  # 1. Bilateral commodity CONSUMPTION matrices (Kastner, diagonal kept)
  #    crop/pasture -> primary-equivalent trade; livestock -> direct trade.
  # ---------------------------------------------------------------------------
  consMats <- list()
  if (length(cropProds) > 0) {
    gross  <- tradedPrimaries(gdx, bilateral = TRUE, convFactor = "exporter",
                              kastner = FALSE, level = level)
    grossC <- dimSums(gross, dim = 3.1)                       # ex.im x year x product (primary eq.)
    cp <- intersect(cropProds, getItems(grossC, dim = 3))
    if (length(cp) > 0)
      consMats$crop <- tradeKastner(gdx, trade = grossC[, , cp], level = level,
                                    products = "kall", attributes = "dm", selfselfTrade = FALSE)
  }
  if (length(liProds) > 0) {
    tradeRaw <- collapseNames(readGDXBilateral(gdx, "ov21_trade"))[, , "level", drop = TRUE]
    lp <- intersect(liProds, getItems(tradeRaw, dim = 3))
    if (length(lp) > 0)
      consMats$liv <- tradeKastner(gdx, trade = tradeRaw[, , lp], level = level,
                                   products = "kall", attributes = "dm", selfselfTrade = FALSE)
  }
  kcons  <- mbind(consMats)                                   # ex.im x year x product
  cyears <- getYears(kcons)
  aProd  <- getItems(kcons, dim = 3)

  # ---------------------------------------------------------------------------
  # 2. Column-normalise and distribute the actual resource of each origin
  # ---------------------------------------------------------------------------
  colsum <- dimSums(kcons, dim = 1.2)                         # by origin (ex)
  recip  <- 1 / colsum; recip[!is.finite(recip)] <- 0
  shares <- arf(kcons, recip, maskDim = 1.2)                  # share[ex,im] = kcons / colsum[ex]
  alloc  <- arf(shares, resource[, cyears, aProd], maskDim = 1.2)  # x resource[ex]

  # ---------------------------------------------------------------------------
  # demand-category shares for the prim/secd/feed split (region's own)
  # ---------------------------------------------------------------------------
  dem      <- demand(gdx, level = level)[, , "dom_balanceflow", invert = TRUE]
  dem      <- dem[, cyears, ]
  totReg   <- dimSums(dem, dim = 3.1)                  # total demand per product, by region
  demRatio <- dem / totReg
  demRatio[!is.finite(demRatio)] <- 0
  # Fallback for regions that bear a product's footprint (via imported secondary or
  # livestock goods) but have ZERO local demand for that primary product: without
  # this their 0/0 -> 0 shares dump the whole footprint into prim. e.g. palm oil is
  # processed in the producing country and imported as OIL, so temperate importers
  # have no oilpalm demand and oilpalm would wrongly show a prim footprint. Use the
  # GLOBAL demand mix of the product for those zero-demand cells (oilpalm -> secd).
  gloMix <- dimSums(dem, dim = 1)                      # global, by category.product
  gloMix <- gloMix / dimSums(gloMix, dim = 3.1)
  gloMix[!is.finite(gloMix)] <- 0
  regs0     <- getItems(dem, dim = 1)
  gloMixReg <- mbind(lapply(regs0, function(r) setItems(gloMix, dim = 1, r)))  # broadcast to all regions
  demRatio  <- demRatio + gloMixReg * (totReg == 0)    # fill zero-demand cells with global mix
  sSecdProd <- collapseNames(demRatio[, , "processed"])
  sFeedProd <- collapseNames(demRatio[, , "feed"])

  # Optional: re-attribute the processed-then-fed share (e.g. soybean -> oilcake ->
  # livestock feed) from the secd pathway to the feed pathway. phi[primary] is the
  # feed fraction of the secondary products derived from that primary, in primary
  # equivalents (mass allocation via primaryPerSecondary). The moved share is
  # sSecd * phi, so prim is unchanged and prim + secd + feed still sums to 1.
  if (secdToFeed) {
    pps     <- primaryPerSecondary(gdx, level = level, allocation = "value")[, cyears, ]
    secProd <- getItems(pps, dim = 3.1)                                       # secondary (ksd)
    demSec  <- dem[, , secProd]
    feedPE  <- dimSums(collapseNames(demSec[, , "feed"]) * pps, dim = 3.1)    # prim-eq fed via processing
    totPE   <- dimSums(dimSums(demSec, dim = 3.1) * pps,        dim = 3.1)    # prim-eq of all processing
    phi     <- feedPE / totPE; phi[!is.finite(phi)] <- 0                      # per primary product, in [0,1]
    phiFull <- sSecdProd; phiFull[, , ] <- 0
    cphi    <- intersect(getItems(phiFull, dim = 3), getItems(phi, dim = 3))
    phiFull[, , cphi] <- phi[, , cphi]
    moved     <- sSecdProd * phiFull
    sFeedProd <- sFeedProd + moved
    sSecdProd <- sSecdProd - moved
  }

  # ---------------------------------------------------------------------------
  # BILATERAL output: split the allocation by the CONSUMER's (im) pathway
  # ---------------------------------------------------------------------------
  if (bilateral) {
    pr  <- intersect(aProd, getItems(demRatio, dim = 3.2))
    am  <- alloc[, , pr]
    getItems(am, dim = 1.1) <- paste0(getItems(am, dim = 1.1), "_tmp")   # mask ex -> shares match im
    secd <- am * sSecdProd[, , pr]
    feed <- am * sFeedProd[, , pr]
    prim <- am - secd - feed
    bil <- mbind(add_dimension(prim, dim = 3.1, add = "pathway", nm = "prim"),
                 add_dimension(secd, dim = 3.1, add = "pathway", nm = "secd"),
                 add_dimension(feed, dim = 3.1, add = "pathway", nm = "feed"))
    getItems(bil, dim = 1.1) <- sub("_tmp$", "", getItems(bil, dim = 1.1))
    if (reassignLivestock) bil <- reassignLivestockPathway(bil, kli = kli)
    if (!is.null(file)) write.magpie(bil, file_name = file)
    return(bil)
  }

  # ---------------------------------------------------------------------------
  # REGIONAL accounting
  # ---------------------------------------------------------------------------
  productionA  <- dimSums(alloc, dim = 1.2)                   # by ex  == resource[,,aProd]
  consumptionA <- dimSums(alloc, dim = 1.1)                   # by im
  diagA        <- dimSums(alloc[selfself, , ], dim = 1.2)     # domestic, by region
  importsA     <- consumptionA - diagA
  exportsA     <- productionA  - diagA

  # products present in the resource but not in any trade matrix -> pure domestic
  rProd <- setdiff(rProducts, aProd)
  bind0 <- function(x, dom0) if (length(rProd) > 0) mbind(x, dom0) else x
  res0  <- resource[, cyears, ]
  production  <- bind0(productionA,  res0[, , rProd])
  consumption <- bind0(consumptionA, res0[, , rProd])
  imports     <- bind0(importsA, res0[, , rProd] * 0)
  exports     <- bind0(exportsA, res0[, , rProd] * 0)
  netTrade    <- imports - exports

  # secd = processed-demand share, feed = feed-demand share, prim = the rest.
  # Defining prim = F - secd - feed (rather than summing the remaining demand
  # categories) keeps prim + secd + feed == F exactly, so the footprint is fully
  # allocated even where a product has zero (or only balanceflow) demand.
  splitPath <- function(F) {
    pr    <- intersect(getItems(F, dim = 3), getItems(demRatio, dim = 3.2))
    F     <- F[, , pr]
    secd  <- F * sSecdProd[, , pr]
    feed  <- F * sFeedProd[, , pr]
    prim  <- F - secd - feed
    mbind(add_dimension(prim, dim = 3.1, add = "pathway", nm = "prim"),
          add_dimension(secd, dim = 3.1, add = "pathway", nm = "secd"),
          add_dimension(feed, dim = 3.1, add = "pathway", nm = "feed"))
  }
  production  <- splitPath(production)
  consumption <- splitPath(consumption)
  exports     <- splitPath(exports)
  imports     <- splitPath(imports)
  netTrade    <- splitPath(netTrade)

  acc <- function(x, nm) add_dimension(x, dim = 3.1, add = "accounting", nm = nm)
  out <- switch(type,
    production  = acc(production,  "production"),
    consumption = acc(consumption, "consumption"),
    trade       = mbind(acc(exports, "export"), acc(imports, "import"), acc(netTrade, "net-trade")),
    all         = mbind(acc(production, "production"), acc(consumption, "consumption"),
                        acc(exports, "export"), acc(imports, "import"), acc(netTrade, "net-trade")),
    stop("Invalid type. Choose 'production', 'consumption', 'trade', or 'all'."))

  if (reassignLivestock) out <- reassignLivestockPathway(out, kli = kli)
  if (!is.null(file)) write.magpie(out, file_name = file)
  return(out)
}
