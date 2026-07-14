#' @title footprints
#' @description Consumption-based (embodied) resource footprints as ready-to-use
#'   MAgPIE objects, on top of the column-normalised Kastner allocation in
#'   \code{\link{embodiedResourceKastner}}. A single entry point that (a) chooses
#'   the resource (\code{land}, \code{water}, \code{emissions} or \code{labor}),
#'   (b) derives regional production / consumption / import / export / net-trade
#'   by prim/secd/feed pathway and product from the bilateral Kastner allocation,
#'   and (c) returns the footprint as an absolute total, per capita, or per tonne
#'   of the final product consumed in each pathway.
#'
#'   This is the reusable computation extracted from the bilateral footprint
#'   plotting scripts, so any MAgPIE run can obtain the same numbers without the
#'   plotting layer. Regional accounting is derived from the bilateral object by
#'   aggregation: production = sum over importer; consumption = sum over exporter;
#'   domestic = the self-trade diagonal; imports = consumption - domestic; exports
#'   = production - domestic. The pathway split of every accounting component is
#'   therefore keyed on the CONSUMER's demand mix (consistent bilateral basis).
#'
#' @export
#'
#' @param gdx GDX file (must contain bilateral trade, i.e. a bilateral MAgPIE run).
#' @param resource which resource footprint: "land", "water", "emissions" or
#'   "labor" (alias "labour").
#' @param type the footprint metric:
#'   \itemize{
#'     \item \code{"total"} (default): absolute embodied footprint
#'       (region, year, accounting.pathway.product), in the resource's own unit
#'       (Mha, km3, Mt CO2eq, mio people);
#'     \item \code{"perCapita"}: total divided by population
#'       (region, year, accounting.pathway.product); unit is resource-per-person
#'       (e.g. Mha / mio people = ha / capita);
#'     \item \code{"perTonne"}: footprint per tonne of the FINAL product consumed
#'       in each pathway (region, year, accounting.pathway), products summed within
#'       each pathway. The denominator is primary demand (prim), secondary (ksd)
#'       demand consumed as food (secd) and livestock (kli) demand (feed); see
#'       \code{secdToFeed}. Per-tonne is a pathway-level quantity, so the product
#'       dimension is collapsed.
#'   }
#' @param level regional aggregation level (only "reg" supported by the underlying
#'   embodied* functions).
#' @param file optional file name to write the result with \code{write.magpie}.
#' @param reassignLivestock logical; if TRUE (default) move every livestock
#'   product's whole footprint into the feed (Livestock) pathway, so the feed
#'   pathway carries the full livestock footprint. See
#'   \code{\link{reassignLivestockPathway}}.
#' @param secdToFeed logical; if TRUE (default) route each crop's processed-then-fed
#'   share from secd to feed, and (for \code{type = "perTonne"}) drop the fed share
#'   of secondary products from the secd denominator so it reads per tonne of
#'   secondary eaten as food. See \code{\link{embodiedResourceKastner}}.
#' @param bil optional precomputed RAW bilateral embodied object (exporter.importer,
#'   year, pathway.product) from \code{embodied*Kastner(bilateral = TRUE,
#'   reassignLivestock = FALSE)}. When supplied the expensive Kastner computation
#'   is skipped and this object is used directly (reassignment, if requested, is
#'   applied here). Enables reuse of a cached bilateral allocation.
#' @param dem optional precomputed demand object (region, year, demand.product),
#'   as returned by \code{demand(gdx, level)} WITHOUT the dom_balanceflow category;
#'   only used for \code{type = "perTonne"}. Read from \code{gdx} if not supplied.
#' @param ... further resource-specific arguments passed to the chosen
#'   \code{embodied*Kastner} wrapper (e.g. \code{landType}, \code{waterType},
#'   \code{unit}); ignored when \code{bil} is supplied.
#'
#' @return MAgPIE object; layout depends on \code{type} (see above).
#' @author David M Chen
#' @seealso \code{\link{embodiedResourceKastner}}, \code{\link{reportFootprints}},
#'   \code{\link{footprintDemand}}, \code{\link{reassignLivestockPathway}}
#' @examples
#' \dontrun{
#'   land   <- footprints(gdx, "land", type = "total")       # Mha, by product & pathway
#'   emisPC <- footprints(gdx, "emissions", type = "perCapita")
#'   waterT <- footprints(gdx, "water", type = "perTonne")   # by pathway
#' }

footprints <- function(gdx, resource = "land", type = "total", level = "reg",
                       file = NULL, reassignLivestock = TRUE, secdToFeed = TRUE,
                       bil = NULL, dem = NULL, ...) {

  if (level != "reg") stop("footprints currently supports level = 'reg' only.")
  type <- match.arg(type, c("total", "perCapita", "perTonne"))

  kli <- readGDX(gdx, "kli")
  ksd <- readGDX(gdx, "ksd")

  # --------------------------------------------------------------------------
  # Regional accounting by pathway.product, derived from the bilateral allocation.
  # Keeps the product dimension. Returns (region, year, accounting.pathway.product)
  # with accounting in {production, consumption, import, export, net-trade}.
  # Kept as a local helper: only footprints() needs it; the plotting scripts carry
  # their own copy (plotting stays self-contained).
  # --------------------------------------------------------------------------
  .deriveFootprintFlows <- function(bil) {
    regions <- getItems(bil, dim = 1.1)
    self    <- paste(regions, regions, sep = ".")
    asReg   <- function(x) { names(dimnames(x))[1] <- "region"; x }

    prod <- asReg(dimSums(bil, dim = 1.2))                 # by exporter (origin)
    cons <- asReg(dimSums(bil, dim = 1.1))                 # by importer (consumer)
    diag <- asReg(dimSums(bil[self, , ], dim = 1.2))       # domestic (self-trade)
    imp  <- cons - diag
    exp  <- prod - diag
    net  <- imp - exp

    acc <- function(x, nm) add_dimension(x, dim = 3.1, add = "accounting", nm = nm)
    mbind(acc(prod, "production"), acc(cons, "consumption"),
          acc(imp, "import"), acc(exp, "export"), acc(net, "net-trade"))
  }

  # --- 1. bilateral embodied allocation (RAW pathway split) -------------------
  if (is.null(bil)) {
    fn <- switch(resource,
                 land      = embodiedLand,
                 water     = embodiedWater,
                 emissions = embodiedEmissions,
                 labor     = embodiedLabor,
                 labour    = embodiedLabor,
                 stop("Invalid resource. Choose 'land', 'water', 'emissions' or 'labor'."))
    bil <- fn(gdx, level = level, bilateral = TRUE, secdToFeed = secdToFeed,
              reassignLivestock = FALSE, ...)
  }
  if (reassignLivestock) bil <- reassignLivestockPathway(bil, kli = kli)

  # --- 2. regional accounting by pathway.product (aggregation of bilateral) ---
  flows <- .deriveFootprintFlows(bil)

  # --- 3. metric transform ----------------------------------------------------
  if (type == "total") {
    out <- flows

  } else if (type == "perCapita") {
    pop <- population(gdx, level = level)
    cy  <- intersect(getYears(flows), getYears(pop))
    out <- flows[, cy, ] / pop[, cy, ]

  } else if (type == "perTonne") {
    if (is.null(dem)) dem <- demand(gdx, level = level)[, , "dom_balanceflow", invert = TRUE]
    prods <- getItems(flows, dim = 3.3)
    denom <- footprintDemand(dem, prods, kli = kli, ksd = ksd, secdToFeed = secdToFeed)
    flowsP <- dimSums(flows, dim = 3.3)          # sum products within pathway -> accounting.pathway
    cy <- intersect(getYears(flowsP), getYears(denom))
    # divide each accounting component by its pathway's demand (matched by pathway)
    out <- flowsP[, cy, ] / denom[, cy, ]
  }

  if (!is.null(file)) write.magpie(out, file_name = file)
  return(out)
}
