#' @title reportFootprints
#' @description Reports the consumption-based (embodied) resource footprints of
#'   MAgPIE results in the standard reporting-variable hierarchy for shiny/mip.
#'   Built on \code{\link{footprints}} / \code{\link{embodiedResourceKastner}} (the
#'   column-normalised Kastner allocation), so each variable is the amount of the
#'   resource embodied in the products a region CONSUMES (production footprint of
#'   the goods consumed there, including via imports). Three metrics are reported
#'   under a common tree, \code{Footprints|<metric>|<Resource>|...}:
#'   \itemize{
#'     \item \strong{Total} — absolute footprint (Mha, km3, Mt CO2eq, mio people);
#'     \item \strong{Per-Capita} — footprint divided by population;
#'     \item \strong{Per-Tonne} — footprint per tonne of the final product consumed
#'       in each pathway.
#'   }
#'   For \emph{Total} and \emph{Per-Capita} the product tree (Crops -> Cereals/Oil
#'   crops/..., Livestock products, Pasture, Bioenergy crops, Forage) is reported
#'   with \code{+} summation markers and the prim/secd/feed pathway split
#'   (Primary/Secondary/Livestock end use) with \code{++} markers; both partitions
#'   sum to the resource total. Per-Capita stays additive because every product and
#'   pathway shares the same population denominator (and the global value is the
#'   global footprint divided by global population, not the sum of regional
#'   per-capita values). \emph{Per-Tonne} is NOT additive — each pathway has its own
#'   tonnes denominator (primary demand / secondary (ksd) demand / livestock (kli)
#'   demand) — so it is reported only at the pathway level as flat variables with NO
#'   summation markers and no grand total.
#'
#'   NB requires a BILATERAL MAgPIE run (bilateral trade in the GDX). It is called
#'   from \code{\link{getReport}} once per resource (so each is a right-sized
#'   worker in the parallel report pool rather than one worker holding all four).
#'   On standard runs the bilateral trade matrix (\code{ov21_trade} with an
#'   exporter-importer dimension) is absent, so the function emits a message and
#'   returns \code{NULL}; the reporting wrapper then simply omits the footprint
#'   variables.
#'
#' @export
#'
#' @param gdx GDX file (bilateral MAgPIE run).
#' @param level spatial aggregation: "reg", "glo" or "regglo" (default). The
#'   footprint is computed at "reg"; for the global total the absolute footprint
#'   and each denominator are summed over regions and only then divided, so the
#'   per-capita/per-tonne globals are correct intensive quantities.
#' @param resources character vector of resources to report; any subset of
#'   \code{c("land", "emissions", "water", "labor")}.
#' @param reassignLivestock logical; if TRUE (default) the Livestock pathway
#'   carries the full livestock footprint (kli products' own footprint moved into
#'   feed). See \code{\link{reassignLivestockPathway}}.
#' @param secdToFeed logical; if TRUE (default) route each crop's
#'   processed-then-fed share from secd to feed (and drop the fed-secondary share
#'   from the per-tonne secd denominator). See \code{\link{embodiedResourceKastner}}.
#'
#' @return consumption footprints as a MAgPIE object with reporting names, or
#'   \code{NULL} (with a message) if the GDX is not a bilateral trade run.
#' @author David M Chen
#' @seealso \code{\link{footprints}}, \code{\link{embodiedResourceKastner}}
#' @importFrom magclass mbind getNames setNames getItems getYears dimSums collapseNames
#' @importFrom magpiesets reporthelper summationhelper
#' @importFrom gdx2 readGDX
#' @examples
#' \dontrun{
#'   x <- reportFootprints(gdx)
#' }
#'
#' @section Footprint variables (Land shown; analogous for Emissions/Water/Labor):
#' Name | Unit | Meta
#' ---|---|---
#' Footprints\|Total\|Land | million ha | Total consumption-based land footprint
#' Footprints\|Total\|Land\|+\|Crops | million ha | Land embodied in crops consumed
#' Footprints\|Total\|Land\|++\|Livestock | million ha | Footprint consumed via livestock (feed + kli own)
#' Footprints\|Per-Capita\|Land | ha / capita | Land footprint per capita (additive)
#' Footprints\|Per-Tonne\|Land\|Primary | ha / t | Land per tonne of primary product eaten directly
#' Footprints\|Per-Tonne\|Land\|Secondary | ha / t | Land per tonne of secondary (processed) product consumed
#' Footprints\|Per-Tonne\|Land\|Livestock | ha / t | Land per tonne of livestock product consumed
#' @md

reportFootprints <- function(gdx, level = "regglo",
                             resources = c("land", "emissions", "water", "labor"),
                             reassignLivestock = TRUE, secdToFeed = TRUE) {

  # Footprints require a BILATERAL trade run: ov21_trade must carry the
  # exporter x importer (i_im) dimension. On standard runs ov21_trade is regional
  # (or the symbol is absent), so readGDXBilateral() returns NULL or errors on the
  # missing i_im set. Detect that up front and skip cleanly rather than letting the
  # embodied pipeline throw deeper down (readGDXBilateral / tradeKastner).
  bilatTrade <- tryCatch(readGDXBilateral(gdx, "ov21_trade"), error = function(e) NULL)
  if (is.null(bilatTrade)) {
    message("reportFootprints: no bilateral trade found footprints are only reported for ",
            "bilateral trade runs. Skipping.")
    return(NULL)
  }

  resInfo <- list(
    land      = list(lab = "Land",      total = "million ha", perCapita = "ha / capita",      perTonne = "ha / t"),
    emissions = list(lab = "Emissions", total = "Mt CO2eq",   perCapita = "t CO2eq / capita", perTonne = "t CO2eq / t"),
    water     = list(lab = "Water",     total = "km3",        perCapita = "1000 m3 / capita", perTonne = "1000 m3 / t"),
    labor     = list(lab = "Labor",     total = "mio people", perCapita = "people / capita",  perTonne = "people / t"))

  pathLabels <- c(prim = "Primary", secd = "Secondary", feed = "Livestock")
  kli <- readGDX(gdx, "kli")
  ksd <- readGDX(gdx, "ksd")

  # add a global (region-sum) entry for glo/regglo; only valid for additive
  # (absolute) quantities, so numerator and denominators are aggregated here
  # BEFORE any per-capita / per-tonne division.
  rg <- function(z) {
    if (!(level %in% c("glo", "regglo"))) return(z)
    g <- dimSums(z, dim = 1)
    getItems(g, dim = 1) <- "GLO"
    if (level == "glo") g else mbind(z, g)
  }

  # additive hierarchy: grand total + product tree (+) + pathway split (++).
  # `cons` = consumption footprint (region, year, pathway.product).
  additiveTree <- function(cons, base, unit) {
    total <- setNames(dimSums(cons, dim = 3), paste0(base, " (", unit, ")"))
    prodOnly <- dimSums(cons, dim = "pathway")                # region, year, product
    out <- reporthelper(prodOnly, dim = 3.1, level_zero_name = base, detail = TRUE)
    getNames(out) <- paste0(gsub("\\.", "|", getNames(out)), " (", unit, ")")
    out <- summationhelper(out, sep = "+")
    pathOnly <- dimSums(cons, dim = 3.2)                      # region, year, pathway
    getItems(pathOnly, dim = 3) <- pathLabels[getItems(pathOnly, dim = 3)]
    getNames(pathOnly) <- paste0(base, "|", getNames(pathOnly), " (", unit, ")")
    out2 <- summationhelper(pathOnly, sep = "++")
    mbind(total, out, out2)
  }

  x <- NULL
  for (r in resources) {
    if (!(r %in% names(resInfo))) stop("Unknown resource '", r, "'.")
    info <- resInfo[[r]]; lab <- info$lab

    # absolute consumption footprint by pathway.product (reg)
    ftot    <- footprints(gdx, resource = r, type = "total", level = "reg",
                          reassignLivestock = reassignLivestock, secdToFeed = secdToFeed)
    consAbs <- collapseNames(ftot[, , "consumption"])         # pathway.product
    prods   <- getItems(consAbs, dim = 3.2)

    # denominators at reg (aggregate to regglo separately from the numerator)
    pop   <- population(gdx, level = "reg")
    dem   <- demand(gdx, level = "reg")[, , "dom_balanceflow", invert = TRUE]
    denom <- footprintDemand(dem, prods, kli = kli, ksd = ksd, secdToFeed = secdToFeed)  # pathway

    consAbsRG <- rg(consAbs); popRG <- rg(pop); denomRG <- rg(denom)

    # --- Total (absolute, additive) -----------------------------------------
    x <- mbind(x, additiveTree(consAbsRG, paste0("Footprints|Total|", lab), info$total))

    # --- Per-Capita (additive: shared population denominator) ----------------
    cyPC   <- intersect(getYears(consAbsRG), getYears(popRG))
    consPC <- consAbsRG[, cyPC, ] / popRG[, cyPC, ]
    x <- mbind(x, additiveTree(consPC, paste0("Footprints|Per-Capita|", lab), info$perCapita))

    # --- Per-Tonne (NOT additive: pathway-specific tonnes) -------------------
    # flat pathway-level variables, no summation markers, no grand total.
    cyPT   <- intersect(getYears(consAbsRG), getYears(denomRG))
    consPT <- dimSums(consAbsRG, dim = 3.2)[, cyPT, ] / denomRG[, cyPT, ]   # region, year, pathway
    getItems(consPT, dim = 3) <- pathLabels[getItems(consPT, dim = 3)]
    getNames(consPT) <- paste0("Footprints|Per-Tonne|", lab, "|", getNames(consPT),
                               " (", info$perTonne, ")")
    x <- mbind(x, consPT)
  }

  return(x)
}
