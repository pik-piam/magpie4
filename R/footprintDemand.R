#' @title footprintDemand
#' @description Per-tonne denominator for consumption-based (embodied) resource
#'   footprints: the tonnes of the FINAL product consumed in each prim/secd/feed
#'   pathway. Used by \code{\link{footprints}} (\code{type = "perTonne"}) and
#'   \code{\link{reportFootprints}} to turn an absolute footprint into a per-tonne
#'   intensity that is comparable across resources.
#'   \itemize{
#'     \item \code{prim} = primary products eaten directly (all demand categories
#'       except the processed and feed categories);
#'     \item \code{secd} = tonnes of SECONDARY (ksd) products consumed (the
#'       processing OUTPUT, not the primary input); with \code{secdToFeed = TRUE}
#'       the fed-secondary share is excluded so it reads per tonne of secondary
#'       eaten as food/non-feed;
#'     \item \code{feed} = tonnes of LIVESTOCK (kli) products consumed.
#'   }
#'
#' @export
#'
#' @param dem demand object (region, year, demand.product) as returned by
#'   \code{demand(gdx, level)}, with the \code{dom_balanceflow} category already
#'   removed.
#' @param prods products present in the footprint (used to restrict the primary
#'   set); typically \code{getItems(<footprint>, dim = "product")}.
#' @param kli livestock product set (\code{readGDX(gdx, "kli")}).
#' @param ksd secondary (processed) product set (\code{readGDX(gdx, "ksd")}).
#' @param secdToFeed logical; if TRUE (default) drop the fed share of secondary
#'   products from the secd denominator so it reads per tonne of secondary eaten
#'   as food. Should match the \code{secdToFeed} used for the numerator. See
#'   \code{\link{embodiedResourceKastner}}.
#'
#' @return MAgPIE object (region, year, pathway) with pathway in
#'   \{prim, secd, feed\}, in tonnes (Mt DM) of the final product consumed.
#' @author David M Chen
#' @seealso \code{\link{footprints}}, \code{\link{reportFootprints}}
#' @importFrom magclass getItems dimSums add_dimension mbind
#' @examples
#' \dontrun{
#'   dem   <- demand(gdx, level = "reg")[, , "dom_balanceflow", invert = TRUE]
#'   denom <- footprintDemand(dem, getItems(dem, dim = 3.2),
#'                            kli = readGDX(gdx, "kli"), ksd = readGDX(gdx, "ksd"))
#' }

footprintDemand <- function(dem, prods, kli, ksd, secdToFeed = TRUE) {
  dProd  <- getItems(dem, dim = 3.2)
  primP  <- setdiff(intersect(dProd, prods), c(kli, ksd))   # primary crops/pasture only
  ksdDem <- intersect(dProd, ksd)
  kliDem <- intersect(dProd, kli)

  dP   <- dem[, , primP]
  prim <- dimSums(dP[, , c("processed", "feed"), invert = TRUE], dim = 3)
  secd <- if (length(ksdDem) > 0) {
            ds <- dem[, , ksdDem]
            dimSums(if (secdToFeed) ds[, , "feed", invert = TRUE] else ds, dim = 3)
          } else dimSums(dP[, , "processed"], dim = 3)
  liv  <- if (length(kliDem) > 0) dimSums(dem[, , kliDem], dim = 3) else dimSums(dP[, , "feed"], dim = 3)

  mbind(add_dimension(prim, 3.1, "pathway", "prim"),
        add_dimension(secd, 3.1, "pathway", "secd"),
        add_dimension(liv,  3.1, "pathway", "feed"))
}
