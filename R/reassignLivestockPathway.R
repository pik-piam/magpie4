#' @title reassignLivestockPathway
#' @description Collapse every livestock (\code{kli}) product's prim/secd/feed
#'   pathway split into a single \code{feed} (Livestock) pathway, leaving all
#'   non-livestock products untouched. The Kastner prim/secd/feed pathway split
#'   in \code{\link{embodiedResourceKastner}} is keyed on each product's demand
#'   share, so a livestock product's OWN footprint (enteric/manure emissions,
#'   herding labour, drinking water) lands in the prim/secd pathway of the
#'   \code{kli} product. To show the FULL livestock footprint in the Livestock
#'   pathway, this moves each \code{kli} product's whole footprint (all pathways)
#'   into \code{feed}. Non-\code{kli} feed crops/pasture keep their pathway, so
#'   they still count as Livestock via the \code{feed} pathway.
#'
#'   Works on both layouts produced by \code{\link{embodiedResourceKastner}}:
#'   the bilateral object (dim 3 = \code{pathway.product}) and the regional
#'   accounting object (dim 3 = \code{accounting.pathway.product}); the pathway
#'   and product sub-dimensions are located by name/position, so the object must
#'   carry a sub-dimension named \code{pathway} with the product as the last
#'   sub-dimension of dim 3. The operation is a no-op when no \code{kli} product
#'   is present (e.g. the land resource, whose livestock land sits in the feed
#'   pathway of crops) and is idempotent (re-applying it changes nothing). Each
#'   product's total (summed over pathway) is conserved.
#'
#' @export
#'
#' @param x MAgPIE object with a \code{pathway} sub-dimension in dim 3 and the
#'   product as the last sub-dimension of dim 3 (bilateral or regional output of
#'   \code{\link{embodiedResourceKastner}} and its \code{embodied*Kastner} wrappers).
#' @param kli character vector of livestock product names (the \code{kli} set).
#'   Alternatively supply \code{gdx} to read it.
#' @param gdx optional GDX file to read the \code{kli} set from when \code{kli}
#'   is not given.
#'
#' @return MAgPIE object of the same layout as \code{x}, with all \code{kli}
#'   products' footprint moved to the \code{feed} pathway.
#' @author David M Chen
#' @seealso \code{\link{embodiedResourceKastner}}, \code{\link{footprints}}
#' @importFrom magclass getItems dimSums mbind add_dimension
#' @importFrom gdx2 readGDX

reassignLivestockPathway <- function(x, kli = NULL, gdx = NULL) {

  if (is.null(kli)) {
    if (is.null(gdx)) stop("reassignLivestockPathway: supply either 'kli' or 'gdx'.")
    kli <- readGDX(gdx, "kli")
  }

  # Locate the pathway and product sub-dimensions of dim 3 by their set names.
  d3 <- strsplit(names(dimnames(x))[[3]], "\\.")[[1]]
  if (!("pathway" %in% d3)) stop("reassignLivestockPathway: object has no 'pathway' sub-dimension in dim 3.")
  pathPos <- match("pathway", d3)          # sub-dim index of pathway within dim 3
  prodPos <- length(d3)                    # product is the last sub-dim of dim 3

  prods <- getItems(x, dim = as.numeric(paste0("3.", prodPos)))
  liP   <- intersect(prods, kli)
  if (length(liP) == 0) return(x)          # no livestock products -> nothing to move
  nonLi <- setdiff(prods, liP)

  # kli products: sum over pathway, then re-add pathway = "feed" at its original
  # position so the layout matches the untouched non-kli slice for mbind.
  xLi <- add_dimension(dimSums(x[, , liP], dim = "pathway"),
                       dim = as.numeric(paste0("3.", pathPos)), add = "pathway", nm = "feed")
  mbind(x[, , nonLi], xLi)
}
