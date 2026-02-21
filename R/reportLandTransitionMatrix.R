#' @title reportLandTransitionMatrix
#' @description reports land transition matrix with gross land-use change flows
#' between all land types
#'
#' @export
#'
#' @param gdx GDX file
#' @param level The aggregation level to be used ("regglo" by default)
#' @return land transition matrix as MAgPIE object (Mha/yr)
#' @author Florian Humpenoeder
#' @examples
#'
#'   \dontrun{
#'     x <- reportLandTransitionMatrix(gdx)
#'   }
#'
#' @md

reportLandTransitionMatrix <- function(gdx, level = "regglo") {

  a <- landTransitionMatrix(gdx, level = level)

  if (is.null(a)) return(NULL)

  landTypes <- c("crop", "past", "forestry", "primforest", "secdforest", "urban", "other")
  forestTypes <- c("forestry", "primforest", "secdforest")
  nonForestTypes <- c("crop", "past", "urban", "other")

  unit <- " (Mha/yr)"

  outputParts <- list()

  # Grand total: sum of all off-diagonal transitions
  offDiagPairs <- unlist(lapply(landTypes, function(src) {
    paste0(src, ".", setdiff(landTypes, src))
  }))
  grandTotal <- dimSums(a[, , offDiagPairs], dim = 3)
  outputParts <- append(outputParts, list(
    list(paste0("Resources|Land Transitions", unit), grandTotal)
  ))

  # For each source land type
  for (src in landTypes) {
    srcName <- reportingnames(src)
    fromPrefix <- paste0("Resources|Land Transitions|From ", srcName)

    # All off-diagonal destinations
    dsts <- setdiff(landTypes, src)

    # Total leaving this source
    srcTotal <- dimSums(a[, , paste0(src, ".", dsts)], dim = 3)
    outputParts <- append(outputParts, list(
      list(paste0("Resources|Land Transitions|+|From ", srcName, unit), srcTotal)
    ))

    # Non-forest destinations (excluding self)
    nonForestDsts <- setdiff(nonForestTypes, src)
    for (dst in nonForestDsts) {
      dstName <- reportingnames(dst)
      outputParts <- append(outputParts, list(
        list(paste0(fromPrefix, "|+|To ", dstName, unit),
             collapseNames(a[, , paste0(src, ".", dst)]))
      ))
    }

    # Forest aggregate (non-self forest destinations)
    forestDsts <- setdiff(forestTypes, src)
    forestAgg <- dimSums(a[, , paste0(src, ".", forestDsts)], dim = 3)
    outputParts <- append(outputParts, list(
      list(paste0(fromPrefix, "|+|To ", reportingnames("forest"), unit), forestAgg)
    ))

    # Forest sub-destinations (non-self forest types)
    for (dst in forestDsts) {
      dstName <- reportingnames(dst)
      outputParts <- append(outputParts, list(
        list(paste0(fromPrefix, "|To ", reportingnames("forest"), "|+|To ", dstName, unit),
             collapseNames(a[, , paste0(src, ".", dst)]))
      ))
    }
  }

  outputParts <- lapply(outputParts, function(part) {
    return(setNames(part[[2]], part[[1]]))
  })

  result <- do.call(mbind, outputParts)

  return(result)
}
