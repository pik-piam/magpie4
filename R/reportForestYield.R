#' @title reportForestYield
#' @description reports MAgPIE harvested area for timber.
#'
#' @export
#'
#' @param gdx GDX file
#' @return Yield from Forests for timber production
#' @author Abhijeet Mishra, Florian Humpenoeder
#' @examples
#'
#'   \dontrun{
#'     x <- reportForestYield(gdx)
#'   }
#'
#'
#' @section Forest yield variables:
#' Name | Unit | Meta
#' ---|---|---
#' Timber Yields\|Harvest\|Forestry | m3 per ha | Timber yield from plantation forestry
#' Timber Yields\|Harvest\|Primary forest | m3 per ha | Timber yield from primary forest harvesting
#' Timber Yields\|Harvest\|Secondary forest | m3 per ha | Timber yield from secondary forest harvesting
#' @md


reportForestYield <- function(gdx, level = "regglo") {
  a <- NULL

  if (suppressWarnings(!is.null(readGDX(gdx, "fcostsALL")))) {
    a_harvest <- ForestYield(gdx, level = level)
    if (!is.null(a_harvest)) {
      getNames(a_harvest) <- paste0("Timber Yields|Harvest|", getNames(a_harvest))
      getNames(a_harvest) <- paste0(getNames(a_harvest), " (m3 per ha)")
      a <- a_harvest
    }
  } else {
    message("Disabled (no timber) ", appendLF = FALSE)
  }

  return(a)
}
