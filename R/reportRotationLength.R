#' @title reportRotationLength
#' @description reports Forest rotation length.
#'
#' @export
#'
#' @param gdx GDX file
#' @return Forest rotation length
#' @author Abhijeet Mishra
#' @examples
#'
#'   \dontrun{
#'     x <- reportRotationLength(gdx)
#'   }
#'
#' @section Rotation length variables:
#' Name | Unit | Meta
#' ---|---|---
#' Rotation lengths\|Forestry | years | Optimal rotation length for timber plantations
#' @md


reportRotationLength <- function(gdx, level = "regglo") {
  a <- NULL

  if (suppressWarnings(!is.null(readGDX(gdx, "fcostsALL")))) {
    a <- RotationLength(gdx, level = "regglo")
    getNames(a) <- paste0("Rotation lengths|", getNames(a))
    getNames(a) <- paste0(getNames(a), " (years)")
  } else {
    message("NULL returned.")
  }

  return(a)
}