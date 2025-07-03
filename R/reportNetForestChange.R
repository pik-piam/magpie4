#' @title reportNetForestChange
#' @description reports net and gross forest area change
#'
#' @export
#'
#' @param gdx GDX file
#' @return NetForestChange as magclass object (Mha per year)
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' x <- reportNetForestChange(gdx)
#' }
#'
reportNetForestChange <- function(gdx) {

  x <- NULL
  s32_aff_plantation <- readGDX(gdx,"s32_aff_plantation")
  a <- NetForestChange(gdx, level = "regglo")

  if (!is.null(a)) {
    ### Net change
    x <- mbind(x, setNames(dimSums(a[,,c("plantNet","ndcNet","affNet","secdfNet","primfNet")], dim = 3), "Resources|NetForestChange (Mha/yr)"))

    ### Reduction
    x <- mbind(x, setNames(dimSums(a[,,c("plantRed","ndcRed","affRed","secdfRed","primfRed")], dim = 3), "Resources|GrossForestLoss (Mha/yr)"))
    x <- mbind(x, setNames(dimSums(a[,,c("primfRed")], dim = 3), "Resources|GrossForestLoss|+|Primary (Mha/yr)"))
    x <- mbind(x, setNames(dimSums(a[,,c("secdfRed")], dim = 3), "Resources|GrossForestLoss|+|Secondary (Mha/yr)"))
    x <- mbind(x, setNames(dimSums(a[,,c("plantRed","affRed","ndcRed")], dim = 3), "Resources|GrossForestLoss|+|Planted (Mha/yr)"))
    x <- mbind(x, setNames(dimSums(a[,,c("plantRed")], dim = 3), "Resources|GrossForestLoss|Planted|Plantation|+|Timber (Mha/yr)"))
    if(s32_aff_plantation == 0) {
      x <- mbind(x, setNames(dimSums(a[,,c("affRed")], dim = 3), "Resources|GrossForestLoss|Planted|Natural|+|CO2-price AR (Mha/yr)"))
      x <- mbind(x, setNames(new.magpie(getRegions(x), getYears(x), NULL, fill = 0, sets = getSets(x)), "Resources|GrossForestLoss|Planted|Plantation|+|CO2-price AR (Mha/yr)"))
    } else if (s32_aff_plantation == 1) {
      x <- mbind(x, setNames(new.magpie(getRegions(x), getYears(x), NULL, fill = 0, sets = getSets(x)), "Resources|GrossForestLoss|Planted|Natural|+|CO2-price AR (Mha/yr)"))
      x <- mbind(x, setNames(dimSums(a[,,c("affRed")], dim = 3), "Resources|GrossForestLoss|Planted|Plantation|+|CO2-price AR (Mha/yr)"))
    }
    x <- mbind(x, setNames(dimSums(a[,,c("ndcRed")], dim = 3), "Resources|GrossForestLoss|Planted|Natural|+|NPI_NDC AR (Mha/yr)"))

    ### Expansion
    x <- mbind(x, setNames(dimSums(a[,,c("plantExp","ndcExp","affExp","secdfExp","primfExp")], dim = 3), "Resources|GrossForestGain (Mha/yr)"))
    x <- mbind(x, setNames(dimSums(a[,,c("primfExp")], dim = 3), "Resources|GrossForestGain|+|Primary (Mha/yr)"))
    x <- mbind(x, setNames(dimSums(a[,,c("secdfExp")], dim = 3), "Resources|GrossForestGain|+|Secondary (Mha/yr)"))
    x <- mbind(x, setNames(dimSums(a[,,c("plantExp", "affExp", "ndcExp")], dim = 3), "Resources|GrossForestGain|+|Planted (Mha/yr)"))
    x <- mbind(x, setNames(dimSums(a[,,c("plantExp")], dim = 3), "Resources|GrossForestGain|Planted|Plantation|+|Timber (Mha/yr)"))
    if(s32_aff_plantation == 0) {
      x <- mbind(x, setNames(dimSums(a[,,c("affExp")], dim = 3), "Resources|GrossForestGain|Planted|Natural|+|CO2-price AR (Mha/yr)"))
      x <- mbind(x, setNames(new.magpie(getRegions(x), getYears(x), NULL, fill = 0, sets = getSets(x)), "Resources|GrossForestGain|Planted|Plantation|+|CO2-price AR (Mha/yr)"))
    } else if (s32_aff_plantation == 1) {
      x <- mbind(x, setNames(new.magpie(getRegions(x), getYears(x), NULL, fill = 0, sets = getSets(x)), "Resources|GrossForestGain|Planted|Natural|+|CO2-price AR (Mha/yr)"))
      x <- mbind(x, setNames(dimSums(a[,,c("affExp")], dim = 3), "Resources|GrossForestGain|Planted|Plantation|+|CO2-price AR (Mha/yr)"))
    }
    x <- mbind(x, setNames(dimSums(a[,,c("ndcExp")], dim = 3), "Resources|GrossForestGain|Planted|Natural|+|NPI_NDC AR (Mha/yr)"))
  }

  return(x)
}
