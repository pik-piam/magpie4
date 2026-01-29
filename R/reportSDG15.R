#' @title reportSDG15
#' @description reports all SDG indicators relevant for SD15 - Life on Land
#' @import magpiesets
#'
#' @export
#'
#' @param gdx GDX file
#' @return MAgPIE object
#' @author Benjamin Bodirsky, Isabelle Weindl
#' @examples
#'
#'   \dontrun{
#'     x <- reportSDG15(gdx)
#'   }
#'
#'
#' @section SDG15 Life on land variables:
#' Name | Unit | Meta
#' ---|---|---
#' SDG\|SDG15\|Forest share | share of total land | Share of land covered by forest (primary, secondary, forestry)
#' SDG\|SDG15\|Primary forest share | share of total land | Share of land covered by primary forest
#' SDG\|SDG15\|Afforestation | million ha | Area of afforestation (NDC and additional)
#' SDG\|SDG15\|Other natural land share | share of total land | Share of land covered by other natural land
#' SDG\|SDG15\|Terrestrial biodiversity | index | Biodiversity Intactness Index (BII)
#' SDG\|SDG15\|Non-agricultural land share | share of total land | Share of land not used for agriculture
#' SDG\|SDG15\|Biological nitrogen fixation on cropland | Mt N/yr | Total biological nitrogen fixation on cropland
#' SDG\|SDG15\|Industrial and intentional biological fixation of N | Mt N/yr | Sum of fertilizer and crop nitrogen fixation
#' @md


reportSDG15 <- function(gdx) {
  x <- NULL

  indicatorname <- "SDG|SDG15|Forest share"
  unit <- "share of total land"
  out <- land(gdx, level = "regglo")
  out <- dimSums(out[, , c("forestry", "primforest", "secdforest")]) / dimSums(out)
  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x <- mbind(x, out)

  indicatorname <- "SDG|SDG15|Primary forest share"
  unit <- "share of total land"
  out <- land(gdx, level = "regglo")
  out <- dimSums(out[, , c("primforest")]) / dimSums(out)
  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x <- mbind(x, out)

  indicatorname <- "SDG|SDG15|Biodiversity protection proportion"
  unit <- "share of total land"
  # #p35_save_primforest / vm_land.l(j,"primforest")
  # out <- land(gdx,level="regglo",types = NULL,subcategories = c("primforest","forestry","secdforest","other"),sum = FALSE)
  # out<- dimSums(out[,,c("prot")])/dimSums(out)
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)

  indicatorname <- "SDG|SDG15|Afforestation"
  unit <- "million ha"
  out <- dimSums(landForestry(gdx, level = "regglo")[, , c("ndc", "aff")], dim = 3)
  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x <- mbind(x, out)

  indicatorname <- "SDG|SDG15|Other natural land share"
  unit <- "share of total land"
  out <- land(gdx, level = "regglo")
  out <- dimSums(out[, , c("other")]) / dimSums(out)
  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x <- mbind(x, out)

  indicatorname <- "SDG|SDG15|Terrestrial biodiversity"
  unit <- "index"
  out <- BII(gdx, level = "regglo")
  if (!is.null(out)) {
    getNames(out) <- paste0(indicatorname, " (", unit, ")")
  } else {
    cat("No biodiversity reporting possible")
  }
  x <- mbind(x, out)

  indicatorname <- "SDG|SDG15|Freshwater biodiversity"
  unit <- "index"
  #out <- land(gdx,level="regglo")
  #getNames(out) <- paste0(indicatorname, " (",unit,")")
  #x <- mbind(x,out)

  indicatorname <- "SDG|SDG15|Non-agricultural land share"
  unit <- "share of total land"
  out <- land(gdx, level = "regglo")
  out <- dimSums(out[, , c("forestry", "primforest", "secdforest", "urban", "other")]) / dimSums(out)
  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x <- mbind(x, out)

  budget <- NitrogenBudget(gdx, level = "regglo")

  indicatorname <- "SDG|SDG15|Biological nitrogen fixation on cropland"
  unit <- "Mt N/yr"
  bio_fix <- c("fixation_crops", "fixation_freeliving")
  out <- dimSums(budget[, , bio_fix], dim = 3)
  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x <- mbind(x, out)

  indicatorname <- "SDG|SDG15|Industrial and intentional biological fixation of N"
  unit <- "Mt N/yr"
  new_inputs <- c("fertilizer", "fixation_crops")
  out <- dimSums(budget[, , new_inputs], dim = 3)
  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x <- mbind(x, out)

  #x <- x[,,sort(getNames(x))]
  return(x)
}
