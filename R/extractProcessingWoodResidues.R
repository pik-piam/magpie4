#' @title extractProcessingWoodResidues
#' @description Extracts the potential of wood processing residues (sawmill byproducts)
#' from a MAgPIE GDX file at ISO country level and converts to energy (PJ).
#' Processing residues are computed from industrial roundwood demand using a
#' 30% residue rate (see \code{reportProcessingResiduesForestry} in magpie4).
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#'
#' @return MAgPIE object with potential wood processing residues at ISO level in PJ.
#'
#' @author Kristine Karstens
#'
#' @importFrom gdx2 readGDX
#' @importFrom madrat toolAggregate
#'
#' @examples
#' \dontrun{
#'   x <- extractProcessingWoodResidues(gdx)
#' }

extractProcessingWoodResidues <- function(gdx, file = NULL) {

  # see: https://github.com/pik-piam/magpie4/blob/master/R/reportProcessingResiduesForestry.R
  processingResidueRate <- 0.3

  # Read data from GDX
  demandWoodIso <- gdx2::readGDX(gdx, "p73_forestry_demand_prod_specific")[, , "industrial_roundwood"]
  demandWoodReg <- gdx2::readGDX(gdx, "pm_demand_forestry")[, , "wood"]
  attrWood      <- gdx2::readGDX(gdx, "fm_attributes")[, , "wood"][, , "ge"]  # PJ/Mt DM
  i2iso         <- gdx2::readGDX(gdx, "i_to_iso")
  imVolConv     <- gdx2::readGDX(gdx, "im_vol_conv") # <- tDM / m^3 = MtDM / Mm^3

  # Subset to modelled timesteps only
  tModelled     <- gdx2::readGDX(gdx, "t")
  demandWoodIso <- demandWoodIso[, intersect(tModelled, getYears(demandWoodIso)), ]

  # Map regional wood density to ISO level
  # Each ISO country inherits its parent region's density value (not a sum)
  volConWoodIso <- toolAggregate(imVolConv, rel = i2iso, from = "i", to = "iso",
                                 weight = NULL)

  # Keep constant after 2100
  years <- getYears(demandWoodIso, as.integer = TRUE)
  yearsAfter2100 <- years[years > 2100]
  if (length(yearsAfter2100) > 0) {
    demandWoodIso[, yearsAfter2100, ] <- setYears(demandWoodIso[, "y2100", ], NULL)
  }

  # Quality check: ISO vs regional consistency
  cyears <- intersect(getYears(demandWoodIso), getYears(demandWoodReg))
  diff   <- dimSums(round(demandWoodIso[, cyears, ] * volConWoodIso, 3), dim = 1) -
    dimSums(demandWoodReg[, cyears, ], dim = 1)
  if (any(abs(diff) > 0.1)) {
    warning("Discrepancy between ISO and regional wood demand")
  }

  # Potential processing residues: Mm3 * MtDM/Mm3 * PJ/MtDM * rate
  potentialProcWoodRes <- demandWoodIso * volConWoodIso * attrWood * processingResidueRate

  getNames(potentialProcWoodRes) <- "Biomass potential|Wood processing residues (PJ/yr)"
  getSets(potentialProcWoodRes)  <- c("iso", "year", "data")
  getComment(potentialProcWoodRes) <- c("description: Potential wood processing residues at ISO level",
                                        "unit: PJ/yr",
                                        "source: MAgPIE p73_forestry_demand_prod_specific * processingResidueRate")

  out(potentialProcWoodRes, file)
}
