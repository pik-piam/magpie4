#' @title extractWoodFuel
#' @description Extracts wood fuel demand from a MAgPIE GDX file at ISO country level
#' and converts from volumetric units (Mm³) to energy (PJ).
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#'
#' @return MAgPIE object with wood fuel demand at ISO level in PJ.
#'   Dimension 1: ISO country, Dimension 2: year, Dimension 3: "woodfuel"
#'
#' @author Kristine Karstens
#'
#' @importFrom gdx2 readGDX
#' @importFrom magclass getYears setYears dimSums getNames getSets
#' @importFrom madrat toolAggregate
#'
#' @examples
#' \dontrun{
#'   x <- extractWoodFuel(gdx)
#' }

extractWoodFuel <- function(gdx, file = NULL) {

  # Read data from GDX
  demandWoodFuelIso  <- gdx2::readGDX(gdx, "p73_forestry_demand_prod_specific")[, , "wood_fuel"]
  demandWoodFuelReg  <- gdx2::readGDX(gdx, "pm_demand_forestry")[, , "woodfuel"]
  attrWoodFuel       <- gdx2::readGDX(gdx, "fm_attributes")[, , "woodfuel"][, , "ge"] # PJ/Mt DM
  i2iso              <- gdx2::readGDX(gdx, "i_to_iso")
  imVolConv          <- gdx2::readGDX(gdx, "im_vol_conv") # tDM / m^3 = MtDM / Mm^3

  # Map regional wood density to ISO level
  # Each ISO country inherits its parent region's density value (not a sum)
  volConWoodIso <- toolAggregate(imVolConv, rel = i2iso, from = "i", to = "iso",
                                 weight = NULL)
  # Keep constant after 2100
  years <- getYears(demandWoodFuelIso, as.integer = TRUE)
  yearsAfter2100 <- years[years > 2100]
  if (length(yearsAfter2100) > 0) {
    demandWoodFuelIso[, yearsAfter2100, ] <- setYears(demandWoodFuelIso[, "y2100", ], NULL)
  }

  # Quality check: ISO vs regional consistency
  cyears <- intersect(getYears(demandWoodFuelIso), getYears(demandWoodFuelReg))
  diff   <- dimSums(round(demandWoodFuelIso[, cyears, ] * volConWoodIso, 3), dim = 1) -
              dimSums(demandWoodFuelReg[, cyears, ], dim = 1)
  if (any(abs(diff) > 0.1)) {
    warning("Discrepancy between ISO and regional wood fuel (check s73_timber_demand_switch)")
  }

  # Convert to PJ: Mm3 * MtDM/Mm3 * PJ/MtDM
  # Mm3 * tDM/m3 = Mt DM (10^6 tonnes)
  # Mt DM * GJ/tDM = PJ (10^15 Joules)
  demandWoodFuelPJ <- demandWoodFuelIso * volConWoodIso * attrWoodFuel
  getNames(demandWoodFuelPJ) <- "Biomass supply|Wood fuel (PJ/yr)"
  getSets(demandWoodFuelPJ) <- c("iso", "year", "data")
  getComment(demandWoodFuelPJ) <- c("description: Wood fuel supply at ISO level",
                                    "unit: PJ/yr",
                                    "source: MAgPIE p73_forestry_demand_prod_specific")

  out(demandWoodFuelPJ, file)
}
