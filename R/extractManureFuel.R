#' @title extractManureFuel
#' @description Extracts manure used as direct combustion fuel from a MAgPIE GDX file
#' at ISO country level. Converts from nitrogen mass (Mt N) to energy (PJ) using
#' livestock-specific heating values and nitrogen contents from Hoyos-Sebá et al. (2024).
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#'
#' @return MAgPIE object with manure fuel at ISO level in PJ.
#'
#' @author Kristine Karstens
#'
#' @importFrom gdx2 readGDX
#' @importFrom madrat toolCountryFill
#'
#' @examples
#' \dontrun{
#'   x <- extractManureFuel(gdx)
#' }

extractManureFuel <- function(gdx, file = NULL) {

  i2iso <- gdx2::readGDX(gdx, "i_to_iso")
  kli   <- gdx2::readGDX(gdx, "kli")

  # Livestock-specific conversion factors (Hoyos-Sebá et al., 2024)
  # HHV (MJ/kg DM, dry basis) and N content (% of DM)
  conversionFactors <- new.magpie(cells_and_regions = "GLO", years = NULL, names = kli,
                                  sets = c("region", "year", "kli"))

  # Higher Heating Value (MJ/kg DM)
  conversionFactors[, , "livst_rum"]  <- 17.51  # Cattle (beef + dairy)
  conversionFactors[, , "livst_pig"]  <- 13.66  # Pig
  conversionFactors[, , "livst_chick"] <- 14.87  # Poultry (broiler + layer)
  conversionFactors[, , "livst_egg"]  <- 14.87  # Poultry (using same as chicken)
  conversionFactors[, , "livst_milk"] <- 17.51  # Dairy cattle (using cattle value)

  # Nitrogen content (% of DM)
  nContent <- new.magpie(cells_and_regions = "GLO", years = NULL, names = kli,
                         sets = c("region", "year", "kli"))
  nContent[, , "livst_rum"]  <- 0.0205  # 2.05% for cattle
  nContent[, , "livst_pig"]  <- 0.0510  # 5.10% for pig
  nContent[, , "livst_chick"] <- 0.0350  # 3.50% for poultry
  nContent[, , "livst_egg"]  <- 0.0350  # 3.50% for poultry
  nContent[, , "livst_milk"] <- 0.0205  # 2.05% for dairy cattle

  manureFuelIsoN <- gdxAggregate(gdx, ManureExcretion(gdx, level = "grid"), to = "iso")[, , "fuel", pmatch = TRUE]

  # Convert Mt N to PJ by livestock type: (Mt N / N_content) * HHV
  # Mt N / (fraction) = Mt DM; Mt DM * MJ/kg = PJ (1 Mt = 10^9 kg, 1 PJ = 10^9 MJ)
  manureDM <- manureFuelIsoN / nContent # Mt DM
  manureFuelPJ <- manureDM * conversionFactors # PJ

  # Sum across livestock types
  manureFuelPJ <- dimSums(manureFuelPJ, dim = 3)
  manureFuelPJ <- toolCountryFill(manureFuelPJ, fill = 0)
  getNames(manureFuelPJ) <- "Biomass supply|Manure Collected As Fuel (PJ/yr)"
  getSets(manureFuelPJ) <- c("iso", "year", "data")
  getComment(manureFuelPJ) <- c("description: Manure used as direct fuel at ISO level",
                                "unit: PJ/yr",
                                "source: MAgPIE ManureExcretion [fuel]")

  out(manureFuelPJ, file)
}
