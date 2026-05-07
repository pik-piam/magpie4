#' @title extractBiogasFeedstock
#' @description Extracts biogas feedstock from a MAgPIE GDX file at ISO country level.
#' Currently covers two sources: manure allocated to digesters and fodder crops
#' (placeholder, all zeros until MAgPIE tracks foddr/forage crop allocation to biogas).
#' Manure is converted from nitrogen mass (Mt N) to energy (PJ) using livestock-specific
#' heating values and nitrogen contents from Hoyos-Sebá et al. (2024).
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#'
#' @return MAgPIE object with biogas feedstock potential at ISO level in PJ.
#'   Dimension 1: ISO country, Dimension 2: year,
#'   Dimension 3: source type ("manure", "fodder")
#'
#' @author Kristine Karstens
#'
#' @importFrom gdx2 readGDX
#' @importFrom magclass new.magpie dimSums mbind setNames getSets
#' @importFrom madrat toolCountryFill
#'
#' @examples
#' \dontrun{
#'   x <- extractBiogasFeedstock(gdx)
#' }

extractBiogasFeedstock <- function(gdx, file = NULL) {

  # ------------------------------------------------------------------
  # Source 1: Manure for biogas — manure tracked as going to digester
  # ------------------------------------------------------------------

  kli <- gdx2::readGDX(gdx, "kli")

  # Livestock-specific conversion factors (Hoyos-Sebá et al., 2024)
  # HHV (MJ/kg DM, dry basis) and N content (% of DM)
  conversionFactors <- new.magpie(cells_and_regions = "GLO", years = NULL, names = kli,
                                  sets = c("region", "year", "kli"))

  # Higher Heating Value (MJ/kg DM)
  conversionFactors[, , "livst_rum"]   <- 17.51  # Cattle (beef + dairy)
  conversionFactors[, , "livst_pig"]   <- 13.66  # Pig
  conversionFactors[, , "livst_chick"] <- 14.87  # Poultry (broiler + layer)
  conversionFactors[, , "livst_egg"]   <- 14.87  # Poultry (using same as chicken)
  conversionFactors[, , "livst_milk"]  <- 17.51  # Dairy cattle (using cattle value)

  # Nitrogen content (% of DM)
  nContent <- new.magpie(cells_and_regions = "GLO", years = NULL, names = kli,
                         sets = c("region", "year", "kli"))
  nContent[, , "livst_rum"]   <- 0.0205  # 2.05% for cattle
  nContent[, , "livst_pig"]   <- 0.0510  # 5.10% for pig
  nContent[, , "livst_chick"] <- 0.0350  # 3.50% for poultry
  nContent[, , "livst_egg"]   <- 0.0350  # 3.50% for poultry
  nContent[, , "livst_milk"]  <- 0.0205  # 2.05% for dairy cattle

  manureBiogasIsoN <- gdxAggregate(gdx, ManureExcretion(gdx, level = "grid"), to = "iso")[, , "digester", pmatch = TRUE]

  # Convert Mt N to PJ by livestock type: (Mt N / N_content) * HHV
  # Mt N / (fraction) = Mt DM; Mt DM * MJ/kg = PJ (1 Mt = 10^9 kg, 1 PJ = 10^9 MJ)
  manureDM      <- manureBiogasIsoN / nContent  # Mt DM
  manureBiogas  <- dimSums(manureDM * conversionFactors, dim = 3)  # PJ
  manureBiogas  <- toolCountryFill(manureBiogas, fill = 0)

  # ------------------------------------------------------------------
  # Source 2: Fodder crops for biogas
  # TODO: implement once MAgPIE tracks foddr/forage crops allocated to biogas
  # ------------------------------------------------------------------
  fodderBiogasIso <- manureBiogas * 0

  # ------------------------------------------------------------------
  # Combine sources
  # ------------------------------------------------------------------
  biogasFeedstock <- mbind(
    setNames(manureBiogas,    "Biomass potential|Biogas feedstock|+|Manure - Anaerobic Digester (PJ/yr)"),
    setNames(fodderBiogasIso, "Biomass potential|Biogas feedstock|+|Forage (PJ/yr)")
  )

  getSets(biogasFeedstock) <- c("iso", "year", "data")
  getComment(biogasFeedstock) <- c("description: Biogas feedstock potential at ISO level",
                                   "unit: PJ/yr",
                                   "source: MAgPIE ManureExcretion [digester]; fodder placeholder (zeros)")

  out(biogasFeedstock, file)
}
