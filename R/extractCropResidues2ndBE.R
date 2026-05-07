#' @title extractCropResidues2ndBE
#' @description Extracts crop residues available for 2nd generation bioenergy from a
#' MAgPIE GDX file at ISO country level. Applies soil cover constraints (minimum 30\%
#' soil cover retained, Lutz et al. 2019) and a collection fraction to estimate
#' sustainably harvestable residue biomass, converted to energy (PJ).
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param collectionFraction fraction of available residues that can be physically collected (default: 0.25)
#' @param minDensityForExtraction minimum residue density (tDM/ha) below which extraction is not economic (default: 0.1)
#'
#' @return MAgPIE object with crop residue potential for 2nd gen bioenergy at ISO level in PJ.
#'   Dimension 1: ISO country, Dimension 2: year,
#'   Dimension 3: residue types (res_cereals, res_fibrous, res_nonfibrous)
#'
#' @author Kristine Karstens
#'
#' @seealso \code{\link{ResidueBiomass}}, \code{\link{ResidueUsage}}
#'
#' @importFrom gdx2 readGDX
#' @importFrom magclass new.magpie collapseNames collapseDim dimSums getSets getNames
#' @importFrom madrat toolAggregate toolConditionalReplace toolCountryFill
#' @importFrom mstools toolIso2CellCountries
#' @importFrom magpiesets reportingnames
#'
#' @references
#' Lutz et al. (2019), GMD, \url{https://gmd.copernicus.org/articles/12/2419/2019/}
#'
#' @examples
#' \dontrun{
#'   x <- extractCropResidues2ndBE(gdx)
#' }

extractCropResidues2ndBE <- function(gdx, file = NULL, collectionFraction = 0.25, minDensityForExtraction = 0.1) {

  # Read sets and mappings
  kres     <- gdx2::readGDX(gdx, "kres")
  kcr      <- gdx2::readGDX(gdx, "kcr")
  kcr2kres <- gdx2::readGDX(gdx, "kres_kcr")
  i2iso    <- gdx2::readGDX(gdx, "i_to_iso")
  dmToGE   <- gdx2::readGDX(gdx, "fm_attributes")[, , kres][, , "ge"] # GJ/t == PJ/Mt

  # Get residue biomass and usage
  usage   <- magpie4::ResidueUsage(gdx, level = "reg", products = "kres", attributes = c("dm"))
  biomass <- magpie4::ResidueBiomass(gdx, level = "grid", product_aggr = "kres", attributes = c("dm"))

  # Calculate recycling shares and downscale to ISO
  recycleShare <- collapseNames(usage[, , "recycling"], collapsedim = 1) / dimSums(usage, dim = "usage")
  recycleShare <- toolAggregate(recycleShare, rel = i2iso, from = "i", to = "iso")
  recycled     <- collapseNames(biomass * mstools::toolIso2CellCountries(recycleShare, cells = "lpjcell"))

  # Get crop area
  area    <- magpie4::croparea(gdx, level = "grid", products = "kcr", product_aggr = FALSE)
  # We ignore sunflower, oilpalm, foddr, begr, betr, thus partrel = TRUE
  # this is also done in the corresponding magpie4 functions (e.g. ResidueUsage)
  area    <- toolAggregate(area, kcr2kres, from = "kcr", to = "kres", partrel = TRUE, dim = 3)[, , kres]

  # Apply soil cover constraints (30% minimum, Lutz et al. 2019 eq. 5)
  # i.e. how much residue must remain to keep the soil healthy
  # Also: We only consider ares with at least minDensityForExtraction
  potResDensity <- toolConditionalReplace(recycled / area,
                                          c("is.na()", "is.infinite()"),
                                          0)
  soilCoverParameter <- new.magpie(names = kres, sets = c("region", "year", "A_m"))
  soilCoverParameter[, , "res_cereals"]    <- 0.45
  soilCoverParameter[, , "res_fibrous"]    <- 0.15
  soilCoverParameter[, , "res_nonfibrous"] <- 0.55
  coverToMass     <- function(F_c, A_m) { -log(1 - F_c) / A_m }
  minResDensity   <- coverToMass(0.3, soilCoverParameter)
  availResDensity <- potResDensity - minResDensity
  availResDensity[availResDensity < minDensityForExtraction] <- 0 # Ignore too sparse areas

  # Back to absolute numbers
  residuesAvail   <- collapseNames(availResDensity * area)

  # Apply collection fraction
  residuesCollected <- collectionFraction * residuesAvail # Only collect a fraction of the available mass
  residuesCollected <- toolConditionalReplace(collapseNames(residuesCollected),
                                              c("is.na()", "is.infinite()"),
                                              0)

  # Aggregate to iso
  residuesCollected <- toolCountryFill(dimSums(residuesCollected, dim = c(1.1, 1.2)), fill = 0)

  # Convert to GE
  residuesFor2ndBE <- residuesCollected * dmToGE

  residuesFor2ndBE <- collapseDim(residuesFor2ndBE)
  getNames(residuesFor2ndBE) <- paste0("Biomass potential|Crop residues|+|",
                                       magpiesets::reportingnames(getNames(residuesFor2ndBE)),
                                       " (PJ/yr)")
  getSets(residuesFor2ndBE) <- c("iso", "year", "data")
  getComment(residuesFor2ndBE) <- c("description: Crop residues potential for 2nd gen bioenergy at ISO level",
                                    "unit: PJ/yr",
                                    "source: MAgPIE ResidueBiomass, recycling share, soil cover constraint")

  out(residuesFor2ndBE, file)
}


tmpTraditionalFuel <- function(gdx, collectionFraction = 0.25, minDensityForExtraction = 0.1) {
  # -------------------------------
  # Temporary code for analysis
  # -------------------------------

  # Read sets and mappings
  kres     <- gdx2::readGDX(gdx, "kres")
  kcr      <- gdx2::readGDX(gdx, "kcr")
  kcr2kres <- gdx2::readGDX(gdx, "kres_kcr")
  i2iso    <- gdx2::readGDX(gdx, "i_to_iso")
  dmToGE   <- gdx2::readGDX(gdx, "fm_attributes")[, , kres][, , "ge"]  # GJ/t == PJ/Mt

  # Get residue biomass and usage
  usage   <- magpie4::ResidueUsage(gdx, level = "reg", products = "kres", attributes = c("dm")) # Mt
  biomass <- magpie4::ResidueBiomass(gdx, level = "grid", product_aggr = "kres", attributes = c("dm"))

  # Calculate recycling shares and downscale to ISO
  beShare <- collapseNames(usage[, , "bioenergy"], collapsedim = 1) / dimSums(usage, dim = "usage")
  beShare <- toolAggregate(beShare, rel = i2iso, from = "i", to = "iso")
  be     <- collapseNames(biomass * mstools::toolIso2CellCountries(beShare, cells = "lpjcell"))

  # Apply collection fraction
  be <- toolConditionalReplace(collapseNames(be),
                               c("is.na()", "is.infinite()"),
                               0)

  # Aggregate to iso
  be <- toolCountryFill(dimSums(be, dim = c(1.1, 1.2)), fill = 0)

  # Convert to GE
  be <- be * dmToGE

  getSets(be) <- c("iso", "year", "data")
  be <- collapseDim(be)

  return(be)

  # -------------------------------
  # End Temporary code for analysis
  # -------------------------------
}
