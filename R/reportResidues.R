#' @title reportSOM
#' @description Report soil organic carbon stock size for future MAgPIE projections
#'
#' @export
#'
#' @param gdx GDX file
#' @param baseyear baseyear for calculating carbon stock change
#' @author Kristine Karstens
#' @examples
#'   \dontrun{
#'     x <- reportSOM(gdx)
#'   }
#'

reportResidues <- function(gdx){

  .reportNicely <- function(x, levelZeroName = NULL, unit = NULL, summation = TRUE, sep = "+") {
    x <- setNames(x, paste0(levelZeroName, "|", gsub("\\.", "|", getItems(x, dim = 3)), " ", unit))
    if (summation == TRUE) x <- magpiesets::summationhelper(x, sep = sep)
    return(x)
  }

  prefix     <- "Resources|Carbon|Cropland|Residues"
  agPre      <- paste0("|", magpiesets::reportingnames("ag"))
  removalPre <- paste0("|", magpiesets::reportingnames("removal"))
  unit       <- "(Mt C/yr)"

  ### Biomass reporting
  biomass <- Residues(gdx, level="regglo", products = "kres", output = "biomass")
  getItems(biomass, dim = 3.1) <- magpiesets::reportingnames(getItems(biomass, dim = 3.1))
  getItems(biomass, dim = 3.2) <- magpiesets::reportingnames(getItems(biomass, dim = 3.2))
  biomassPoolTotal <- dimSums(biomass, dim = 3.2)
  biomassTotal     <- dimSums(biomass, dim = 3)

  biomass          <- .reportNicely(biomass,          prefix, unit)
  biomassPoolTotal <- .reportNicely(biomassPoolTotal, prefix, unit)
  biomassTotal     <- setNames(biomassTotal, paste(prefix, unit))

  ### Field Balance
  fieldBalance <- Residues(gdx, level="regglo", products = "kres", output = "fieldBalance")
  getItems(fieldBalance, dim = 3.1) <- magpiesets::reportingnames(getItems(fieldBalance, dim = 3.1))
  getItems(fieldBalance, dim = 3.2) <- magpiesets::reportingnames(getItems(fieldBalance, dim = 3.2))
  fieldBalanceUseTotal <- dimSums(fieldBalance, dim = 3.2)

  fieldBalance         <- .reportNicely(fieldBalance,         paste0(prefix, agPre), unit)
  fieldBalanceUseTotal <- .reportNicely(fieldBalanceUseTotal, paste0(prefix, agPre), unit, sep = "++")

  ### Residue Demand
  resDemand <- Residues(gdx, level="regglo", products = "kres", output = "resDemand")
  getItems(resDemand, dim = 3.1) <- magpiesets::reportingnames(getItems(resDemand, dim = 3.1))
  getItems(resDemand, dim = 3.2) <- magpiesets::reportingnames(getItems(resDemand, dim = 3.2))
  resDemandCatTotal <- dimSums(resDemand, dim = 3.2)

  resDemand         <- .reportNicely(resDemand,         paste0(prefix, agPre, removalPre), unit)
  resDemandCatTotal <- .reportNicely(resDemandCatTotal, paste0(prefix, agPre, removalPre), unit, sep = "++")

  out <- mbind(biomassTotal, biomassPoolTotal, fieldBalanceUseTotal,
               biomass, resDemandCatTotal, fieldBalance, resDemand)

  return(out)
}
