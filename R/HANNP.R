#' @title HANPP
#' @description calculates the Human Appropriation of Net Primary Production (HANNP)
#'              based on outputs from MAgPIE gdx file and LPJmL data
#' @export
#'
#' @param gdx         GDX file
#' @param level       Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global)
#'
#' @return magpie object
#' @author Felicitas Beier, Isabelle Weindl
#' @examples
#'   \dontrun{
#'     x <- HANNP(gdx)
#'   }
#'
#' @importFrom madrat toolAggregate

HANPP <- function(gdx, level = "regglo") {

  # Formula (see https://de.wikipedia.org/wiki/Human_Appropriation_of_Net_Primary_Production
  # HANPP = deltaNPPluc + NPPharv
  # deltaNPPluc = NPPpot - NPPact

  # calculate everything at cluster level and aggregate in the end

  # NPPharv
  # crop production appropriated for human usage
  #cropHarv <- production() # see: https://github.com/FelicitasBeier/magpie4/blob/HANNP/R/production.R

  # timber harvest appropriated for human usage
  #timberHarv <- production() # maybe: https://github.com/FelicitasBeier/magpie4/blob/HANNP/R/production.R

  # grass for feed
  #grassFeed <- dimSums(readGDX(gdx, "ov_dem_feed", select = list(type = "level")), dim = "kap")

  # residues appropriated for human usage
  #residueAppropriation <- Residues(gdx = gdx, level = "cell", products = "kres",
  #                                  waterAggr = TRUE, output = "fieldBalance")[, , c("burned", "removal")]
  # select: "fieldBalance" and from that select "burned", "removal"

  # NPPecoAg:
  #recycled residues: Residues("fieldBalance") --> select "recycle"
  #below ground residues on cropland: Residues("Biomass"); ResiduesBiomass --> use bg; (ag (already part of fieldBalance)) --> Kristine which function is better?

  # NPPecoNat
  #fallow: confirmed with Benni: nothing grows there in MAgPIE

  # NPPluc:

  # NPPpot
  # based on function to be written in mrlandcore (Feli)

  # NPPact
  # based on function to be written in mrlandcore (Feli)

  #out <- NPPharv + (NPPpot - NPPact)

  return(out)
}
