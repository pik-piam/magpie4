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
  
  # Bottom-up reconstruction of NPPact:
  # NPPact = NPPharv + NPPecoMan + NPPecoNat
  # NPPharv: harvested or destroyed NPP ("appropriation")
  # NPPecoMan: biomass remaining in managed ecosystems (including belowground biomass)
  # NPPecoNat: natural vegetation productivity on remaining natural land

  # calculate everything at cluster level and aggregate in the end

  
  #### NPPharv
  
  ##CROPLAND
  # crop production appropriated for human usage
  #cropHarv <- production() # see: https://github.com/FelicitasBeier/magpie4/blob/HANNP/R/production.R

  ## residues appropriated for human usage
  #residuesHarv <- Residues(gdx = gdx, level = "cell", products = "kres",
  #                                  waterAggr = TRUE, output = "fieldBalance")[, , c("burned", "removal")]
  # select: "fieldBalance" and from that select "burned", "removal"
  
  
  ##PASTURE
  # grazed biomass on pastures
  #pastureHarv <- dimSums(readGDX(gdx, "ov_dem_feed", select = list(type = "level")), dim = "kap")
  
  
  ##FORESTRY & FOREST
  # check woodProduction.R
  # timber harvest appropriated for human usage
  #timberHarv <- production() # maybe: https://github.com/FelicitasBeier/magpie4/blob/HANNP/R/production.R
  # Is selective logging in natural forests included in timber harvest (Florian)? 
  # woodfuel harvest
  # losses??? (only using timber and fuelwood may underestimate NPPharv in forests) Florian?
  
  # NPPharv = cropHarv + residuesHarv + pastureHarv + timberHarv

  
  #### NPPecoMan:
  
  ##CROPLAND
  #recycled residues: Residues("fieldBalance") --> select "recycle"
  #below ground residues on cropland: Residues("Biomass"); ResiduesBiomass --> use bg; (ag (already part of fieldBalance)) --> Kristine which function is better?
  
  #fallow: where should this be allocated to? NPPharv or here under NPPecoMan? What happens with the biomass in the end?
  # What happens there in MAgPIE (Benni)? Is there some sort of plant growth?
  # we could use grass NPP
  

  
  ##PASTURE
  # unappropriated pasture biomass (biomass production on pastures (use production.R) minus grazed biomass)
  # What about below-ground biomass?
  
  
  ##FORESTRY
  # unharvested biomass (standing forestry biomass that is not harvested or destroyed)
  

  
  #### NPPecoNat:
  
  ## NPP over natural land
  # NPPecoNat = natural land area × LPJmL NPP density
  
  # What about natural succession? 
  # Should potential NPP be scaled with e.g the ratio between max carbon density and actual carbon density in this grid?

  
  #### NPPluc:
  
  ## Difference between NPPpot and NPPact
  
  # NPPpot
  # based on function in mrlandcore (Feli)

  # NPPact
  # to be reconstructed bottom-up from NPPharv, NPPecoMan, and NPPecoNat
  
  #NPPact <- NPPharv + NPPecoMan + NPPecoNat
  
  
  
  
  #HANPP = NPPharv + deltaNPPluc
  #      = NPPharv + (NPPpot - NPPact)
  #      = NPPharv + (NPPpot - NPPharv - NPPecoMan - NPPecoNat)
  #      = NPPpot - NPPecoMan - NPPecoNat
  
  #out <- HANPP

  return(out)
}
