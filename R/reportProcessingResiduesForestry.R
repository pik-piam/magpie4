#' @title reportProcessingResiduesForestry
#' @description Reports processing residues from timber production available for energy use.
#'
#' Processing residues are sawmill byproducts (sawdust, bark, slabs, offcuts) generated
#' during conversion of industrial roundwood to finished products. They are NOT currently
#' tracked as a variable in MAgPIE's GAMS code and represent additional biomass supply
#' for REMIND's bioenergy sector.
#'
#' Note: Logging residues (branches, tops) tracked in `v73_prod_residues` are NOT included
#' here because they already feed into MAgPIE's woodfuel supply equation (`q73_prod_woodfuel`)
#' and would be double-counted if reported separately for REMIND.
#'
#' @details
#' ## Full mass balance of timber harvest
#'
#' When 1 hectare of forest is harvested, the total biomass removed is:
#' - **Stem biomass** = `pm_growing_stock` (tDM/ha)
#' - **Logging residues** (branches, tops) = 15% of stem, tracked in `v73_prod_residues`
#' - **Total removed** = stem x 1.15
#'
#' The model splits stem production between industrial roundwood (`vm_prod("wood")`)
#' and woodfuel (`vm_prod("woodfuel")`). Only the industrial roundwood fraction enters
#' processing facilities and generates processing residues. Woodfuel is burned directly.
#' Logging residues already feed into MAgPIE's woodfuel supply (NOT reported here).
#'
#' ```
#' 1 hectare harvested -> total biomass = stem x 1.15
#' |
#' +-- Stem (100% = vm_prod, split between "wood" and "woodfuel" by the model)
#' |   If allocated to industrial roundwood (vm_prod("wood")) -> processing facilities:
#' |   |
#' |   +-- Sawnwood pathway (~57% of IR):
#' |   |   +-- 50% finished sawnwood
#' |   |   +-- 43% processing residues (sawdust, slabs, bark, offcuts) -> THIS FUNCTION
#' |   |   +--  7% losses
#' |   |
#' |   +-- Pulpwood pathway (~35% of IR):
#' |   |   +-- 95-100% utilized (black liquor burned internally at pulp mill)
#' |   |   +-- 0-5% solid residues available externally
#' |   |
#' |   +-- Other industrial roundwood (~8% of IR):
#' |       +-- Assumed similar to sawnwood (43% residue rate)
#' |
#' +-- Logging residues (15% of stem = branches, tops)
#'     -> Already in MAgPIE woodfuel via v73_prod_residues (NOT reported here)
#' ```
#'
#' ## Processing residue rate
#'
#' The rate is derived from FAO product composition of industrial roundwood
#' (from `f73_prod_specific_timber`, stable at these shares across 1965-2015)
#' and product-specific recovery rates:
#'
#' | Product | Share of IR | Recovery | Residue rate | Source |
#' |---|---|---|---|---|
#' | Sawnwood (sawlogs + veneer) | ~57% | 50% | **43%** | FAO/UNECE; Mantau 2012 UNECE DP-51 |
#' | Pulpwood | ~35% | 95-100% | **0-5%** | Black liquor burned internally |
#' | Other industrial roundwood | ~8% | ~50% | **43%** | Assumed same as sawnwood |
#'
#' Production-weighted rate: 0.57 x 0.43 + 0.35 x 0.025 + 0.08 x 0.43 = **0.29**
#' Rounded to **0.30** (stable at 30-35% across 1965-2015 FAO data).
#'
#' ## Why only industrial roundwood, not woodfuel
#'
#' Processing residues are computed from industrial roundwood supply (`ov_supply("wood")`)
#' only. Woodfuel (`ov_supply("woodfuel")`) is excluded because it is burned directly
#' for energy — it does not pass through sawmills or pulp mills and therefore generates
#' no processing residues.
#'
#' ## Why all processing residues go to energy
#'
#' In reality, some processing residues are used for pulp or particleboard. However,
#' MAgPIE models all wood products as aggregate "industrial roundwood" (`kforestry = wood`).
#' The downstream split into sawnwood, pulp, and panels is not modeled — there is no
#' competing use for processing residues within MAgPIE. Therefore, all processing
#' residues are assumed available for energy use.
#'
#' ## Construction wood
#'
#' `calcConstructionWoodDemand` in mrcommons applies a factor 2 to construction wood demand,
#' reflecting a 50% sawmill recovery rate (Churkina et al. 2020). This means half of the
#' harvested roundwood for construction becomes processing waste. These residues are clean,
#' uniform offcuts with high energy recovery potential. Reported separately from general
#' processing residues.
#'
#' ## What this function does NOT include
#'
#' - **Logging residues** (branches, tops): already in MAgPIE woodfuel (`v73_prod_residues`)
#' - **Post-consumer waste wood** (demolition, furniture): excluded for consistency with
#'   `carbonLTS`, which tracks CO2 from HWP decay using IPCC first-order decay (half-lives:
#'   sawnwood 30yr, construction 60yr, pulpwood 2yr). Adding end-of-life energy recovery
#'   would require splitting HWP outflow into burned vs decayed fractions — assumptions
#'   not currently in the model. The CO2 from HWP decay is already reported; the energy
#'   from burning waste wood is not tracked to avoid parallel lifetime assumptions.
#' - **Black liquor**: burned internally at pulp mills, not available for external energy
#'
#' ## References
#'
#' - FAO/UNECE conversion factors for sawnwood recovery rates
#' - Mantau, U. (2012). Wood flows in Europe (EU27). UNECE/FAO Discussion Paper 51 (DP-51).
#' - Oswalt et al. (2019). Forest Resources of the United States. USDA.
#' - Churkina et al. (2020). Buildings as a global carbon sink. Nature Sustainability.
#'
#' @export
#'
#' @param gdx GDX file
#' @param level Level of regional aggregation ("regglo" by default)
#' @return MAgPIE object with processing residues in PJ/yr
#' @author Florian Humpenoeder
#' @importFrom magclass dimSums setNames mbind
#' @importFrom gdx2 readGDX
#' @examples
#' \dontrun{
#'   x <- reportProcessingResiduesForestry(gdx)
#' }
#'
#' @section Variables:
#' Name | Unit | Meta
#' ---|---|---
#' Residues for energy\|Forestry\|+\|Processing residues from construction wood | PJ/yr | Sawmill waste from construction wood (50% of demand)
#' Residues for energy\|Forestry\|+\|Processing residues from other roundwood | PJ/yr | 30% of non-construction roundwood
#' Residues for energy\|+\|Forestry | PJ/yr | Total forestry processing residues for energy
#' @md

reportProcessingResiduesForestry <- function(gdx, level = "regglo") {

  if (suppressWarnings(is.null(readGDX(gdx, "fcostsALL")))) {
    message("Disabled (no timber) ", appendLF = FALSE)
    return(NULL)
  }

  ge <- 18 # GJ per tDM, so mio tDM * 18 = PJ

  # Processing residue rate: 30% of industrial roundwood
  # Derived from FAO product composition and sawmill recovery rates:
  # sawnwood (~57% of IR, 43% residue) + pulpwood (~35%, 0-5% solid residue) +
  # other (~8%, 43% residue) = 29%, rounded to 30%.
  # Stable at 30-35% across 1965-2015 FAO data.
  processingResidueRate <- 0.30

  # Total industrial roundwood supply (mio tDM)
  woodSupply <- dimSums(
    readGDX(gdx, "ov_supply", select = list(type = "level"))[, , "wood"],
    dim = 3
  )

  # --- 1. Construction wood processing residues ---
  constrWood <- readGDX(gdx, "p73_demand_constr_wood", react = "silent")
  if (!is.null(constrWood) && any(constrWood > 0)) {
    # p73_demand_constr_wood includes factor 2 from preprocessing.
    # 50% of this demand is processing waste at the sawmill.
    constrResidues <- constrWood * 0.5 * ge
  } else {
    constrResidues <- woodSupply * 0
    constrWood <- woodSupply * 0
  }

  # --- 2. Processing residues from non-construction industrial roundwood ---
  nonConstrWood <- woodSupply - constrWood
  nonConstrWood[nonConstrWood < 0] <- 0
  otherResidues <- nonConstrWood * processingResidueRate * ge

  # --- Aggregate to requested level ---
  constrResidues <- gdxAggregate(gdx, constrResidues, to = level, absolute = TRUE)
  otherResidues <- gdxAggregate(gdx, otherResidues, to = level, absolute = TRUE)

  # --- Format output ---
  unit <- "PJ/yr"
  total <- constrResidues + otherResidues

  out <- mbind(
    setNames(total, paste0("Residues for energy|+|Forestry (", unit, ")")),
    setNames(constrResidues,
             paste0("Residues for energy|Forestry|+|Processing residues from construction wood (", unit, ")")),
    setNames(otherResidues,
             paste0("Residues for energy|Forestry|+|Processing residues from other roundwood (", unit, ")"))
  )

  return(out)
}
