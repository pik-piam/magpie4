#' @title reportDemandBioenergy
#' @description reports Bioenergy Demand in EJ/yr
#'
#' @export
#'
#' @param gdx GDX file
#' @param detail if detail=F, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @return Bioenergy demand as MAgPIE object (EJ/yr)
#' @author Florian Humpenoeder, Kristine Karstens
#' @importFrom luscale superAggregate
#' @importFrom gdx2 readGDX
#' @importFrom magpiesets reportHelper summationHelper
#' @examples
#'
#'   \dontrun{
#'     x <- reportDemandBioenergy()
#'   }
#'
#' @section Bioenergy demand variables:
#' Name | Unit | Meta
#' ---|---|---
#' Demand\|Bioenergy | EJ/yr | Total bioenergy demand
#' Demand\|Bioenergy\|++\|2nd generation | EJ/yr | Second generation bioenergy demand (dedicated crops and residues)
#' Demand\|Bioenergy\|++\|1st generation | EJ/yr | First generation bioenergy demand (oils, ethanol)
#' Demand\|Bioenergy\|++\|Traditional Burning | EJ/yr | Traditional biomass burning demand
#' @md

reportDemandBioenergy <- function(gdx, detail = FALSE, level = "regglo") {

  # Subdivied bioenergy demand product specific into traditional, 1st generation and 2nd generation
  # This information is just available as in level 'reg' and will be aggregated to 'regglo'

  if (is.null(bioenergy1stTra <- readGDX(gdx, "i60_1stgen_bioenergy_dem", react = "silent"))) {
    out <- demandBioenergy(gdx, level = "regglo")
    y   <- dimSums(out, dim = 3)
    getNames(out) <- paste0("Demand|Bioenergy|++|", getNames(out), " (EJ/yr)")
    getNames(y)   <- "Demand|Bioenergy (EJ/yr)"
    out <- mbind(out, y)

  } else {

    kres   <- readGDX(gdx, "kres")
    gen1st <- readGDX(gdx, "k1st60")
    kbe    <- readGDX(gdx, "kbe60")

    bioenergyDemand   <- collapseNames(demand(gdx, level = "regglo", attributes = "ge")[, , "bioenergy"]) / 1000
    bioenergyRes2nd   <- superAggregateX(readGDX(gdx, "ov60_2ndgen_bioenergy_dem_residues",
                                                 select = list(type = "level")),
                                         aggr_type = "sum", level = "regglo") / 1000
    bioenergyCrops2nd <- superAggregateX(readGDX(gdx, "ov60_2ndgen_bioenergy_dem_dedicated",
                                                 select = list(type = "level")),
                                         aggr_type = "sum", level = "regglo") / 1000
    bioenergy1stTra   <- superAggregateX(bioenergy1stTra, aggr_type = "sum", level = "regglo") / 1000

    bioenergyOverflow <- bioenergyDemand - bioenergyCrops2nd - bioenergyRes2nd - bioenergy1stTra
    bioenergy1st      <- add_columns(bioenergy1stTra[, , gen1st],
                                     addnm = setdiff(getNames(bioenergy1stTra), gen1st))
    bioenergy1st[is.na(bioenergy1st)] <- 0
    bioenergyTra      <- add_columns(bioenergy1stTra[, , kres],
                                     addnm = setdiff(getNames(bioenergy1stTra), kres))
    bioenergyTra[is.na(bioenergyTra)] <- 0

    bioenergyBiochar  <- biochar(gdx, indicator = "bc_feedstock_dem", level = "regglo", feedstockAggr = FALSE,
                                 systemAggr = TRUE, attributes = "ge")

    if (!is.null(bioenergyBiochar)) {

      feedstockShareCrops <- bioenergyCrops2nd / dimSums(bioenergyCrops2nd, dim = "kall")
      feedstockShareCrops <- madrat::toolConditionalReplace(feedstockShareCrops,
                                                            conditions = "is.na()", replaceby = 0)
      feedstockShareRes <- bioenergyRes2nd / dimSums(bioenergyRes2nd, dim = "kall")
      feedstockShareRes <- madrat::toolConditionalReplace(feedstockShareRes,
                                                          conditions = "is.na()", replaceby = 0)
      bioenergyBiocharKall <- new.magpie(getItems(bioenergyBiochar, 1.1),
                                         getItems(bioenergyBiochar, 2),
                                         getItems(bioenergyCrops2nd, 3), fill = 0,
                                         sets = getSets(bioenergyCrops2nd))
      bioenergyBiocharKall[, , kbe]  <- bioenergyBiochar[, , "dedicated"] * feedstockShareCrops[, , kbe] / 1000
      bioenergyBiocharKall[, , kres] <- bioenergyBiochar[, , "residues"] * feedstockShareRes[, , kres] / 1000

      bioenergyCrops2nd[, , kbe] <- bioenergyCrops2nd[, , kbe] - bioenergyBiocharKall[, , kbe]
      bioenergyRes2nd[, , kres]  <- bioenergyRes2nd[, , kres] - bioenergyBiocharKall[, , kres]
    }

    out <- mbind(reporthelper(x = bioenergyRes2nd + bioenergyCrops2nd,
                              level_zero_name = "Demand|Bioenergy|2nd generation",      detail = detail, dim = 3.1),
                 reporthelper(x = bioenergy1st,
                              level_zero_name = "Demand|Bioenergy|1st generation",      detail = detail, dim = 3.1),
                 reporthelper(x = bioenergyTra,
                              level_zero_name = "Demand|Bioenergy|Traditional Burning", detail = detail, dim = 3.1),
                 reporthelper(x = bioenergyOverflow,
                              level_zero_name = "Demand|Bioenergy|Overproduction",      detail = detail, dim = 3.1))

    if (!is.null(bioenergyBiochar)) {
      out <- mbind(out,
                   reporthelper(x = bioenergyBiocharKall,
                                level_zero_name = "Demand|Bioenergy|Biochar",           detail = detail, dim = 3.1))
    }

    out           <- summationhelper(round(out, 8), sep = "++")
    getNames(out) <- paste0(getNames(out), " (EJ/yr)")
  }

  return(out)
}
