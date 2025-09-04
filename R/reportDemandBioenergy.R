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
#' @examples
#'
#'   \dontrun{
#'     x <- reportDemandBioenergy()
#'   }

reportDemandBioenergy <- function(gdx, detail = FALSE) {

  # Subdivied bioenergy demand product specific into traditional, 1st generation and 2nd generation
  # This information is just available as in level 'reg' and will be aggregated to 'regglo'

  if(is.null(bioenergy1stTra <- gdx2::readGDX(gdx, "i60_1stgen_bioenergy_dem", react = "silent"))) {
    out <- demandBioenergy(gdx, level = "regglo")
    y   <- dimSums(out, dim = 3)
    getNames(out) <- paste0("Demand|Bioenergy|++|", getNames(out), " (EJ/yr)")
    getNames(y)   <- "Demand|Bioenergy (EJ/yr)"
    out <- mbind(out, y)

  } else {

    kres   <- gdx2::readGDX(gdx, "kres")
    gen1st <- gdx2::readGDX(gdx, "k1st60")
    kbe    <- gdx2::readGDX(gdx, "kbe60")

    bioenergyDemand   <- collapseNames(demand(gdx, level = "regglo", attributes = "ge")[, , "bioenergy"]) / 1000
    bioenergyRes2nd   <- luscale::superAggregate(gdx2::readGDX(gdx, "ov60_2ndgen_bioenergy_dem_residues", select = list(type = "level")),
                                        aggr_type = "sum", level = "regglo") / 1000
    bioenergyCrops2nd <- luscale::superAggregate(gdx2::readGDX(gdx, "ov60_2ndgen_bioenergy_dem_dedicated", select = list(type = "level")),
                                        aggr_type = "sum", level = "regglo") / 1000
    bioenergy1stTra   <- luscale::superAggregate(bioenergy1stTra, aggr_type="sum", level="regglo")/1000

    bioenergyOverflow <- bioenergyDemand - bioenergyCrops2nd - bioenergyRes2nd - bioenergy1stTra
    bioenergy1st      <- add_columns(bioenergy1stTra[, , gen1st],
                                     addnm = setdiff(getNames(bioenergy1stTra), gen1st))
    bioenergy1st[is.na(bioenergy1st)] <- 0
    bioenergyTra      <- add_columns(bioenergy1stTra[, , kres],
                                     addnm = setdiff(getNames(bioenergy1stTra), kres))
    bioenergyTra[is.na(bioenergyTra)] <- 0

    bioenergyBiochar  <- biochar(gdx, indicator = "bc_feedstock_dem", level = "regglo", feedstockAggr = FALSE,
                                 systemAggr = TRUE, attributes = "ge")

    if(!is.null(bioenergyBiochar)) {

      feedstockShareCrops <- bioenergyCrops2nd / dimSums(bioenergyCrops2nd, dim = "kall")
      feedstockShareCrops <- madrat::toolConditionalReplace(feedstockShareCrops,
                                                            conditions = "is.na()", replaceby = 0)
      feedstockShareRes <- bioenergyRes2nd / dimSums(bioenergyRes2nd, dim = "kall")
      feedstockShareRes <- madrat::toolConditionalReplace(feedstockShareRes,
                                                          conditions = "is.na()", replaceby = 0)
      bioenergyBiocharKall <- new.magpie(getRegions(bioenergyBiochar), getYears(bioenergyBiochar),
                                         getItems(bioenergyCrops2nd, dim = 3), fill = NA,
                                         sets = getSets(bioenergyCrops2nd))
      bioenergyBiocharKall[, , kbe]  <- bioenergyBiochar[, , "dedicated"] * feedstockShareCrops[, , kbe]
      bioenergyBiocharKall[, , kres] <- bioenergyBiochar[, , "residues"] * feedstockShareRes[, , kres]

      bioenergyCrops2nd[, , kbe] <- bioenergyCrops2nd[, , kbe] - bioenergyBiocharKall[, , kbe]
      bioenergyRes2nd[, , kres]  <- bioenergyRes2nd[, , kres] - bioenergyBiocharKall[, , kres]
    }

    out <- mbind(magpiesets::reporthelper(x = bioenergyRes2nd + bioenergyCrops2nd,
                              level_zero_name = "Demand|Bioenergy|2nd generation",      detail = detail, dim = 3.1),
                 magpiesets::reporthelper(x = bioenergy1st,
                              level_zero_name = "Demand|Bioenergy|1st generation",      detail = detail, dim = 3.1),
                 magpiesets::reporthelper(x = bioenergyTra,
                              level_zero_name = "Demand|Bioenergy|Traditional Burning", detail = detail, dim = 3.1),
                 magpiesets::reporthelper(x = bioenergyOverflow,
                              level_zero_name = "Demand|Bioenergy|Overproduction",      detail = detail, dim = 3.1))

    if(!is.null(bioenergyBiochar)) {
      out <- mbind(out,
                   magpiesets::reporthelper(x = bioenergyBiocharKall,
                                level_zero_name = "Demand|Bioenergy|Biochar",           detail = detail, dim = 3.1))
    }

    out           <- magpiesets::summationhelper(round(out, 8), sep = "++")
    getNames(out) <- paste0(getNames(out), " (EJ/yr)")
  }

  return(out)
}
