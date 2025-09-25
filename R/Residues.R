#' @title Residues
#' @description reads various crop residue (carbon) outputs out of a MAgPIE gdx file
#' @importFrom memoise memoise
#' @export
#'
#' @param gdx         GDX file
#' @param level       Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global)
#' @param products    Selection of products (either "kcr" or "kres")
#' @param waterAggr   Aggregate irrigated and non-irriagted production or not (boolean).
#' @param output      Switch between different outputs: "biomass", "fieldBalance", "resDemand", all
#'
#' @return production as MAgPIE object (unit depends on attributes)
#' @author Kristine Karstens, Michael Crawford
#' @seealso \code{\link{ResidueBiomass}}
#' @examples
#'
#'   \dontrun{
#'     x <- Residues(gdx)
#'   }
#' @importFrom madrat toolAggregate

Residues <- memoise(function(gdx, level = "regglo", products = "kres", waterAggr = TRUE, output = "all"){

  DirectoryChangeTest()

  attr     <- "c" # before dm
  kres     <- readGDX(gdx, "kres")
  kresfull <- c(kres, "res_nouse")
  kcr      <- readGDX(gdx, "kcr")
  kcr2kres <- readGDX(gdx, "kres_kcr")
  missingKcr   <- setdiff(kcr, kcr2kres$kcr)
  missingMap   <- data.frame(kres = rep("res_nouse", length(missingKcr)),
                             kcr  = missingKcr)
  kcr2kresfull <- rbind(kcr2kres, missingMap)
  fmAttributes <- readGDX(gdx, "fm_attributes")[, , kres]

  if (level %in% c("reg", "regglo", "glo")) {

    biomassAgKcr   <- collapseNames(readGDX(gdx, "ov_res_biomass_ag")[, , "level"][, , attr])
    biomassBgKcr   <- collapseNames(readGDX(gdx, "ov_res_biomass_bg")[, , "level"][, , attr])
    recyclingKcr   <- collapseNames(readGDX(gdx, "ov18_res_ag_recycling")[, , "level"][, , attr])
    burnKcr        <- collapseNames(readGDX(gdx, "ov_res_ag_burn", "ov18_res_ag_burn",
                                            format = "first_found")[, , "level"][, , attr])
    removalKcr     <- collapseNames(readGDX(gdx, "ov18_res_ag_removal")[, , "level"][, , attr])
    feedKres       <- dimSums(collapseNames(readGDX(gdx, "ov_dem_feed")[, , "level"][, , kres] *
                                              fmAttributes[, , kres][, , attr],
                                            collapsedim = c("type", "attributes")), dim = 3.1)
    materialKres   <- collapseNames(readGDX(gdx, "ov_dem_material")[, , "level"][, , kres] *
                                      fmAttributes[, , kres][, , attr], collapsedim = c("type", "attributes"))
    bioenergyKres  <- collapseNames(readGDX(gdx, "ov_dem_bioen")[, , "level"][, , kres] *
                                      fmAttributes[, , kres][, , attr], collapsedim = c("type", "attributes"))

    # When aggregating to kres no water specific results are possible
    wSpecific <- any(grepl("^w$", getSets(biomassAgKcr)))
    if (waterAggr == FALSE && "wSpecific != TRUE" ) {
      stop("No water-type specific output available.")
    }

    if (products == "kres"){

      .aggregateKcr2Kres <- function(input) {
        if (wSpecific) input <- dimSums(input, dim = "w")
        madrat::toolAggregate(input, rel = kcr2kresfull, from = "kcr", to = "kres",
                      dim = 3.1, partrel = TRUE)[, , kresfull]
      }
      biomassAgKres      <- .aggregateKcr2Kres(biomassAgKcr)
      biomassBgKres      <- .aggregateKcr2Kres(biomassBgKcr)
      recyclingKres      <- .aggregateKcr2Kres(recyclingKcr)
      burnKres           <- .aggregateKcr2Kres(burnKcr)
      removalKres        <- .aggregateKcr2Kres(removalKcr)

      # ########### TESTING #################
      # prod     <- collapseNames(readGDX(gdx, "ov_prod_reg")[, , "level"])
      # prodKres <- prod[, , kres] * fmAttributes[, , kres][, , "c"]
      # range(removalKres-prodKres)
      #
      # supplyKres     <- collapseNames(readGDX(gdx, "ov_supply")[, , "level"][, , kres]) * fmAttributes[, , kres][, , "c"]
      # range(prodKres-supplyKres)
      # where(abs(prodKres-supplyKres) > 1)$true
      # # $individual
      # # i     t       kall.attributes
      # # LAM "LAM" "y1995" "res_cereals.c"
      # # CAZ "CAZ" "y1995" "res_fibrous.c"
      # # EUR "EUR" "y1995" "res_fibrous.c"
      # # MEA "MEA" "y1995" "res_fibrous.c"
      # # REF "REF" "y1995" "res_fibrous.c"
      # # CAZ "CAZ" "y2000" "res_fibrous.c"
      # # REF "REF" "y2000" "res_fibrous.c"
      # #
      # # $regions
      # # [1] "LAM" "CAZ" "EUR" "MEA" "REF"
      # #
      # # $years
      # # [1] "y1995" "y2000"
      # #
      # # $data
      # # [1] "res_cereals.c" "res_fibrous.c"
      #
      #
      # fbask     <- collapseNames(readGDX(gdx, "im_feed_baskets")[, , kres])
      # bflow     <- collapseNames(readGDX(gdx, "fm_feed_balanceflow")[, , kres])
      # kap     <- readGDX(gdx, "kap")
      # time    <- getYears(prod)
      #
      # feedKres_real <-  dimSums(prod[, , kap] * fbask[, time, kres] + bflow[, time, kres], dim = 3.1) *
      #   fmAttributes[, , kres][, , "c"]
      #
      # range(feedKres - feedKres_real)
      # where(abs(feedKres - feedKres_real) > 1)$true
      # # $individual
      # # i     t       kall.attributes
      # # MEA "MEA" "y1995" "res_cereals.c"
      # # OAS "OAS" "y1995" "res_cereals.c"
      # # SSA "SSA" "y1995" "res_cereals.c"
      # # USA "USA" "y1995" "res_cereals.c"
      # # IND "IND" "y1995" "res_fibrous.c"
      # # LAM "LAM" "y1995" "res_fibrous.c"
      # # USA "USA" "y1995" "res_fibrous.c"
      # # USA "USA" "y2000" "res_fibrous.c"
      # # USA "USA" "y2005" "res_fibrous.c"
      # # REF "REF" "y2010" "res_fibrous.c"
      # #
      # # $regions
      # # [1] "MEA" "OAS" "SSA" "USA" "IND" "LAM" "REF"
      # #
      # # $years
      # # [1] "y1995" "y2000" "y2005" "y2010"
      # #
      # # $data
      # # [1] "res_cereals.c" "res_fibrous.c"

      ########### TESTING #################

      # CHECK
      # [x] FEED DMEAND OTHER fibrous -> basket equation
      # [] check attribute equation

      biomass      <- mbind(add_dimension(biomassAgKres, add = "plantpart", nm = "ag"),
                            add_dimension(biomassBgKres, add = "plantpart", nm = "bg"))

      fieldBalance <- mbind(add_dimension(recyclingKres, add = "fieldbalance", nm = "recycle"),
                            add_dimension(burnKres,      add = "fieldbalance", nm = "burned"),
                            add_dimension(removalKres,   add = "fieldbalance", nm = "removal"))

      resDemand <- mbind(add_dimension(feedKres,      add = "usage", nm = "feed"),
                         add_dimension(materialKres,  add = "usage", nm = "other_util"),
                         add_dimension(bioenergyKres, add = "usage", nm = "bioenergy"))

      check <- round(dimSums(removalKres, dim = 3) - dimSums(resDemand, dim = 3), 5)
      if (any(check != 0)) {
        warning("Sum over all residue removal options and residue removal in total are not matching.")
      }

      check <- round(biomassAgKres - dimSums(fieldBalance, dim = 3.1), 5)
      if (any(check != 0)) {
        warning("Sum over all residue usage options and ag-residue biomass in total are not matching.")
      }

    } else if (products == "kcr"){

      biomass      <- mbind(add_dimension(biomassAgKcr, add = "plantpart", nm = "ag"),
                            add_dimension(biomassBgKcr, add = "plantpart", nm = "bg"))

      fieldBalance <- mbind(add_dimension(recyclingKcr, add = "fieldbalance", nm = "recycle"),
                            add_dimension(burnKcr,      add = "fieldbalance", nm = "burned"),
                            add_dimension(removalKcr,   add = "fieldbalance", nm = "removal"))

      resDemand <- mbind(add_dimension(feedKres,      add = "usage", nm = "feed"),
                         add_dimension(materialKres,  add = "usage", nm = "other_util"),
                         add_dimension(bioenergyKres, add = "usage", nm = "bioenergy"))

      if(wSpecific) {
        weight <- dimSums(removalKcr[, , kcr2kres$kcr], dim = 3.2)
      } else {
        weight <- removalKcr[, , kcr2kres$kcr]
      }
      resDemand <- toolAggregate(resDemand, kcr2kres, weight = weight, from = "kres", to = "kcr", dim = 3.2)

      getSets(resDemand, fulldim = FALSE)[3] <- "usage.kcr.attributes"
      resDemand <- add_columns(resDemand, addnm = missingKcr, dim = 3.2)
      resDemand[is.na(resDemand)] <- 0

      check <- round(dimSums(removalKcr, dim = 3) - dimSums(resDemand, dim = 3), 5)
      if(any(check != 0)) {
        warning(paste0("Sum over all residue removal options and residue removal in total are not matching.
                       Non-matching in the range of ", toString(range(check))))
      }

      check <- round(biomassAgKcr - dimSums(fieldBalance, dim = 3.1), 5)
      if(any(check != 0)) {
        warning(paste0("Sum over all residue usage options and ag-residue biomass in total are not matching.
                       Non-matching in the range of ", toString(range(check))))
      }
    } else { stop(paste0("Product type ", products, " unknown.")) }

    ### reg, regglo, glo aggregation
    biomass      <- gdxAggregate(gdx, biomass,      to = level, absolute = TRUE)
    fieldBalance <- gdxAggregate(gdx, fieldBalance, to = level, absolute = TRUE)
    resDemand    <- gdxAggregate(gdx, resDemand,    to = level, absolute = TRUE)

  } else {
    stop("Level not available.")
  }

  if (output == "biomass") {
    out <- biomass
  } else if (output == "fieldBalance") {
    out <- fieldBalance
  } else if (output == "resDemand") {
    out <- resDemand
  } else if (output == "all") {
    if (products == "kcr" && wSpecific) {
      out <- mbind(collapseNames(dimSums(biomass, dim ="w")),
                   collapseNames(dimSums(fieldBalance, dim ="w")),
                   collapseNames(resDemand))
    } else {
      out <- mbind(collapseNames(biomass),
                   collapseNames(fieldBalance),
                   collapseNames(resDemand))
    }
  } else {
    stop("Output type not available.")
  }

  if (products == "kcr" && waterAggr == TRUE && output %in% c("biomass", "fieldBalance")) {
    out <- dimSums(out, dim = "w")
  }

  return(out)
}
)
