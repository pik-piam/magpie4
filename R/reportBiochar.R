#' @title reportBiochar
#' @description reports production and carbon storage of biochar
#'
#' @export
#'
#' @param gdx GDX file

#' @return Biochar prodcution and carbon storage as MAgPIE object
#' @author Kristine Karstens, Isabelle Weindl
#' @examples
#'
#'   \dontrun{
#'     x <- reportBiochar()
#'   }

reportBiochar <- function(gdx) {

  ### report production values
  biocharProduction  <- biochar(gdx, indicator = "bc_production", level = "regglo", feedstockAggr = TRUE,
                               systemAggr = FALSE, attributes = c("ge", "dm", "c"))
  biocharProduction  <- dimOrder(biocharProduction, perm = c(2, 1), dim = 3)
  getNames(biocharProduction) <- paste0("Production|Biochar.", getNames(biocharProduction))

  out <- mbind(setNames(biocharProduction[, , "ge"],
                        gsub("\\.ge\\.", "\\|Energy content\\|",
                             paste0(getNames(biocharProduction[, , "ge"]), " (EJ/yr)"))),
               setNames(dimSums(biocharProduction[, , "ge"], dim = 3.3), "Production|Biochar|Energy content (EJ/yr)"),
               setNames(biocharProduction[, , "dm"],
                        gsub("\\.dm\\.", "\\|Dry Matter\\|",
                             paste0(getNames(biocharProduction[, , "dm"]), " (Mt DM/yr)"))),
               setNames(dimSums(biocharProduction[, , "dm"], dim = 3.3), "Production|Biochar|Dry Matter (Mt DM/yr)"),
               setNames(biocharProduction[, , "c"],
                        gsub("\\.c\\.", "\\|Carbon\\|",
                             paste0(getNames(biocharProduction[, , "c"]), " (Mt C/yr)"))),
               setNames(dimSums(biocharProduction[, , "c"], dim = 3.3), "Production|Biochar|Carbon (Mt C/yr)"))

    out <- magpiesets::summationhelper(round(out, 8), sep = "++", excludeLevels = 3)

    ### report CDR
    biocharCDR <- biochar(gdx, indicator = "bc_stable_carbon", level = "regglo", feedstockAggr = TRUE,
                          systemAggr = TRUE, attributes = "c")
    biocharCDR <- biocharCDR * 44 / 12 # Mt C/yr to Mt CO2/yr

    ### Cumulative Emissions
    # Always use raw data for cumulative calculation
    biocharCDRCum <- biocharCDR
    # Apply timestep weighting and cumulative sum
    timesteps <- m_yeardiff(gdx)
    biocharCDRCum [, "y1995", ] <- 0
    biocharCDRCum <- biocharCDR * timesteps[, getYears(biocharCDR), ] # annual CDR * time steps length
    biocharCDRCum <- as.magpie(apply(biocharCDRCum, c(1, 3), cumsum)) # sum up over all previous years
    biocharCDRCum <- biocharCDRCum - setYears(biocharCDRCum[, 1995, ], NULL) # normalize to diff 1995
    biocharCDRCum <- biocharCDRCum / 1000 # Convert to Gt

    ### Emissions with filter
    yrFix <- as.numeric(gdx2::readGDX(gdx, "sm_fix_SSP2"))
    years <- getYears(biocharCDR, as.integer = TRUE)
    yrsHist <- years[years > 1995 & years <= yrFix]
    yrsFut  <- years[years >= yrFix]
    # Apply lowpass filter (not applied on 1st time step, applied separately on historic and future period)
    biocharCDRLowpass <- mbind(biocharCDR[, 1995, ],
                               lowpass(biocharCDR[, yrsHist, ], i = 3),
                               lowpass(biocharCDR[, yrsFut, ],  i = 3)[, -1, ])
    out <- mbind(out,
                 setNames(-biocharCDR, "Emissions|CO2|Land RAW|Land-use Change|Biochar (Mt CO2/yr)"),
                 setNames(-biocharCDRLowpass, "Emissions|CO2|Land|Land-use Change|Biochar (Mt CO2/yr)"),
                 setNames(-biocharCDRCum, "Emissions|CO2|Land|Cumulative|Land-use Change|Biochar (Gt CO2)"))

  return(out)
}
