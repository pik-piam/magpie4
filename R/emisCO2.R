#' @title emisCO2
#' @description reads detailed CO2 emissions out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation;
#'                "reg" (regional),
#'                "glo" (global),
#'                "regglo" (regional and global) or
#'                any other aggregation level defined in superAggregate
#' @param unit "element" or "gas";
#'               "element": co2_c in Mt C/yr
#'               "gas": co2_c Mt CO2/yr
#' @param sum_cpool aggregate carbon pools (TRUE), below ground (soilc) and
#'                  above ground (vegc and litc) will be reported, if FALSE
#' @param sum_land TRUE (default) or FALSE. Sum over land types (TRUE)
#'                 or report land-type specific emissions (FALSE).
#' @param cumulative Logical; Determines if emissions are reported annually
#'                  (FALSE) or cumulative (TRUE). The starting point for
#'                  cumulative emissions is y1995.
#' @param baseyear Baseyear used for cumulative emissions (default = 1995)
#' @param lowpass number of lowpass filter iterations (default = 3)
#' @param stock Carbon stocks. If NULL, carbonstock function is called instead.
#' @return CO2 emissions as MAgPIE object (unit depends on \code{unit})
#' @author Florian Humpenoeder
#' @importFrom magclass new.magpie getCells lowpass setNames getNames getYears setYears
#' @examples
#' \dontrun{
#' x <- emisCO2(gdx)
#' }
#'
emisCO2 <- function(gdx, file = NULL, level = "cell", unit = "gas",
  sum_cpool = TRUE, sum_land = TRUE, cumulative = FALSE,
  baseyear = 1995, lowpass = 3, stock = NULL) {
  
  # calc difference in  carbon density between age-classes for regrowth
  # emissions (within a given timestep)
  .ac_diff <- function(x, timestep_length) {
    a <- x
    a[, , ] <- 0
    ac <- getNames(x, dim = "ac")
    year <- getYears(x, as.integer = TRUE)
    
    # emis regrowth 2005 = (90 tc/ha in ac45 - 100 tc/ha in ac50) * 10 Mha
    # area 2005 in ac50 * diff carbon density 2005 between ac45 and ac50
    # acx excluded because area is accumulating in acx.
    # This would overestimate regrowth emissions.
    for (t in 1:nyears(x)) {
      if (t == 1) shifter <- 1 else shifter <- (year[t] - year[t - 1]) / 5
      ac_sub <- ac[(1 + shifter):(length(ac) - 1)]
      ac_sub_before <- ac[(1 + shifter):(length(ac) - 1) - shifter]
      a[, t, ac_sub] <- setNames(x[, t, ac_sub_before],
        getNames(x[, t, ac_sub])) - x[, t, ac_sub]
      # account for regrwoth of age-classes within time step length
      a[, t, "ac0"] <- -x[, t, "ac0"]
      ac_est <- ac[1:shifter]
      if (length(ac_est) > 1) {
        ac_est <- ac_est[-1]
        ac_est_before <- ac[1:shifter - 1]
        a[, t, ac_est] <- setNames(x[, t, ac_est_before],
          getNames(x[, t, ac_est])) - x[, t, ac_est]
      }
    }
    a <- a / timestep_length
    return(a)
  }
  
  # calc difference in  carbon density between time steps for cc emissions
  .t_diff <- function(x, timestep_length) {
    a <- x
    a[, , ] <- 0
    
    # cd 1995 - cd 2000
    # cd 2000 - cd 2005
    for (t in 2:nyears(x)) {
      a[, t, ] <- setYears(x[, t - 1, ], getYears(x[, t, ])) - x[, t, ]
    }
    a <- a / timestep_length
    return(a)
  }
  
  # calc emissions for non-age class land types
  # crop,past,urban,primforest
  .emis_nac <- function(gdx, b = dummy, type = "cc") {
    timestep_length <- readGDX(gdx, "im_years", react = "silent")
    if (is.null(timestep_length)) timestep_length <- timePeriods(gdx)
    
    ov_land <- readGDX(gdx, "ov_land", select = list(type = "level"))
    names(dimnames(ov_land))[1] <- "j"
    p32_land <- landForestry(gdx, level = "cell")
    
    if (type == "cc") {
      fm_carbon_density <- readGDX(gdx,
        "fm_carbon_density")[, getYears(ov_land), ]
      names(dimnames(fm_carbon_density))[2] <- "t"
      fm_carbon_density <- .t_diff(fm_carbon_density, timestep_length)
    }
    
    somOn <- !is.element("soilc",
      getNames(readGDX(gdx, "pm_carbon_density_ac"),
        dim = 2))
    dynSom <- !is.null(readGDX(gdx, "ov59_som_pool", react = "silent"))
    
    if (somOn) {
      
      agPools <- c("litc", "vegc")
      
      # test dynamic vs. static
      if (dynSom) {
        
        pools59   <- readGDX(gdx, "pools59", "land", types = "sets",
          react = "silent", format = "first_found")
        pools59 <- pools59[-which(pools59 == "forestry")]
        
        cshare    <- cshare(gdx, level = "cell", noncrop_aggr = FALSE,
          reference = "actual")[, , "total", invert = TRUE]
        cshare[is.na(cshare)]     <- 1
        
        top <- readGDX(gdx, "f59_topsoilc_density")[, getYears(cshare), ]
        top <- .t_diff(top, timestep_length)
        sub <- readGDX(gdx, "i59_subsoilc_density")[, getYears(cshare), ]
        sub <- .t_diff(sub, timestep_length)
        
        b[, , "crop"][, , agPools]         <- fm_carbon_density[, , "crop"][, , agPools]       * ov_land[, , "crop"]
        b[, , "past"][, , agPools]         <- fm_carbon_density[, , "past"][, , agPools]       * ov_land[, , "past"]
        b[, , "urban"]                     <- fm_carbon_density[, , "urban"]                  * ov_land[, , "urban"]
        b[, , "primforest"][, , agPools]   <- fm_carbon_density[, , "primforest"][, , agPools] * ov_land[, , "primforest"]
        
        b[, , pools59][, , "soilc"]          <-  (top * cshare[, , pools59] + sub) * ov_land[, , pools59]
        b[, , "forestry_aff"][, , "soilc"]   <- (top * collapseNames(cshare[, , "forestry"]) + sub) * dimSums(p32_land[, , "aff"], dim = 3)
        b[, , "forestry_ndc"][, , "soilc"]   <- (top * collapseNames(cshare[, , "forestry"]) + sub) * dimSums(p32_land[, , "ndc"], dim = 3)
        b[, , "forestry_plant"][, , "soilc"] <- (top * collapseNames(cshare[, , "forestry"]) + sub) * dimSums(p32_land[, , "plant"], dim = 3)
        
      } else {
        
        i59_topsoilc_density     <- readGDX(gdx, "i59_topsoilc_density")[, getYears(ov_land), ]
        i59_topsoilc_density <- .t_diff(i59_topsoilc_density, timestep_length)
        i59_subsoilc_density     <- readGDX(gdx, "i59_subsoilc_density")[, getYears(ov_land), ]
        i59_subsoilc_density <- .t_diff(i59_subsoilc_density, timestep_length)
        
        # cropland, pasture, urban land and primforest is simple
        b[, , "crop"][, , agPools]           <- fm_carbon_density[, , "crop"][, , agPools] * ov_land[, , "crop"]
        b[, , "crop"][, , "soilc"]           <- (i59_topsoilc_density + i59_subsoilc_density)   * ov_land[, , "crop"]
        b[, , "past"]                        <- fm_carbon_density[, , "past"]                     * ov_land[, , "past"]
        b[, , "urban"]                       <- fm_carbon_density[, , "urban"]                    * ov_land[, , "urban"]
        b[, , "primforest"]                  <- fm_carbon_density[, , "primforest"]               * ov_land[, , "primforest"]
        b[, , "secdforest"][, , "soilc"]     <- fm_carbon_density[, , "secdforest"][, , "soilc"]    * ov_land[, , "secdforest"]
        b[, , "forestry_aff"][, , "soilc"]   <- collapseNames(fm_carbon_density[, , "forestry"])[, , "soilc"]      * dimSums(p32_land[, , "aff"], dim = 3)
        b[, , "forestry_ndc"][, , "soilc"]   <- collapseNames(fm_carbon_density[, , "forestry"])[, , "soilc"]      * dimSums(p32_land[, , "ndc"], dim = 3)
        b[, , "forestry_plant"][, , "soilc"] <- collapseNames(fm_carbon_density[, , "forestry"])[, , "soilc"]      * dimSums(p32_land[, , "plant"], dim = 3)
        b[, , "other"][, , "soilc"]          <- fm_carbon_density[, , "other"][, , "soilc"]         * ov_land[, , "other"]
      }
      
    } else {
      # cropland, pasture, urban land and primforest is simple
      b[, , "crop"]                  <- fm_carbon_density[, , "crop"] * ov_land[, , "crop"]
      b[, , "past"]                  <- fm_carbon_density[, , "past"] * ov_land[, , "past"]
      b[, , "urban"]                 <- fm_carbon_density[, , "urban"] * ov_land[, , "urban"]
      b[, , "primforest"]            <- fm_carbon_density[, , "primforest"] * ov_land[, , "primforest"]
    }
    b[, 1, ] <- NA
    return(b)
  }
  
  # calc emissions for age class land types
  # forestry,secdforest,other
  .emis_ac <- function(gdx, b = dummy, type = "cc") {
    timestep_length <- readGDX(gdx, "im_years", react = "silent")
    if (is.null(timestep_length)) timestep_length <- timePeriods(gdx)
    
    if (type == "cc") {
      # do stuff
      pm_carbon_density_ac <- readGDX(gdx, "pm_carbon_density_ac")[, getYears(timestep_length), ]
      pm_carbon_density_ac <- .t_diff(pm_carbon_density_ac, timestep_length)
      p32_carbon_density_ac <- readGDX(gdx, "p32_carbon_density_ac", react = "quiet")
      if (!is.null(p32_carbon_density_ac)) p32_carbon_density_ac <- .t_diff(p32_carbon_density_ac, timestep_length)
    } else if (type == "regrowth") {
      pm_carbon_density_ac <- readGDX(gdx, "pm_carbon_density_ac")[, getYears(timestep_length), ]
      pm_carbon_density_ac <- .ac_diff(pm_carbon_density_ac, timestep_length)
      p32_carbon_density_ac <- readGDX(gdx, "p32_carbon_density_ac", react = "quiet")
      if (!is.null(p32_carbon_density_ac)) p32_carbon_density_ac <- .ac_diff(p32_carbon_density_ac, timestep_length)
    }
    
    somOn <- !is.element("soilc", getNames(pm_carbon_density_ac, dim = 2))
    agPools <- c("litc", "vegc")
    
    # forestry land
    ####################
    p32_land <- landForestry(gdx, level = "cell")
    if (is.null(p32_land)) {
      b[, , "forestry_plant"] <- 0
    } else {
      names(dimnames(p32_land))[1] <- "j"
      if (!is.null(p32_carbon_density_ac)) {
        if (somOn) {
          b[, , "forestry_aff"][, , agPools]  <- dimSums(collapseNames(p32_carbon_density_ac[, , "aff"] * p32_land[, , "aff"]), dim = c(3.1))
          b[, , "forestry_ndc"][, , agPools]  <- dimSums(collapseNames(p32_carbon_density_ac[, , "ndc"] * p32_land[, , "ndc"]), dim = c(3.1))
          b[, , "forestry_plant"][, , agPools]  <- dimSums(collapseNames(p32_carbon_density_ac[, , "plant"] * p32_land[, , "plant"]), dim = c(3.1))
        } else {
          b[, , "forestry_aff"] <- dimSums(collapseNames(p32_carbon_density_ac[, , "aff"] * p32_land[, , "aff"]), dim = c(3.1))
          b[, , "forestry_ndc"] <- dimSums(collapseNames(p32_carbon_density_ac[, , "ndc"] * p32_land[, , "ndc"]), dim = c(3.1))
          b[, , "forestry_plant"] <- dimSums(collapseNames(p32_carbon_density_ac[, , "plant"] * p32_land[, , "plant"]), dim = c(3.1))
        }
      } else {
        if (somOn) {
          b[, , "forestry_aff"][, , agPools] <- dimSums(pm_carbon_density_ac * collapseNames(p32_land[, , "aff"]), dim = 3.1)
          b[, , "forestry_ndc"][, , agPools] <- dimSums(pm_carbon_density_ac * collapseNames(p32_land[, , "ndc"]), dim = 3.1)
          b[, , "forestry_plant"][, , agPools] <- dimSums(pm_carbon_density_ac * collapseNames(p32_land[, , "plant"]), dim = 3.1)
        } else {
          b[, , "forestry_aff"] <- dimSums(pm_carbon_density_ac * collapseNames(p32_land[, , "aff"]), dim = 3.1)
          b[, , "forestry_ndc"] <- dimSums(pm_carbon_density_ac * collapseNames(p32_land[, , "ndc"]), dim = 3.1)
          b[, , "forestry_plant"] <- dimSums(pm_carbon_density_ac * collapseNames(p32_land[, , "plant"]), dim = 3.1)
        }
      }
    }
    ####################
    
    # secdforest
    ####################
    p35_secdforest <- readGDX(gdx, "p35_secdforest", react = "quiet")
    if (is.null(p35_secdforest)) {
      b[, , "secdforest"] <- 0
    } else {
      names(dimnames(p35_secdforest))[1] <- "j"
      if (dim(p35_secdforest)[3] == 122) p35_secdforest <- collapseNames(p35_secdforest[, , "after"])
      if (somOn) {
        b[, , "secdforest"][, , agPools] <- dimSums(pm_carbon_density_ac * p35_secdforest, dim = 3.1)
      } else {
        b[, , "secdforest"] <- dimSums(pm_carbon_density_ac * p35_secdforest, dim = 3.1)
      }
    }
    ####################
    
    # other land
    ####################
    p35_other <- readGDX(gdx, "p35_other", react = "quiet")
    if (is.null(p35_other)) {
      b[, , "other"] <- 0
    } else {
      names(dimnames(p35_other))[1] <- "j"
      if (dim(p35_other)[3] == 122) p35_other <- collapseNames(p35_other[, , "after"])
      if (somOn) {
        b[, , "other"][, , agPools] <- dimSums(pm_carbon_density_ac * p35_other, dim = 3.1)
      } else {
        b[, , "other"] <- dimSums(pm_carbon_density_ac * p35_other, dim = 3.1)
      }
    }
    ####################
    b[, 1, ] <- NA
    return(b)
  }
  
  agPools <- c("vegc", "litc")
  
  ### emis_total
  # calc total emissions as difference between carbon stocks,
  # divided by time step length.
  # should be equal to vm_btm_cell(j,emis_co2,"co2_c")
  timestep_length <- readGDX(gdx, "im_years", react = "silent")
  if (is.null(timestep_length)) timestep_length <- timePeriods(gdx)
  stock <- carbonstock(gdx, level = "cell", sum_cpool = FALSE, sum_land = FALSE)
  #  a <- new.magpie(getCells(stock),getYears(stock),getNames(stock),fill = 0); a[,1,] <- NA; dummy <- a;
  a <- stock
  a[, , ] <- 0
  a[, 1, ] <- NA
  dummy <- a
  t <- 2:length(timestep_length)
  a[, t, ] <- (setYears(stock[, t - 1, ], getYears(a[, t, ])) - stock[, t, ]) /
    timestep_length[, t, ]
  emis_total <- a
  
  ### emis_cc
  # vegc,litc,soilc #natural + indirect human effects
  # calc cc emissions by multiplication of area with carbon density
  # from previous time step.
  # example for 2005; previous time step 2000 -> 5 year time step length
  # emis cc 2005 = ((100 tc/ha in ac50 in 2000 - 90 tc/ha in ac50 in 2005) *
  #                                10 Mha in 2005) / 5 years = 20 MtC/yr
  emis_cc <- .emis_nac(gdx, b = dummy, type = "cc")
  emis_cc <- .emis_ac(gdx, b = emis_cc, type = "cc")
  
  ### emis_regrowth
  # vegc,litc; soilc=0 #direct human + natural effects
  # calc regrowth emissions by multiplication of age-class specific area with
  # increase of carbon density in age-classes according to the time step length
  # example for 2005; previous time step 2000 -> 5 year time step length
  # emis regrowth 2005 = ((90 tc/ha in ac45 in 2005 - 100 tc/ha in ac50 in 2005)
  #                                  * 10 Mha in 2005) / 5 years = -20 MtC/yr
  emis_regrowth <- .emis_ac(gdx, b = dummy, type = "regrowth")
  
  ### emis_degrad
  # calc emissios from forest degradation (secdforest and primforest),
  # e.g. shifting_agriculture
  # emis_degrad = (area damaged * carbon density) / time step length
  emis_degrad <- dummy
  p35_disturbance_loss_secdf <- readGDX(gdx, "p35_disturbance_loss_secdf", react = "quiet")
  p35_disturbance_loss_primf <- readGDX(gdx, "p35_disturbance_loss_primf", react = "quiet")
  if (all(!is.null(p35_disturbance_loss_secdf), !is.null(p35_disturbance_loss_primf))) {
    agPools <- c("vegc", "litc")
    pm_carbon_density_ac <- readGDX(gdx, "pm_carbon_density_ac")[, getYears(timestep_length), ]
    emis_degrad[, , "secdforest"][, , agPools] <- dimSums(p35_disturbance_loss_secdf * pm_carbon_density_ac[, , agPools], dim = "ac")
    fm_carbon_density <- readGDX(gdx, "fm_carbon_density")[, getYears(emis_degrad), ]
    names(dimnames(fm_carbon_density))[2] <- "t"
    emis_degrad[, , "primforest"][, , agPools] <- p35_disturbance_loss_primf * fm_carbon_density[, , "primforest"][, , agPools]
  }
  emis_degrad <- emis_degrad / timestep_length
  emis_degrad[, 1, ] <- NA
  
  
  ## Emissions from deforestation ---
  # For land types with age classes, deforestation emissions are calculated as the difference between
  # realized forest area and a counterfactual forest area, where all forest area in age class x is 
  # assumed to progress to age class x+1 in time t+1.
  .calcChangeAC <- function(forest) {
    
    counterfactual <- forest
    counterfactual[ , , ] <- 0
    
    ageClasses <- getItems(forest, dim = 3)
    years <- getItems(forest, dim = 2)
    for (t in seq(from = 2, to = length(years))) {
      for (ac in seq(from = 2, to = length(ageClasses))) {
        if (ageClasses[ac] == "acx") {
          # The largest age class simply accumulates
          counterfactual[ , t, ac] <- forest[ , t - 1, "ac155"] + forest[ , t - 1, "acx"] 
        } else {
          counterfactual[ , t, ac] <- forest[ , t - 1, ac - 1]
        }
      }
    }
    
    change <- counterfactual - forest
    change[change < 0] <- 0
    
    return(change)
  }
  
  emis_deforestation <- dummy 
  emis_other_land <- dummy
  
  ### Find changes in secdf (has age classes) ---
  secdforest <- collapseNames(readGDX(gdx, "ov35_secdforest")[, , "level"])
  secdforest_change <- .calcChangeAC(secdforest)
  
  ### Find changes in other land (has age classes) ----
  other <- collapseNames(readGDX(gdx, "ov35_other")[, , "level"])
  other_change <- .calcChangeAC(other)
  
  ### Find changes in primf (does not have age classes) ----
  primforest <- collapseNames(readGDX(gdx, "ov_land")[, , "primforest"][, , "level"])
  primforest_change <- primforest[, 1, ]
  primforest_change[, ,] <- 0
  for (t in 2:nyears(primforest)) {
    temp <- setYears(primforest[, t - 1, ], getYears(primforest[, t, ])) - primforest[, t, ]
    temp[temp < 0] <- 0 # if primforest increased in future, it does not count as "deforestation"
    primforest_change <- mbind(primforest_change,temp)
  }
  
  ### Calculate corresponding carbon densities changes due to deforestation of secondary and primary forest
  if (all(!is.null(secdforest_change), !is.null(primforest_change))) {
    agPools <- c("vegc", "litc")
    
    # secondary forest
    pm_carbon_density_ac <- readGDX(gdx, "pm_carbon_density_ac")[, getYears(timestep_length), ]
    emis_deforestation[, , "secdforest"][, , agPools] <- dimSums(pm_carbon_density_ac[, , agPools] * secdforest_change, dim = "ac")
    
    # primary forest
    fm_carbon_density <- readGDX(gdx, "fm_carbon_density")[, getYears(emis_deforestation), ]
    names(dimnames(fm_carbon_density))[2] <- "t"
    emis_deforestation[, , "primforest"][, , agPools] <- primforest_change * fm_carbon_density[, , "primforest"][, , agPools]
  }
  emis_deforestation <- emis_deforestation / timestep_length
  emis_deforestation[, 1, ] <- NA
  
  ### Calculate corresponding carbon densities changes due to deforestation of other land
  if (all(!is.null(other_change))) {
    agPools <- c("vegc", "litc")
    pm_carbon_density_ac <- readGDX(gdx, "pm_carbon_density_ac")[, getYears(timestep_length), ]
    emis_other_land[, , "other"][, , agPools] <- dimSums(pm_carbon_density_ac[, , agPools] * other_change, dim = "ac")
  }
  emis_other_land <- emis_other_land / timestep_length
  emis_other_land[, 1, ] <- NA
  
  # ### emis_harvest (wood)
  # emis_harvest <- dummy
  # ov_hvarea_forestry <- readGDX(gdx,"ov_hvarea_forestry",select = list(type="level"),react = "silent")
  # ov_hvarea_secdforest <- readGDX(gdx,"ov_hvarea_secdforest",select = list(type="level"),react = "silent")
  # ov_hvarea_primforest <- readGDX(gdx,"ov_hvarea_primforest",select = list(type="level"),react = "silent")
  # ov_hvarea_other <- readGDX(gdx,"ov_hvarea_other",select = list(type="level"),react = "silent")
  # pm_carbon_density_ac_forestry <- readGDX(gdx,"pm_carbon_density_ac_forestry",react = "silent")
  # pm_carbon_density_ac <- readGDX(gdx,"pm_carbon_density_ac",react = "silent")
  # pm_carbon_density_ac <- readGDX(gdx,"pm_carbon_density_ac",react = "silent")
  # fm_carbon_density <- readGDX(gdx,"fm_carbon_density",react = "silent")
  # if (all(!is.null(ov_hvarea_forestry),!is.null(ov_hvarea_secdforest))) {
  #   names(dimnames(pm_carbon_density_ac_forestry))[2] <- "t"
  #   names(dimnames(fm_carbon_density))[2] <- "t"
  #   emis_harvest[,,"forestry_plant"][,,agPools] <- dimSums(ov_hvarea_forestry * pm_carbon_density_ac_forestry[,getYears(emis_harvest),agPools], dim="ac") / timestep_length
  #   emis_harvest[,,"secdforest"][,,agPools] <- dimSums(ov_hvarea_secdforest * pm_carbon_density_ac[,getYears(emis_harvest),agPools], dim="ac") / timestep_length
  #   emis_harvest[,,"primforest"][,,agPools] <- collapseNames(ov_hvarea_primforest * fm_carbon_density[,getYears(emis_harvest),"primforest"][,,agPools]) / timestep_length
  #   emis_harvest[,,"other"][,,agPools] <- dimSums(ov_hvarea_other * pm_carbon_density_ac[,getYears(emis_harvest),agPools], dim="ac") / timestep_length
  # }
  # emis_harvest[,1,] <- NA
  
  ### emis_lu
  # calc emis_lu as residual
  # emis_lu includes all land-related process-based emissions
  emis_lu <- emis_total - emis_cc
  
  ### emis_luc
  # vegc,litc,soilc #Direct human effect
  # emis_luc includes gross land-use changes emissions
  emis_luc <- emis_lu - (emis_regrowth + emis_deforestation + emis_other_land)
  
  # assign proper names
  emis_total     <- add_dimension(emis_total, dim = 3.3,
    nm = "total", add = "type")
  emis_cc        <- add_dimension(emis_cc, dim = 3.3,
    nm = "cc", add = "type")
  emis_lu        <- add_dimension(emis_lu, dim = 3.3,
    nm = "lu", add = "type")
  emis_luc       <- add_dimension(emis_luc, dim = 3.3,
    nm = "lu_luc", add = "type")
  emis_regrowth  <- add_dimension(emis_regrowth, dim = 3.3,
    nm = "lu_regrowth", add = "type")
  emis_degrad    <- add_dimension(emis_degrad, dim = 3.3,
    nm = "lu_degrad", add = "type")
  # emis_harvest <- add_dimension(emis_harvest, dim = 3.3,
  #                               nm = "lu_harvest", add = "type")
  
  emis_deforestation    <- add_dimension(emis_deforestation, dim = 3.3,
    nm = "lu_deforestation", add = "type")
  
  emis_other_land       <- add_dimension(emis_other_land, dim = 3.3,
    nm = "lu_other_conversion", add = "type")
  
  # bind together
  a <- mbind(emis_total, emis_cc, emis_lu, emis_luc, emis_regrowth, emis_degrad, emis_deforestation, emis_other_land)
  
  # calc emis_total
  # calc emis_cc #vegc,litc,soilc #natural + indirect human effects
  # calc emis_regrowth #vegc,litc; soilc=0 #direct human + natural effects
  # calc emis_harvest #vegc,litc; soilc=0 #Direct human effect
  # calc emis_degrad #vegc,litc; soilc=0 #Natural effect
  # calc emis_luc = emis_total - sum(emis_cc + emis_regrowth + emis_harvest +
  #                                    emis_degrad)
  #                                  #vegc,litc,soilc #Direct human effect
  
  if (sum_cpool) {
    a <- dimSums(a, dim = "c_pools")
  } else {
    below <- dimSums(a[, , "soilc"], dim = c("c_pools"))
    below <- add_dimension(below, dim = 3.2,
      nm = "Below Ground Carbon", add = "c_pools")
    above <- dimSums(a[, , c("vegc", "litc")], dim = c("c_pools"))
    above <- add_dimension(above, dim = 3.2,
      nm = "Above Ground Carbon", add = "c_pools")
    a <- mbind(below, above)
  }
  
  # "Caution. Interpretation of land-type specific emissions for soilc is
  # tricky because soil carbon is moved between land types in case of land-use
  # change, For instance, in case of forest-to-cropland conversion the
  # remaining fraction of soil carbon is moved from forest to cropland,
  # which will result in very high soilc emissions from forest and very high
  # negative soilc emissions from cropland"
  if (sum_land) {
    a <- dimSums(a, dim = c("land"))
  }
  
  # unit conversion
  if (unit == "gas") {
    a <- a * 44 / 12
  } # from Mt C/yr to Mt CO2/yr
  
  # years
  years <- getYears(a, as.integer = TRUE)
  yr_hist <- years[years > 1995 & years <= 2020]
  yr_fut <- years[years >= 2020]
  
  # apply lowpass filter (not applied on 1st time step,
  # applied seperatly on historic and future period)
  if (!is.null(lowpass)) a <- mbind(a[, 1995, ],
    lowpass(a[, yr_hist, ],
      i = lowpass),
    lowpass(a[, yr_fut, ],
      i = lowpass)[, -1, ])
  
  if (cumulative) {
    im_years <- new.magpie("GLO", years, NULL)
    im_years[, , ] <- c(1, diff(years))
    a[, "y1995", ] <- 0
    a <- a * im_years[, getYears(a), ]
    a <- as.magpie(apply(a, c(1, 3), cumsum))
    a <- a - setYears(a[, baseyear, ], NULL)
  }
  
  # aggregate over regions
  if (level != "cell") a <- superAggregate(a, aggr_type = "sum",
    level = level, na.rm = FALSE)
  
  out(a, file)
}
