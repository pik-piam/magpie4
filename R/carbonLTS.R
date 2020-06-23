#' @title carbonLTS
#' @description reads carbon stored in harvested timber out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param unit element" or "gas"; "element": co2_c in Mt C/yr, n2o_n in Mt N/yr, ch4 in Mt CH4/yr; "gas": co2_c Mt CO2/yr, n2o_n in Mt NO2/yr, ch4 in Mt CH4/yr
#' @param half_life Half life in years for decay in wood products loosing half theor carbon content. (35 yrs is deafault)
#' @details Annual (and cumulative) Carbon stored in harvested wood products as well as slow emissions from half life deacy.
#' @param cumulative Logical; Determines if cHWP emissions are reported annually (FALSE) or cumulative (TRUE). The starting point for cumulative emissions is y1995.
#' @param baseyear Baseyear used for cumulative emissions (default = 1995)
#' @return carbon stocks in MtC from harvested timber
#' @author Abhijeet Mishra, Florian Humpenoeder
#' @importFrom gdx readGDX out
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @importFrom luscale superAggregate
#' @importFrom utils tail
#' @examples
#' 
#'   \dontrun{
#'     x <- carbonLTS(gdx)
#'   }

carbonLTS <- function(gdx, file=NULL, level="cell",unit="element", half_life=35, cumulative=FALSE, baseyear=1995){
  
  timber <- FALSE
  fore_red <- readGDX(gdx,"ov_forestry_reduction",select = list(type="level"),react = "silent")
  if (!is.null(fore_red)) {
    if (max(fore_red) > 0) {
      timber <- TRUE
    }
  }
  
  if (timber){
    
    kforestry <- readGDX(gdx,"kforestry")
    
      ### Convert from annual values to total values
      timestep_length <- readGDX(gdx,"im_years",react="silent")
      if(is.null(timestep_length)) timestep_length <- timePeriods(gdx)
      timestep_length[,1,] <- 5
      
      ## AgeClass
      ac_sub <- readGDX(gdx,"ac_sub")
      
      ### Production of wood and woodfuel (tDM)
      # pm_carbon_density_ac_forestry <- collapseNames(readGDX(gdx,"pm_carbon_density_ac_forestry")[,,"vegc"])
      # pm_carbon_density_ac          <- collapseNames(readGDX(gdx,"pm_carbon_density_ac")[,,"vegc"])
      # vm_forestry_reduction         <- collapseNames(readGDX(gdx,"ov_forestry_reduction")[,,"plant"][,,"level"])
      # vm_secdforest_reduction       <- collapseNames(readGDX(gdx,"ov_secdforest_reduction")[,,"level"])
      # vm_primforest_reduction       <- collapseNames(readGDX(gdx,"ov_primforest_reduction")[,,"level"])
      # vm_other_reduction            <- collapseNames(readGDX(gdx,"ov_other_reduction")[,,"level"])
      # 
      # prod_forestry   <- vm_forestry_reduction[,,ac_sub]   * pm_carbon_density_ac_forestry[,getYears(timePeriods(gdx)),ac_sub]
      # prod_secdforest <- vm_secdforest_reduction[,,ac_sub] * pm_carbon_density_ac[,getYears(timePeriods(gdx)),ac_sub]
      # prod_primforest <- vm_primforest_reduction           * pm_carbon_density_ac[,getYears(timePeriods(gdx)),"acx"]
      # prod_other      <- vm_other_reduction[,,ac_sub]      * pm_carbon_density_ac[,getYears(timePeriods(gdx)),ac_sub]
      
      prod <- collapseNames(readGDX(gdx,"ov_prod")[,,kforestry][,,"level"]) ## tDM
      f73_volumetric_conversion <- readGDX(gdx,"f73_volumetric_conversion")
      #prod <- dimSums(prod_forestry,dim=3)+dimSums(prod_secdforest,dim=3)+dimSums(prod_primforest,dim=3)+dimSums(prod_other,dim=3)
      
      prod   <- prod/f73_volumetric_conversion ## Convert to Volume -- mio.tDM to mio.m3
      
      prod <- prod*230/10e3 ## Convert to KgC -- 230kg/m3 -- Table S2 in SI of https://doi.org/10.1088/1748-9326/7/3/034023
      ## Multiplication with 230 gives mio.kgC Division with 10e3 gives mio.tC
      
      
      
      #prod <- prod / timestep_length
      
      ### Conversion to MtCO2 as prod_c is in mio.tC
      if(unit=="gas") prod <- prod * 44/12
      
      ## Inflow of the carbon stock calculation is based on www.pnas.org/cgi/doi/10.1073/pnas.1904231116 equation 3
      ## Inflow of carbon is calculated and not tDM because IPCC describes inflow in Gg C yr-1 from eq. 2.8.5
      ## Only locally produced stuff has to be accounted
      domestically_produced_cHWP = prod * readGDX(gdx,"f21_self_suff")[,getYears(prod),kforestry]
      
      wood <- domestically_produced_cHWP[,,"wood"] ## MtC in wood
      woodfuel <- domestically_produced_cHWP[,,"woodfuel"] ## MtC in woodfuel
      
      ## We know carbon stored in harvested wood products for now. We just need to redistribute the slow release over time
      # We will redistribute these emissions according to half life of wood products
      
      ## Initialize values
      new_stock <- NULL
      remaining_stock <- NULL
      annual_release <- NULL
      
      calc_yrs <- getYears(domestically_produced_cHWP)
      
      ## k= decay constant of FOD for each HWP category (HWPj) 
      ## given in units yr-1(k= ln(2)/HL, where HL is half-life of the HWP pool in years (see Section 2.8.3.2).
      ## Assumed 35 years here
      
      k <- round(log(2)/half_life,4)
      
      annual_decay_factors <- timestep_length * k
      
      stored_now  <- domestically_produced_cHWP
      stored_next <- domestically_produced_cHWP
      
      ## Calculation for 1995
      factor_k <- annual_decay_factors[,1,]
      inflow_reducer = (1-exp(-factor_k))/factor_k
      stored_now[,1,"wood"] = (domestically_produced_cHWP[,1,"wood"]*0.9) * exp(-factor_k) + (inflow_reducer * domestically_produced_cHWP[,1,"wood"])
      
      for (i in 1:(length(calc_yrs)-1)) {
        factor_k                   = annual_decay_factors[,i,]
        inflow_reducer             = (1-exp(-factor_k))/factor_k
        stored_next[,i+1,"wood"]   = stored_now[,i,"wood"] * exp(-factor_k) + (inflow_reducer * domestically_produced_cHWP[,i,"wood"])
        stored_now[,i,"wood"]      = stored_next[,i+1,"wood"]
      }
      
      annual_cHWP <- stored_now
      dimSums(annual_cHWP,dim=1)[,,1]
      
      ### Find out how much is lost every year (based on half life)
      
      emis_cHWP <- NULL
      
      for (i in 1:length(calc_yrs)) {
        temp <- domestically_produced_cHWP[,i,"wood"] - domestically_produced_cHWP[,i,"wood"] * annual_decay_factors[,i,]
        emis_cHWP <- mbind(emis_cHWP,temp)
      }
      dimSums(emis_cHWP,dim=1)[,,1]
      
      a         <- add_dimension(annual_cHWP,dim = 3.1,nm = "storage",add = "type")
      emis_cHWP <- add_columns(emis_cHWP,dim = 3.1,addnm = "woodfuel")
      emis_cHWP <- add_dimension(emis_cHWP,dim = 3.1,nm = "decay",add = "type")
      
      a <- mbind(a,emis_cHWP)
      a[,,"storage.woodfuel"] <- 0
      a[,,"decay.woodfuel"] <- 0
      
      if (cumulative) {
        years <- getYears(a,as.integer = T)
        im_years <- new.magpie("GLO",years,NULL)
        im_years[,,] <- c(1,diff(years))
        a[,"y1995",] <- 0
        a <- a*im_years[,getYears(a),]
        a <- as.magpie(apply(a,c(1,3),cumsum))
        a <- a - setYears(a[,baseyear,],NULL)
      }
      
      if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)

    } else { 
      a <- NULL
      message("Disabled for magpie run without dynamic forestry.")
    }
  
  out(a,file)
}