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
  fore_red <- readGDX(gdx,"ov32_land_reduction",select = list(type="level"),react = "silent")
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
    #ac_sub <- readGDX(gdx,"ac_sub")
    
    ### Production of wood and woodfuel (tDM)
    # pm_carbon_density_ac_forestry <- collapseNames(readGDX(gdx,"pm_carbon_density_ac_forestry")[,,"vegc"])
    # pm_carbon_density_ac          <- collapseNames(readGDX(gdx,"pm_carbon_density_ac")[,,"vegc"])
    # vm_forestry_reduction         <- collapseNames(readGDX(gdx,"ov32_land_reduction")[,,"plant"][,,"level"])
    # vm_secdforest_reduction       <- collapseNames(readGDX(gdx,"ov35_secdforest_reduction")[,,"level"])
    # vm_primforest_reduction       <- collapseNames(readGDX(gdx,"ov35_primforest_reduction")[,,"level"])
    # vm_other_reduction            <- collapseNames(readGDX(gdx,"ov35_other_reduction")[,,"level"])
    # 
    # prod_forestry   <- vm_forestry_reduction[,,ac_sub]   * pm_carbon_density_ac_forestry[,getYears(timePeriods(gdx)),ac_sub]
    # prod_secdforest <- vm_secdforest_reduction[,,ac_sub] * pm_carbon_density_ac[,getYears(timePeriods(gdx)),ac_sub]
    # prod_primforest <- vm_primforest_reduction           * pm_carbon_density_ac[,getYears(timePeriods(gdx)),"acx"]
    # prod_other      <- vm_other_reduction[,,ac_sub]      * pm_carbon_density_ac[,getYears(timePeriods(gdx)),ac_sub]
    
    inflow   <- collapseNames(readGDX(gdx,"ov_prod")[,,kforestry][,,"level"]) ## mio. tDM
    inflow   <- inflow/readGDX(gdx,"f73_volumetric_conversion") ## Convert to Volume -- mio.tDM to mio.m3
    ## Conversion factor 230kgC/m3 -- Table S2 in SI of https://doi.org/10.1088/1748-9326/7/3/034023
    ## Unit below = million KgC
    inflow   <- inflow*230   
    inflow   <- inflow/1e3   ## Conversion to million ton C

    
    ## Inflow of the carbon stock calculation is based on www.pnas.org/cgi/doi/10.1073/pnas.1904231116 equation 3
    ## Inflow of carbon is calculated and not tDM because IPCC describes inflow in Gg C yr-1 from eq. 2.8.5
    ## Only locally produced stuff has to be accounted (inflow is already locally produced)
    
    calc_yrs <- getYears(inflow)
    
    ## k= decay constant of FOD for each HWP category (HWPj) 
    ## given in units yr-1(k= ln(2)/HL, where HL is half-life of the HWP pool in years (see Section 2.8.3.2).
    ## Assumed 35 years here
    
    k <- round(log(2)/(half_life),4)
    
    decay_alpha <- exp(-k)
    limiter_beta <- ((1-decay_alpha)/k)
    
    stock   <- new.magpie(cells_and_regions = getCells(inflow),years = getYears(inflow),fill = 0)
    
    ## Old Stock
    previous_stock <- new.magpie(cells_and_regions = "GLO",years = c("y1985","y1990"),fill = c(5450,5750)) # in MtC SI https://doi.org/10.1088/1748-9326/7/3/034023
    previous_stock <- previous_stock*(setYears(inflow[,1,]/dimSums(inflow[,1,],dim=1),NULL))
    dimSums(previous_stock,dim=1)

    ## Old Inflow
    previous_inflow <- new.magpie(cells_and_regions = "GLO",years = c("y1985","y1990"),fill = c(290,310)) # in MtC SI https://doi.org/10.1088/1748-9326/7/3/034023
    previous_inflow <- previous_inflow*(setYears(inflow[,1,]/dimSums(inflow[,1,],dim=1),NULL))
    
    ## 1995 Calculations
    stock[,"y1995",]    = (decay_alpha * setYears(previous_stock[,"y1990","wood"],NULL)) + (limiter_beta * setYears(previous_inflow[,"y1990","wood"],NULL))

    ## Inflow has to be in MtC/yr and Stock in MtC
    for (i in 1:(length(calc_yrs)-1)) {
      stock[,i+1,]    = (decay_alpha * stock[,i,]) + (limiter_beta * inflow[,i,"wood"])
    }

    temp <- NULL
    for(y in 1:(length(getYears(stock))-1)){
      ## Diff is carbon in HWP - Description of EQUATION 2.8.5 in 2013 Revised Supplementary Methods and Good Practice Guidance Arising from the Kyoto Protocol
      diff <- setYears(stock[,y+1,],NULL)-stock[,y,]
      diff <- diff/timestep_length[,y,] 
      if(timestep_length[,y,]>5){
        diff <- diff*timestep_length[,y,]/5
      }
      temp <- mbind(temp,diff)
    }

    missing_yr <- setdiff(getYears(stock),getYears(temp))
    temp <- mbind(temp,setYears(temp[,tail(getYears(temp),1),]*1.05,missing_yr))
    dimSums(temp,dim=1)
    
    annual_cHWP <- temp
    
    ## Find out how much is lost every year (based on half life)
    ## The annual net carbon sink in long-lived  products is derived as the difference between the 
    ## C stocks of two subsequent years.
    
    emis_cHWP <- NULL
    
    for (i in 1:length(calc_yrs)) {
      temp <- inflow[,i,"wood"] - inflow[,i,"wood"] * decay_alpha
      emis_cHWP <- mbind(emis_cHWP,temp)
    }

    a <- mbind(setNames(annual_cHWP,"storage"),setNames(emis_cHWP,"decay"))
      
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
    
    ### Conversion based on unit
    if(unit=="gas") a <- a * 44/12
    } else { 
      a <- NULL
      message("Disabled for magpie run without dynamic forestry.")
    }
  
  out(a,file)
}