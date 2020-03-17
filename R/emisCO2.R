#' @title emisCO2
#' @description reads detailed CO2 emissions out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param unit "element" or "gas"; "element": co2_c in Mt C/yr, n2o_n in Mt N/yr, ch4 in Mt CH4/yr; "gas": co2_c Mt CO2/yr, n2o_n in Mt NO2/yr, ch4 in Mt CH4/yr
#' @param pools_aggr aggregate carbon pools (TRUE), below ground (soilc) and above ground (vegc and litc) will be reported, if FALSE
#' @param cumulative Logical; Determines if emissions are reported annually (FALSE) or cumulative (TRUE). The starting point for cumulative emissions is y1995.
#' @param baseyear Baseyear used for cumulative emissions (default = 1995)
#' @param lowpass number of lowpass filter iterations (default = 3)
#' @param type net emissions (net), positive emissions only (pos) or negative emissions only (neg). Default is "net", which is the sum of positive and negative emissions
#' @param wood_prod_fraction Fraction of carbon stored on wood products excluding wood fuel
#' @param sum TRUE (default) or FALSE. Sum over land types and carbon pools (TRUE) or report land-type and carbon-pool specific emissions (FALSE). For sum=FALSE correct=TRUE should be used.
#' @param correct TRUE or FALSE (default). Correct accounting error in land-type specific emissions. TRUE requires a land transition matrix. Experimental, use with caution. 
#' @param ... further arguments passed to carbonstock function (defaults: cc=TRUE, cc_year=1995, regrowth=TRUE).
#' @return CO2 emissions as MAgPIE object (unit depends on \code{unit})
#' @author Florian Humpenoeder
#' @importFrom magclass new.magpie getCells lowpass
#' @examples
#' 
#'   \dontrun{
#'     x <- emisCO2(gdx)
#'   }
#' 

emisCO2 <- function(gdx, file=NULL, level="cell", unit="element", pools_aggr=TRUE, cumulative=FALSE, baseyear=1995, lowpass=3, type="net", sum=TRUE, correct=FALSE, ...){
  
  #get carbon stocks
  stock <- carbonstock(gdx, level="cell", sum_cpool = FALSE, sum_land = FALSE, ...)
  
  timestep_length <- readGDX(gdx,"im_years",react="silent")
  if(is.null(timestep_length)) timestep_length <- timePeriods(gdx)
  
  #calc emissions
  a <- new.magpie(getCells(stock),getYears(stock),getNames(stock),NA)
  for (t in 2:length(timestep_length)) {
    a[,t,] <- (setYears(stock[,t-1,],NULL) - stock[,t,])/timestep_length[t]
  }
  
  ###correction for land-type specific emissions
  #problem: too high forest-related emissions (primforest/secdforest) if forest is converted to cropland/pasture.
  #Why: The forest-related carbon stock is fully removed, while we see high carbon stock gains in cropland/pasture (but lower than the carbon removed in forest)
  #The emissions associated with the conversion from forest to cropland/pasture is just the difference between the losses in forest and the gains in cropland/pasture
  #The following code reallocates these emissions with the help of a land transition matrix (if available).
  
  #read in land use transitions
  lu_trans <- readGDX(gdx,"ov10_lu_transitions",select=list(type="level"),react = "silent")
  
  if(sum) {
#    a[,,"forestry"] <- 0
    if(pools_aggr) a <- dimSums(a,dim=3) 
    else {
                   a <- mbind(setNames(dimSums(a[,,"soilc"], dim=3),"Below Ground Carbon"),
                              setNames(dimSums(a[,,c("vegc","litc")], dim=3),"Above Ground Carbon"))
    } 
    
  } else if(correct & !is.null(lu_trans)) {
    #add shifting from other land to secdforest happening between the time steps to lu transition matrix
    other_to_secdforest <- readGDX(gdx,"p35_recovered_forest")
    lu_trans[,,"other.secdforest"] <- dimSums(other_to_secdforest,dim=3)
    
    #carbon density of target land-use (land_to10). Assumed carbon density if any land type is converted to cropland, pasture ...
    #cropland, pasture, urban is simple
    c_dens <- readGDX(gdx,"fm_carbon_density")[,getYears(stock),]
    #forestry and other land is different
    ac_start <- readGDX(gdx,"pc52_carbon_density_start")
    c_dens[,,c("forestry","other")] <- ac_start
    #secdforest is more complicated du to the shifting from other land to secdforest at a threshold of 20 tC/ha vegc. The 1st ac matching this threshold is different in each cell and time step.
    #read-in pm_carbon_density_ac
    c_dens_ac <- readGDX(gdx,"pm_carbon_density_ac")
    #convert to 4d array
    dim1 <- getCells(c_dens_ac)
    dim2 <- getYears(c_dens_ac)
    dim3 <- getNames(c_dens_ac,dim=1)
    dim4 <- getNames(c_dens_ac,dim=2)
    c_dens_ac<-array(c_dens_ac,dim=c(length(dim1),length(dim2),length(dim3),length(dim4)),dimnames = list(dim1,dim2,dim3,dim4))
    #set the 3 c_pools (vegc, litc, soilc) of all age-classes below threshold of 20 tC/ha in vegc to NA
    c_dens_ac[c_dens_ac[,,,"vegc"]<=20] <- NA
    #select the minimum carbon density for each j, t and c_pool
    c_dens_ac <- suppressWarnings(apply(c_dens_ac,c(1,2,4),min,na.rm=TRUE))
    #set Inf to zero
    c_dens_ac[is.infinite(c_dens_ac)] <- 0
    c_dens_ac <- as.magpie(c_dens_ac)
    names(dimnames(c_dens_ac)) <- names(dimnames(ac_start))
    c_dens[,,"secdforest"] <- c_dens_ac
    
    #Account for cc TRUE and FALSE
    if(!exists("cc")) cc <- TRUE
    if(!exists("cc_year")) cc_year <- 1995
    if(!cc) c_dens[,,] <- setYears(c_dens[,cc_year,],NULL)
    
    #correct emissions based on lu transitions (shift)
    for(land_from in getNames(lu_trans,dim=1)) {
      for(land_to in getNames(lu_trans,dim=2)) {
        #emissions from land remaining land (e.g. forest remaining forest) is covered by carbon stock calc already.
        if(land_from != land_to) {
          #Multiply lu transition with target carbon density
          shift <- collapseNames(lu_trans[,,paste(land_from,land_to,sep=".")])*collapseNames(c_dens[,,land_to])/timestep_length
          #add these emissins to target (e.g. cropland)
          a[,,land_to] <- a[,,land_to] + shift
          #remove these emissions from source (e.g. forest)
          a[,,land_from] <- a[,,land_from] - shift
        }
      }
    }
  } else if (correct & is.null(lu_trans)) {
    stop("For reporting land-type specific emissions a correction based on a land transition matrix (lu_trans) is needed but lu_trans is not available from your gdx file")
  } else if (!correct) {
    stop("For reporting land-type specific emissions a correction based on a land transition matrix (lu_trans) is needed. Re-run with correct=TRUE")
  }
  
  #unit conversion
  if (unit == "gas") a <- a*44/12 #from Mt C/yr to Mt CO2/yr
  
  if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
    emis_wood_products <- carbonHWP(gdx,unit = unit)/timestep_length[t]
#    a <- a - collapseNames(emis_wood_products[,,"wood"])
    a <- a - collapseNames(emis_wood_products[,,"ind_rw_pool"]) + collapseNames(emis_wood_products[,,"slow_release_pool"])
  }
  

  #years
  years <- getYears(a,as.integer = T)
  yr_hist <- years[years > 1995 & years <= 2020]
  yr_fut <- years[years >= 2020]

  #apply lowpass filter (not applied on 1st time step, applied seperatly on historic and future period)
  if(!is.null(lowpass)) a <- mbind(a[,1995,],lowpass(a[,yr_hist,],i=lowpass),lowpass(a[,yr_fut,],i=lowpass)[,-1,])
  
  #net, pos or negative
  if (type == "net") {
    a <- a
  } else if (type == "pos") {
    a[a < 0] = 0
  } else if (type == "neg") {
    a[a > 0] = 0
  }
  
  if (cumulative) {
    im_years <- new.magpie("GLO",years,NULL)
    im_years[,,] <- c(1,diff(years))
    a[,"y1995",] <- 0
    a <- a*im_years[,getYears(a),]
    a <- as.magpie(apply(a,c(1,3),cumsum))
    a <- a - setYears(a[,baseyear,],NULL)
  }
  
  #aggregate over regions
  if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)

  out(a,file)
}
