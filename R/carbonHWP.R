#' @title carbonHWP
#' @description reads carbon stocks in harvested timber out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param unit element" or "gas"; "element": co2_c in Mt C/yr, n2o_n in Mt N/yr, ch4 in Mt CH4/yr; "gas": co2_c Mt CO2/yr, n2o_n in Mt NO2/yr, ch4 in Mt CH4/yr
#' @details Carbon stored in harvested wood products
#' @return carbon stocks in MtC from harvested timber
#' @author Abhijeet Mishra, Florian Humpenoeder
#' @importFrom gdx readGDX out
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @importFrom luscale superAggregate
#' @importFrom utils tail
#' @examples
#' 
#'   \dontrun{
#'     x <- carbonHWP(gdx)
#'   }

carbonHWP <- function(gdx, file=NULL, level="cell",unit="element"){
  
  ac_sub <- readGDX(gdx,"ac_sub")
  pm_growing_stock <- readGDX(gdx,"pm_growing_stock")
  
  ## Hvarea forestry
  ov_hvarea_forestry <- readGDX(gdx,"ov_hvarea_forestry",select = list(type="level"))
  

  ov_hvarea_secdforest <- readGDX(gdx,"ov_hvarea_secdforest",select = list(type="level"))
  
  ov_hvarea_primforest <- readGDX(gdx,"ov_hvarea_primforest",select = list(type="level"))
  
  ov_hvarea_other <- readGDX(gdx,"ov_hvarea_other",select = list(type="level"))
  

  #### Following calculations are mio. ha  * tDM/ha = mio. tDM
  #### 1 kg DM = 0.5 kg C
  hwp_forestry <- dimSums(ov_hvarea_forestry[,,ac_sub]*pm_growing_stock[,,ac_sub][,,"forestry"],dim=3.1)

  hwp_secdforest <- dimSums(ov_hvarea_secdforest[,,ac_sub]*pm_growing_stock[,,ac_sub][,,"secdforest"],dim=3.1)
  
  hwp_primforest <- dimSums(ov_hvarea_primforest*pm_growing_stock[,,"acx"][,,"primforest"],dim=3.1)

  hwp_other <- dimSums(ov_hvarea_other[,,ac_sub]*pm_growing_stock[,,ac_sub][,,"other"],dim=3.1)
  
  a <- mbind(hwp_forestry,hwp_secdforest,hwp_primforest,hwp_other)
  
  vm_prod <- collapseNames(readGDX(gdx,"ov_prod")[,,readGDX(gdx,"kforestry")][,,"level"])
  
  vm_prod_ratio <- dimSums(vm_prod,dim=1)/dimSums(vm_prod,dim=c(1,3))
  
  a <- dimSums(a,dim=3) * vm_prod_ratio
  
  ## Convert to C from DM
  
  a <- a * readGDX(gdx,"i14_carbon_fraction")
  
  ### IPCC method
  k <- log(2)/35
  ## k= decay constant of FOD for each HWP category (HWPj) 
  ## given in units yr-1(k= ln(2)/HL, where HL is half-life of the HWP pool in years (see Section 2.8.3.2).
  ## Assumed 35 years here
  
  wood <- a[,,"wood"]
  woodfuel <- a[,,"woodfuel"]
  
  ## Initialization based on eq. 2.8.6 from 2013 Revised Supplementary Methods and Good Practice Guidance Arising from the Kyoto Protocol
  ## https://www.ipcc-nggip.iges.or.jp/public/kpsg/pdf/KP_Supplement_Entire_Report.pdf
  product_decay_init <- setYears(dimSums(wood[,1:5,],dim=2)/5,"y1995")
  
  ## Carbon storage calculation based on eq. 2.8.5 from 2013 Revised Supplementary Methods and Good Practice Guidance Arising from the Kyoto Protocol
  ## https://www.ipcc-nggip.iges.or.jp/public/kpsg/pdf/KP_Supplement_Entire_Report.pdf
  
  for(i in 2:(length(getYears(wood)))){
    temp <- setYears((product_decay_init[,i-1,]/exp(k)) 
                     + ((1-(1/exp(k)))/k) 
                     * readGDX(gdx,"f21_self_suff")[,getYears(a)[i-1],"wood"] 
                     * vm_prod[,i-1,"wood"] * 0.5
                     ,NULL)
    getYears(temp) <- getYears(wood)[i]
    product_decay_init <- mbind(product_decay_init,temp)
  }
  ind_rw_pool <- product_decay_init
  getNames(ind_rw_pool) <- "ind_rw_pool"
  
  
  ############################### SLOW RELEASE
  # for (i in 1:50) {
  #   temp <- init-init*k
  #   init <- temp
  #   print(paste0(i,"--",init))
  #   }
  released_overall <- 0
  for(j in 1:(length(getYears(ind_rw_pool)))){
    released <- NULL
    ## Create dummy magpie object. We need to perate on pool of each year and release it slowly
    ## over the duration of model run
    ind_rw_pool_dummy <- ind_rw_pool[,1:(length(getYears(ind_rw_pool))),]
    for(i in 1:(length(getYears(ind_rw_pool_dummy))-1)){
      ## Certain portion of the pool selected above is lost each time step
      ## This is based on half life calculation from IPCC. 
      ## See explanation of k above 
      temp <- ind_rw_pool_dummy[,i,]-ind_rw_pool_dummy[,i,]*k
      released <- mbind(released,ind_rw_pool_dummy[,i,]*k)
      ## Update the pool by removing released parts. 
      ind_rw_pool_dummy[,i+1,] <- setYears(temp,NULL)
    }
    if(j==1){
    ## What part of HWP was lost this year  
    released_overall <- released_overall + released
    ## Re-adjust the HWP pool based on what is lost
    ind_rw_pool[,j,] <- ind_rw_pool[,j,] - released[,j,]
    } else {
      update <- getYears(released)
      ## What part of HWP was lost this year
      released_overall[,update,] <- released_overall[,update,] + released
      ## Re-adjust the HWP pool based on what is lost
      ind_rw_pool[,update,] <- ind_rw_pool[,update,] - released
      }
  }
  
  last_yr <- tail(getYears(released_overall),1)
  missing_yr <- setdiff(getYears(ind_rw_pool),getYears(released_overall))
  released_overall <- mbind(released_overall,setYears(released_overall[,last_yr,],missing_yr))
  getNames(released_overall) <- "slow_release_pool"
  
  a <- mbind(ind_rw_pool,released_overall,wood,woodfuel)

  
  if(unit=="gas") a <- a * 44 / 12
  
  if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)
  
  out(a,file)
}