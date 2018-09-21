#' @title carbonstock
#' @description reads carbon stocks out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param sum_cpool sum over carbon pool dimension (default = TRUE)
#' @param sum_land sum over land type dimension (default = TRUE)
#' @param cc account for climate change impacts on carbon stocks (default = TRUE). FALSE reflects only carbon stock changes due to land management.
#' @param cc_year year for fixing carbon density if cc=FALSE (default = 1995)
#' @details carbon pools consist of vegetation carbon (vegc), litter carbon (litc) and soil carbon (soilc)
#' @return carbon stocks in MtC
#' @author Florian Humpenoeder
#' @importFrom gdx readGDX out
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @importFrom luscale superAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- carbonstock(gdx)
#'   }
#' 

carbonstock <- function(gdx, file=NULL, level="cell", sum_cpool=TRUE, sum_land=TRUE, cc=TRUE, cc_year=1995){
  
  #read in carbon stocks
  a <- readGDX(gdx,"ov_carbon_stock",select=list(type="level"),react="silent")
  names(dimnames(a))[1] <- "j"
  
  #check
  sm_cc_carbon <- readGDX(gdx,"sm_cc_carbon2",react = "silent")
  if(!is.null(sm_cc_carbon)) {
    if(sm_cc_carbon == 0) stop("MAgPIE runs with sm_cc_carbon = 0 are not supported!")
  } 
  
  #recalculate carbon stocks without CC impacts if cc=FALSE
  #if the MAgPIE run was performed with static input cc=TRUE/FALSE should return identical results
  if(!cc) {
    weighted_mean <- function(x,weight,map) {
      #carbon density * area
      a <- clean_magpie(x[map]) * clean_magpie(weight[map])
      #aggregate age-classes
      a <- dimSums(a,dim=c(3.1))
      weight <- dimSums(clean_magpie(weight[map]),dim=c(3.1))
      #calculate new carbon density
      a <- a/weight
      a[is.na(a)] <- 0
      return(a)
    }
    
    #use same structure
    b <- a
    b[,,] <- 0
    
    #read in needed parameters and variables
    t <- readGDX(gdx,"t")
    
    ov_land <- readGDX(gdx,"ov_land",select = list(type="level"))
    names(dimnames(ov_land))[1] <- "j"
    
    #read in carbon density and fix on cc_year
    fm_carbon_density <- readGDX(gdx,"fm_carbon_density")[,t,]
    names(dimnames(fm_carbon_density))[2] <- "t"
    fm_carbon_density[,,] <- setYears(fm_carbon_density[,cc_year,],NULL)
    
    #read in ac-specifc carbon density and fix on cc_year
    pm_carbon_density_ac <- readGDX(gdx,"pm_carbon_density_ac")
    pm_carbon_density_ac[,,] <- setYears(pm_carbon_density_ac[,cc_year,],NULL)
    
    #cropland, pasture, urban land and primforest is simple
    b[,,"crop"] <- fm_carbon_density[,,"crop"]*ov_land[,,"crop"]
    b[,,"past"] <- fm_carbon_density[,,"past"]*ov_land[,,"past"]
    b[,,"urban"] <- fm_carbon_density[,,"urban"]*ov_land[,,"urban"]
    b[,,"primforest"] <- fm_carbon_density[,,"primforest"]*ov_land[,,"primforest"]
    
    #forestry land
    p32_land <- readGDX(gdx,"p32_land","p32_land_fore",react = "quiet")
    if(is.null(p32_land)) {
      b[,,"forestry"] <- fm_carbon_density[,,"forestry"]*ov_land[,,"forestry"]
    } else { 
      if(suppressWarnings(!is.null(readGDX(gdx,"fcosts32H")))){
        p32_land <- readGDX(gdx,"p32_land","p32_land_fore",react = "quiet")
        if(is.null(p32_land)) {
          b[,,"forestry"] <- fm_carbon_density[,,"forestry"]*ov_land[,,"forestry"]
        } else {
          ov_land_forestry <- readGDX(gdx,"ov_land_forestry","ov32_land",select = list(type="level"))
          # p32_land <- collapseNames(p32_land[,,"before"])
          names(dimnames(p32_land))[1] <- "j"
          # ac_land32 <- readGDX(gdx,"ac_land32")
          p32_carbon_density <- readGDX(gdx,"pm_carbon_density_ac") * readGDX(gdx,"p32_forestry_management")
          b[,,"forestry"] <- dimSums(p32_carbon_density*ov_land_forestry,dim=c(3.1,3.3))
        }
      } else {
      ov_land_forestry <- readGDX(gdx,"ov_land_forestry","ov32_land",select = list(type="level"))
      p32_land <- collapseNames(p32_land[,,"before"])
      names(dimnames(p32_land))[1] <- "j"
      ac_land32 <- readGDX(gdx,"ac_land32")
      p32_carbon_density <- readGDX(gdx,"p32_carbon_density")
      p32_carbon_density <- setNames(p32_carbon_density,gsub("indc","ndc",getNames(p32_carbon_density)))
      status32 <- readGDX(gdx,"status32",react = "quiet")
      if(is.null(status32)) {
        p32_carbon_density[,,"new"] <- collapseNames(pm_carbon_density_ac[,,"ac0"])
        p32_carbon_density[,,"new_ndc"] <- collapseNames(pm_carbon_density_ac[,,"ac0"])
        p32_carbon_density[,,"prot"] <- collapseNames(weighted_mean(pm_carbon_density_ac,p32_land,ac_land32)[,,"prot"])
        p32_carbon_density[,,"grow"] <- collapseNames(weighted_mean(pm_carbon_density_ac,p32_land,ac_land32)[,,"grow"])
        p32_carbon_density[,,"old"] <- collapseNames(pm_carbon_density_ac[,,"acx"])
        b[,,"forestry"] <- dimSums(p32_carbon_density*ov_land_forestry,dim=c(3.1))
      } else {
        p32_carbon_density[,,"new"] <- collapseNames(pm_carbon_density_ac[,,"ac0"])
        p32_carbon_density[,,"aff.prot"] <- collapseNames(weighted_mean(pm_carbon_density_ac,p32_land,ac_land32)[,,"aff.prot"])
        p32_carbon_density[,,"ndc.prot"] <- collapseNames(weighted_mean(pm_carbon_density_ac,p32_land,ac_land32)[,,"indc.prot"])
        p32_carbon_density[,,"plant.prot"] <- 0 #temporary fix until forestry is implemented
        p32_carbon_density[,,"avail"] <- collapseNames(weighted_mean(pm_carbon_density_ac,p32_land,ac_land32)[,,"avail"])
        b[,,"forestry"] <- dimSums(p32_carbon_density*ov_land_forestry,dim=c(3.1,3.2))
      }
    }
  }
    
    #secdforest
    p35_secdforest <- readGDX(gdx,"p35_secdforest",react = "quiet")
    if(is.null(p35_secdforest)) {
      b[,,"secdforest"] <- fm_carbon_density[,,"secdforest"]*ov_land[,,"secdforest"]
    } else {
      if(suppressWarnings(!is.null(readGDX(gdx,"fcosts32H")))){
        p35_secdforest <- readGDX(gdx,"p35_secdforest",react = "quiet")
        if(is.null(p35_secdforest)) {
          b[,,"secdforest"] <- fm_carbon_density[,,"secdforest"]*ov_land[,,"secdforest"]
        } else {
          ov_land_secdforest <- readGDX(gdx,"ov_land_secdforest","ov35_secdforest",select = list(type="level"))
          # p35_secdforest <- collapseNames(p35_secdforest[,,"before"])
          names(dimnames(p35_secdforest))[1] <- "j"
          # ac_land35 <- readGDX(gdx,"ac_land35")
          p35_carbon_density_secdforest <- readGDX(gdx,"pm_carbon_density_ac")
          b[,,"secdforest"] <- dimSums(p35_carbon_density_secdforest*ov_land_secdforest,dim=3.1)
        }
      } else {
        ov_land_secdforest <- readGDX(gdx,"ov_land_secdforest","ov35_secdforest",select = list(type="level"))
        p35_secdforest <- collapseNames(p35_secdforest[,,"before"])
        names(dimnames(p35_secdforest))[1] <- "j"
        ac_land35 <- readGDX(gdx,"ac_land35")
        land35 <- readGDX(gdx,"land35",react = "quiet")
        if(length(land35) == 3) {
          p35_carbon_density_secdforest <- readGDX(gdx,"p35_carbon_density_secdforest")
          p35_carbon_density_secdforest[,,"new"] <- collapseNames(pm_carbon_density_ac[,,"ac0"])
          p35_carbon_density_secdforest[,,"grow"] <- collapseNames(weighted_mean(pm_carbon_density_ac,p35_secdforest,ac_land35)[,,"grow"])
          p35_carbon_density_secdforest[,,"old"] <- collapseNames(pm_carbon_density_ac[,,"acx"])
        } else {
          p35_carbon_density_secdforest <- readGDX(gdx,"p35_carbon_density_secdforest")
          p35_carbon_density_secdforest[,,"new"] <- collapseNames(pm_carbon_density_ac[,,"ac0"])
          p35_carbon_density_secdforest[,,"young"] <- collapseNames(weighted_mean(pm_carbon_density_ac,p35_secdforest,ac_land35)[,,"young"])
          p35_carbon_density_secdforest[,,"mid"] <- collapseNames(weighted_mean(pm_carbon_density_ac,p35_secdforest,ac_land35)[,,"mid"])
          p35_carbon_density_secdforest[,,"old"] <- collapseNames(weighted_mean(pm_carbon_density_ac,p35_secdforest,ac_land35)[,,"old"])
        }
        b[,,"secdforest"] <- dimSums(p35_carbon_density_secdforest*ov_land_secdforest,dim=3.1)
      }
    }
    
    #other land
    p35_other <- readGDX(gdx,"p35_other",react = "quiet")
    if(is.null(p35_other)) {
      b[,,"other"] <- fm_carbon_density[,,"other"]*ov_land[,,"other"]
    } else {
      if(suppressWarnings(!is.null(readGDX(gdx,"fcosts32H")))){
        p35_other <- readGDX(gdx,"p35_other",react = "quiet")
        if(is.null(p35_other)) {
          b[,,"other"] <- fm_carbon_density[,,"other"]*ov_land[,,"other"]
        } else {
          ov_land_other <- readGDX(gdx,"ov_land_other","ov35_other",select = list(type="level"))
          # p35_other <- collapseNames(p35_other[,,"before"])
          names(dimnames(p35_other))[1] <- "j"
          # ac_land35 <- readGDX(gdx,"ac_land35")
          p35_carbon_density_other <- readGDX(gdx,"pm_carbon_density_ac")
          b[,,"other"]<- dimSums(p35_carbon_density_other*ov_land_other,dim=3.1)
        }
      }
      else{
        ov_land_other <- readGDX(gdx,"ov_land_other","ov35_other",select = list(type="level"))
        p35_other <- collapseNames(p35_other[,,"before"])
        names(dimnames(p35_other))[1] <- "j"
        ac_land35 <- readGDX(gdx,"ac_land35")
        p35_carbon_density_other <- readGDX(gdx,"p35_carbon_density_other")
        land35 <- readGDX(gdx,"land35",react = "quiet")
        if(length(land35) == 3) {
          p35_carbon_density_other[,,"new"] <- collapseNames(pm_carbon_density_ac[,,"ac0"])
          p35_carbon_density_other[,,"grow"] <- collapseNames(weighted_mean(pm_carbon_density_ac,p35_other,ac_land35)[,,"grow"])
          p35_carbon_density_other[,,"old"] <- collapseNames(pm_carbon_density_ac[,,"acx"])
        } else {
          p35_carbon_density_other[,,"new"] <- collapseNames(pm_carbon_density_ac[,,"ac0"])
          p35_carbon_density_other[,,"young"] <- collapseNames(weighted_mean(pm_carbon_density_ac,p35_other,ac_land35)[,,"young"])
          p35_carbon_density_other[,,"mid"] <- collapseNames(weighted_mean(pm_carbon_density_ac,p35_other,ac_land35)[,,"mid"])
          p35_carbon_density_other[,,"old"] <- collapseNames(weighted_mean(pm_carbon_density_ac,p35_other,ac_land35)[,,"old"])
        }
        b[,,"other"] <- dimSums(p35_carbon_density_other*ov_land_other,dim=3.1)
      }
    }
    
    #replace carbon stock
    a <- b
  }
  
  #rounding
  a <- round(a,digits = 3)
  
  #sum over land pools
  if (sum_land) a <- dimSums(a,dim="land")
  
  #sum over carbon pools
  if (sum_cpool) a <- dimSums(a,dim="c_pools")
  
  #aggregate over regions
  if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)
  
  out(a,file)
}
