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
#' @param regrowth TRUE (default) or FALSE. FALSE returns pure land-use change emissions. Works only in combination with CC=FALSE. 
#' @details carbon pools consist of vegetation carbon (vegc), litter carbon (litc) and soil carbon (soilc)
#' @return carbon stocks in MtC
#' @author Florian Humpenoeder
#' @importFrom gdx readGDX out
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @importFrom luscale superAggregate
#' @importFrom utils head
#' @examples
#' 
#'   \dontrun{
#'     x <- carbonstock(gdx)
#'   }
#' 

carbonstock <- function(gdx, file=NULL, level="cell", sum_cpool=TRUE, sum_land=TRUE, cc=TRUE, cc_year=1995, regrowth=TRUE){
  
  #read in carbon stocks
  a <- readGDX(gdx,"ov_carbon_stock",select=list(type="level"),react="silent")
  names(dimnames(a))[1] <- "j"
  
  #calculate detailed forestry land module carbon stock: aff, ndc, plant
  p32_land <- readGDX(gdx,"p32_land","p32_land_fore",react = "quiet")
  
  dyn_som <- !is.null(readGDX(gdx, "ov59_som_pool", react="silent"))
  
  timber <- FALSE
  fore_red <- readGDX(gdx,"ov_forestry_reduction",select = list(type="level"),react = "silent")
  if (!is.null(fore_red)) {
    if (max(fore_red) > 0) {
      timber <- TRUE
    }
  }
  
  if (timber) {
    # This logical statement is only valid for runs with timber demand turned on.
    # When timber demand is on, the mdoel has to meet certain demand with plantations.
    # Additionaly, plantations are added regularly to the timber plantations pool in ac0.
    # In plantation establishmnet, when time step length are more than 5, not just ac0 is newly established but ac5 as well (if the jump is 10 years).
    # This is done outside optimization but the redistribution of newly established ac0 is made into ac0 and ac5 equally. This should refelcet in p32_land
    # This means that for 10 year timestep jumps, ac0 and ac5 are established with ac0 carbon density.
    # We make this adjustment here. This will not impact any run where plantations are not added during the model run.
    
    timestep_length <- readGDX(gdx,"im_years",react="silent")
    if(is.null(timestep_length)) timestep_length <- timePeriods(gdx)
    
    for(i in getYears(timestep_length)){
      if(as.numeric(timestep_length[,i,])>5){
        ## Count how big the jump is
        jump <- as.numeric(timestep_length[,i,])/5
        ## See which age classes were additionally added along with ac0 in this jump
        ac_to_fix <- readGDX(gdx,"ac")[1:jump]
        ## Take the additiona age calsses added and add them to ac0
        p32_land[,i,"ac0"][,,"plant"] = p32_land[,i,"ac0"][,,"plant"] + dimSums(p32_land[,i,ac_to_fix[-1]][,,"plant"],dim=3)
        ## Reset these added additional age-classes to 0
        p32_land[,i,ac_to_fix[-1]][,,"plant"] <- 0
      }
    }
  }
  
  if(!is.null(p32_land)) {
    #expand p32_land for MAgPIE 4.0
    if(dim(p32_land)[3] == 122) p32_land <- collapseNames(p32_land[,,"after"])
    if(names(dimnames(p32_land))[[3]] == "ac") {
      p32_land_orig <- p32_land
      p32_land <- add_dimension(p32_land,dim=3.1,add = "type32",nm = c("aff","ndc","plant"))
      p32_land[,,"aff.acx"] <- 0
      p32_land[,,"ndc"] <- 0
      p32_land[,,"plant"][,,head(getNames(p32_land,dim=2),-1)] <- 0
      if(abs(sum(p32_land_orig-dimSums(p32_land,dim=3.1))) > 0.1) warning("Differences in p32_land detected")
      names(dimnames(p32_land))[1] <- "j"
    }
    p32_carbon_density_ac <- readGDX(gdx,"p32_carbon_density_ac",react = "silent")
    if(is.null(p32_carbon_density_ac)) p32_carbon_density_ac <- readGDX(gdx,"pm_carbon_density_ac")
    ov32_carbon_stock <- p32_land*p32_carbon_density_ac
    ov32_carbon_stock <- dimSums(ov32_carbon_stock,dim=3.2)
    if(!"soilc" %in% getNames(ov32_carbon_stock,dim=2)) {
      names(dimnames(ov32_carbon_stock))[[3]] <- "type32.c_pools"
      if(dyn_som) {
        cshare    <- collapseNames(cshare(gdx, level="cell", noncrop_aggr=FALSE, reference="actual")[,,"forestry"])
        cshare[is.na(cshare)]     <- 1
        top <- readGDX(gdx, "f59_topsoilc_density")[,getYears(cshare),]
        sub <- readGDX(gdx, "i59_subsoilc_density")[,getYears(cshare),]
        soilc <- dimSums(p32_land,dim=3.2) * (top * cshare + sub)
        soilc <- add_dimension(soilc,dim=3.2,add="c_pools",nm = "soilc")
      } else {
        soilc <- dimSums(p32_land,dim=3.2)*collapseNames(readGDX(gdx,"fm_carbon_density")[,getYears(p32_land),"forestry"])[,,"soilc"]
      }
      ov32_carbon_stock <- mbind(ov32_carbon_stock,soilc)
    }
    #check
    #print(dimSums(ov32_carbon_stock,dim=c(3.1,1))/dimSums(collapseNames(a[,,"forestry"]),dim=1))
    #print(dimSums(collapseNames(a[,,"forestry"]),dim=1))
    if(abs(sum(dimSums(ov32_carbon_stock,dim=3.1)-collapseNames(a[,,"forestry"]))) > 0.1){
      warning("Differences in ov32_carbon_stock detected!")
      diff_stock <- dimSums(ov32_carbon_stock,dim=3.1)/collapseNames(a[,,"forestry"])
      diff_stock[is.nan(diff_stock)] <- 1
      diff_stock[is.infinite(diff_stock)] <- 1
      diff_stock <- round(diff_stock,3)
      cat("\nDifferences exist in ",where(diff_stock>1)$true$regions, "in", unique((where(diff_stock>1)$true$individual)[,3]),"\n")
      }
    #integrate
    getNames(ov32_carbon_stock,dim=1) <- paste("forestry",getNames(ov32_carbon_stock,dim=1),sep="_")
    a <- a[,,"forestry",invert=TRUE]
    a <- mbind(a,ov32_carbon_stock)
  } else {
    #static forestry module realization
    forestry <- a[,,"forestry"]
    a <- a[,,"forestry",invert=TRUE]
    forestry_aff <- forestry; forestry_ndc <- forestry; forestry_plant <- forestry; 
    forestry_aff[,,] <- 0; forestry_ndc[,,] <- 0; 
    getNames(forestry_aff,dim=1) <- "forestry_aff"
    getNames(forestry_ndc,dim=1) <- "forestry_ndc"
    getNames(forestry_plant,dim=1) <- "forestry_plant"
    a <- mbind(a,forestry_aff,forestry_ndc,forestry_plant)
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
    
    degrowth <- function(x) {
      y <- x
      #undo age-class growth
      ac <- getNames(x,dim = "ac")
      year <- getYears(x,as.integer = T)
      for (t in 2:nyears(x)) {
        shifter <- (year[t]-year[1])/5
        #down-shifting of age-classes until acx-1
        for (i in (shifter+1):(length(ac)-1)) x[,t,ac[i-shifter]] <- x[,t,ac[i]]
        #acx is difficult because it is the highest age-class. Therefore, area is accumualting here, and its not possible to disentangle reduction due to land conversion and expansion due to shifting of age-classes exactly.
        #approximation: Use area from acx-shifter:acx (e.g. ac140, ac145, acx) from previous time step and scale with ratio of current acx and sum over acx-shifter:acx from previous time step
        ratio <- collapseNames(x[,t,ac[length(ac)]],collapsedim = "ac")/dimSums(setYears(x[,t-1,ac[(length(ac)-shifter):(length(ac))]],NULL),dim="ac")
        ratio[is.na(ratio)] <- 0
        x[,t,ac[(length(ac)-shifter):(length(ac))]] <- ratio*setYears(x[,t-1,ac[(length(ac)-shifter):(length(ac))]],NULL)
      }
      if(any(abs(dimSums(x,dim=3)-dimSums(y,dim=3)) > 1e6)) warning("Problem with regrowth=FALSE")
      return(x)
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
    som_on <- !is.element("soilc", getNames(pm_carbon_density_ac,dim=2))
    
    if(som_on){
      
      ag_pools <- c("litc", "vegc")
      
      #test dynamic vs. static
      if(dyn_som){
        
        pools59   <- readGDX(gdx, "pools59", types="sets", react="silent")
        pools59 <- pools59[-which(pools59=="forestry")]
        
        cshare    <- cshare(gdx, level="cell", noncrop_aggr=FALSE, reference="actual")[,,"total",invert=TRUE]
        cshare[is.na(cshare)]     <- 1
        
        top <- readGDX(gdx, "f59_topsoilc_density")[,getYears(cshare),]
        sub <- readGDX(gdx, "i59_subsoilc_density")[,getYears(cshare),]
        
        b[,,"crop"][,,ag_pools]         <- fm_carbon_density[,,"crop"][,,ag_pools]       * ov_land[,,"crop"]  
        b[,,"past"][,,ag_pools]         <- fm_carbon_density[,,"past"][,,ag_pools]       * ov_land[,,"past"]
        b[,,"urban"]                    <- fm_carbon_density[,,"urban"]                  * ov_land[,,"urban"]
        b[,,"primforest"][,,ag_pools]   <- fm_carbon_density[,,"primforest"][,,ag_pools] * ov_land[,,"primforest"]
        
        b[,,pools59][,,"soilc"]         <-  (top * cshare[,,pools59] + sub) * ov_land[,,pools59]  
        b[,,"forestry_aff"][,,"soilc"]  <- (top * collapseNames(cshare[,,"forestry"]) + sub) * dimSums(p32_land[,,"aff"],dim=3)
        b[,,"forestry_ndc"][,,"soilc"]  <- (top * collapseNames(cshare[,,"forestry"]) + sub) * dimSums(p32_land[,,"ndc"],dim=3)
        b[,,"forestry_plant"][,,"soilc"]  <- (top * collapseNames(cshare[,,"forestry"]) + sub) * dimSums(p32_land[,,"plant"],dim=3)
        
      } else { 
        
        i59_topsoilc_density     <- readGDX(gdx, "i59_topsoilc_density")[,t,]
        i59_topsoilc_density[,,] <- setYears(i59_topsoilc_density[,cc_year,], NULL)
        i59_subsoilc_density     <- readGDX(gdx, "i59_subsoilc_density")[,t,]
        i59_subsoilc_density[,,] <- setYears(i59_subsoilc_density[,cc_year,], NULL)
        
        #cropland, pasture, urban land and primforest is simple
        b[,,"crop"][,,ag_pools]         <- fm_carbon_density[,,"crop"][,,ag_pools] * ov_land[,,"crop"] 
        b[,,"crop"][,,"soilc"]          <- (i59_topsoilc_density + i59_subsoilc_density)   * ov_land[,,"crop"]
        b[,,"past"]                     <- fm_carbon_density[,,"past"]                     * ov_land[,,"past"]
        b[,,"urban"]                    <- fm_carbon_density[,,"urban"]                    * ov_land[,,"urban"]
        b[,,"primforest"]               <- fm_carbon_density[,,"primforest"]               * ov_land[,,"primforest"]
        b[,,"secdforest"][,,"soilc"]    <- fm_carbon_density[,,"secdforest"][,,"soilc"]    * ov_land[,,"secdforest"]
        b[,,"forestry_aff"][,,"soilc"]  <- collapseNames(fm_carbon_density[,,"forestry"])[,,"soilc"]      * dimSums(p32_land[,,"aff"],dim=3)
        b[,,"forestry_ndc"][,,"soilc"]  <- collapseNames(fm_carbon_density[,,"forestry"])[,,"soilc"]      * dimSums(p32_land[,,"ndc"],dim=3)
        b[,,"forestry_plant"][,,"soilc"]  <- collapseNames(fm_carbon_density[,,"forestry"])[,,"soilc"]      * dimSums(p32_land[,,"plant"],dim=3)
        b[,,"other"][,,"soilc"]         <- fm_carbon_density[,,"other"][,,"soilc"]         * ov_land[,,"other"]
      }
      
    } else {
      
      #cropland, pasture, urban land and primforest is simple
      b[,,"crop"]                  <- fm_carbon_density[,,"crop"]*ov_land[,,"crop"] 
      b[,,"past"]                  <- fm_carbon_density[,,"past"]*ov_land[,,"past"]
      b[,,"urban"]                 <- fm_carbon_density[,,"urban"]*ov_land[,,"urban"]
      b[,,"primforest"]            <- fm_carbon_density[,,"primforest"]*ov_land[,,"primforest"]
    }
    
    
    #forestry land
    ####################
    #p32_land <- readGDX(gdx,"p32_land","p32_land_fore",react = "quiet")
    if(is.null(p32_land)) {
      
      b[,,"forestry_plant"] <- fm_carbon_density[,,"forestry"]*ov_land[,,"forestry"]
      
    } else { 
      
      names(dimnames(p32_land))[1] <- "j"
      p32_carbon_density_ac <- readGDX(gdx,"p32_carbon_density_ac",react = "quiet")
      
      if(!is.null(p32_carbon_density_ac)) {
        p32_carbon_density_ac[,,] <- setYears(p32_carbon_density_ac[,cc_year,],NULL)
        
        if(!regrowth) p32_land <- degrowth(p32_land)
        if(som_on){
          b[,,"forestry_aff"][,,ag_pools]  <- dimSums(collapseNames(p32_carbon_density_ac[,,"aff"]*p32_land[,,"aff"]),dim=c(3.1))
          b[,,"forestry_ndc"][,,ag_pools]  <- dimSums(collapseNames(p32_carbon_density_ac[,,"ndc"]*p32_land[,,"ndc"]),dim=c(3.1))
          b[,,"forestry_plant"][,,ag_pools]  <- dimSums(collapseNames(p32_carbon_density_ac[,,"plant"]*p32_land[,,"plant"]),dim=c(3.1))
        } else {
          b[,,"forestry_aff"] <- dimSums(collapseNames(p32_carbon_density_ac[,,"aff"]*p32_land[,,"aff"]),dim=c(3.1))
          b[,,"forestry_ndc"] <- dimSums(collapseNames(p32_carbon_density_ac[,,"ndc"]*p32_land[,,"ndc"]),dim=c(3.1))
          b[,,"forestry_plant"] <- dimSums(collapseNames(p32_carbon_density_ac[,,"plant"]*p32_land[,,"plant"]),dim=c(3.1))
        }
        
        
      } else {
        if(!regrowth) p32_land <- degrowth(p32_land)
        if(som_on){
          b[,,"forestry_aff"][,,ag_pools] <- dimSums(pm_carbon_density_ac*collapseNames(p32_land[,,"aff"]),dim=3.1)
          b[,,"forestry_ndc"][,,ag_pools] <- dimSums(pm_carbon_density_ac*collapseNames(p32_land[,,"ndc"]),dim=3.1)
          b[,,"forestry_plant"][,,ag_pools] <- dimSums(pm_carbon_density_ac*collapseNames(p32_land[,,"plant"]),dim=3.1)
        } else {
          b[,,"forestry_aff"] <- dimSums(pm_carbon_density_ac*collapseNames(p32_land[,,"aff"]),dim=3.1)
          b[,,"forestry_ndc"] <- dimSums(pm_carbon_density_ac*collapseNames(p32_land[,,"ndc"]),dim=3.1)
          b[,,"forestry_plant"] <- dimSums(pm_carbon_density_ac*collapseNames(p32_land[,,"plant"]),dim=3.1)
        }
      }
    }
    ####################
    
    #secdforest 
    ####################
    p35_secdforest <- readGDX(gdx,"p35_secdforest",react = "quiet")
    if(is.null(p35_secdforest)) {
      b[,,"secdforest"] <- fm_carbon_density[,,"secdforest"]*ov_land[,,"secdforest"]
    } else {
      names(dimnames(p35_secdforest))[1] <- "j"
      if(dim(p35_secdforest)[3] == 122) p35_secdforest <- collapseNames(p35_secdforest[,,"after"])
      if(!regrowth) p35_secdforest <- degrowth(p35_secdforest)
      if(som_on){
        b[,,"secdforest"][,,ag_pools] <- dimSums(pm_carbon_density_ac*p35_secdforest,dim=3.1)
      } else {
        b[,,"secdforest"]             <- dimSums(pm_carbon_density_ac*p35_secdforest,dim=3.1)
      }
    }
    ####################
    
    #other land 
    ####################
    p35_other <- readGDX(gdx,"p35_other",react = "quiet")
    if(is.null(p35_other)) {
      b[,,"other"] <- fm_carbon_density[,,"other"]*ov_land[,,"other"]
    } else {
      names(dimnames(p35_other))[1] <- "j"
      if(dim(p35_other)[3] == 122) p35_other <- collapseNames(p35_other[,,"after"])
      if(!regrowth) p35_other <- degrowth(p35_other)
      if(som_on){
        b[,,"other"][,,ag_pools] <- dimSums(pm_carbon_density_ac*p35_other,dim=3.1)
      } else {
        b[,,"other"]                     <- dimSums(pm_carbon_density_ac*p35_other,dim=3.1)
      }
    }
    ####################
    

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
