#' @title SOM
#' @description Calculates soil organic carbon stock size based on a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param type "stock" (default) for absoulte values, "density" for per hectar values
#' @param reference default is "actual" (cshare in actual carbon stocks). Other option is "target" (cshare in target carbon stocks).
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' @return A MAgPIE object containing som values
#' @author Kristine Karstens
#' @examples
#' 
#'   \dontrun{
#'     x <- SOM(gdx)
#'   }
#' 

SOM <- function(gdx, file=NULL, type="stock", reference="actual", level="reg", spamfiledirectory=""){
  
  if(level%in%c("cell","reg","glo","regglo")){
    
    #################################################################
    ### read different input regarding different som realizations ###
    #################################################################
    
    if(!is.null(readGDX(gdx, "ov59_som_pool", react="silent"))){
      
      if(!is.null(nc59 <- readGDX(gdx, "noncropland59", types="sets", react="silent"))){
        
        # Dynamic SOM-module reports som stocks with all pool representation of stocks
        if(reference=="actual")       som_stock <- readGDX(gdx, "ov_som_pool","ovm_som_pool","ov59_som_pool", select=list(type="level"))
        else if(reference=="target")  som_stock <- readGDX(gdx, "ov59_som_target", select=list(type="level"))
        else stop("Unknown 'reference' input parameter.")
        
        som_stock <- mbind(setNames(som_stock[,,"crop"],"cropland"),
                           setNames(dimSums(som_stock[,,nc59], dim=3),"noncropland"))
      } else {
      
        # Dynamic SOM-module reports som stocks
        if(reference=="actual")       som_stock <- readGDX(gdx, "ov_som_pool","ovm_som_pool","ov59_som_pool", select=list(type="level"))
        else if(reference=="target")  som_stock <- readGDX(gdx, "ov59_som_target", select=list(type="level"))
        else stop("Unknown 'reference' input parameter.")
      }
     
      dynamic <- TRUE
      
    } else {
      
      if(reference=="target") cat("Note that foir static SOM implementation 'actual' and 'target' carbon stock values are the same.")
      
      # Static SOM-module has som denisties as input
      som_topsoil_crop_static  <- readGDX(gdx,"i59_topsoilc_density")
      som_topsoil_static       <- readGDX(gdx,"f59_topsoilc_density")
      
      som_dens <- mbind(setNames(som_topsoil_crop_static,                  "cropland"),
                        setNames(som_topsoil_static,                       "noncropland"))
      
      # Reconstruct carbon stocks with carbon density and land information 
      # (note: all noncropland land types have the same soil carbon density even all age-classes of nat-veg)
      land <- land(gdx, level="cell")
      land <- mbind(setNames(land[,,"crop"],"cropland"), 
                    setNames(dimSums(land[,,"crop",invert=TRUE], dim=3),"noncropland"))
      
      som_stock <- land * som_dens[,getYears(land),]
      
      dynamic <- FALSE
    }
    
    #################################################################
    ###  disaggregation to various levels                         ###
    #################################################################
    
    if(level%in%c("reg","glo","regglo")){
      
      som_stock <- gdxAggregate(gdx, som_stock, to=level, absolute=TRUE)
      
    }
    
    #################################################################
    ###  stock vs density output format calculations              ###
    #################################################################
    
    if(type=="stock"){
      
      out <- mbind(som_stock, setNames(dimSums(som_stock, dim=3), "total"))
      
    } else if(type=="density"){
      
      som_stock <- mbind(som_stock, setNames(dimSums(som_stock, dim=3), "total"))
      
      land <- land(gdx, level=level, spamfiledirectory=spamfiledirectory)
      land <- mbind(land[,,"crop", pmatch=TRUE, invert=TRUE], setNames(dimSums(land[,,"crop", pmatch=TRUE],dim=3),"crop")) #can be removed, if interpolation is delivering pure crop again
      land <- mbind(setNames(land[,,"crop"],                             "cropland"),
                    setNames(dimSums(land[,,"crop",invert=TRUE], dim=3), "noncropland"), 
                    setNames(dimSums(land                      , dim=3), "total") ) 
      
      out <- som_stock/land
      out[is.infinite(out)] <- NA
      
    } else {stop(paste("Type", type, "does not exist yet."))}
    
  } else if(level=="grid"){

    #################################################################
    ###  disaggregation to grid level using supporting data       ###
    #################################################################
    
    
    # disaggregation routine takes care of spatial variablity of soil carbon stocks within clusters-
    # it woks as follows:
    # 1. using 1995 half-degree potential natural vegetation soil density pattern
    # 2. scaling them to time dependend potential natural vegetation cluster values
    # 3. using cshare cluster values to scale the different types (cropland, noncropland) for grid values
    # 4. (calculating carbon stocks on grid level)
    
    # 1. load topsoil potential natural vegetation denisties on half-degree level (grid level)
    lpj_soc_dens  <- collapseNames(read.magpie(paste0(spamfiledirectory,"../../modules/59_som/input/lpj_carbon_topsoil_0.5.mz")), collapsedim = 1)
    
    # defining if nocc or cc option was switched on
    carbon_test <- readGDX(gdx, "fm_carbon_density")
    if(all(setYears(carbon_test[,"y1995",], NULL) == carbon_test[,"y1995",, invert=TRUE])){
      nocc <- TRUE
    } else {
      nocc <- FALSE
      cat(paste("Although this is a run with cc: half-degree soil carbon pattern of 1995 is used for disaggregation of carbon stocks from cluster to cell level.", 
                "Note that for that reason no changes in soil carbon stock patterns due to cc below cluster level are present."))
    }
    
    # 2. scaling them to time dependend potential natural vegetation cluster values
    
    # load land information on grid level
    land <- land(gdx, level="grid", sum=TRUE, spamfiledirectory=spamfiledirectory)
    
    # calculate reference stock with original half-degree lpj data and aggregate to cluster level
    som_ref     <- gdxAggregate(gdx, setCells(lpj_soc_dens, getCells(land)) * land, to="cell", spamfiledirectory = spamfiledirectory)
    
    # calculate time dependend potential natural vegetation cluster values
    land_cell   <- land(gdx, level="cell", sum=TRUE, spamfiledirectory=spamfiledirectory)
    soilc_cell  <- readGDX(gdx,"f59_topsoilc_density")[,getYears(land),]
    som_pot     <- land_cell * soilc_cell
    
    # calculate scaling ratio and aggregate back to grid level ( + remove NAs)
    som_scaling <- som_pot/som_ref
    som_scaling[is.na(som_scaling)]          <- 0
    som_scaling[is.infinite(som_scaling)]    <- 0
    som_scaling <- gdxAggregate(gdx, som_scaling, to="grid", spamfiledirectory = spamfiledirectory, absolute=FALSE)
    
    # scale grid level carbon densities to meet model output 
    som_pot_grid  <- setCells(lpj_soc_dens, getCells(som_scaling)) * som_scaling

    # 3. using cshare cluster values to scale the different types (cropland, noncropland) for grid values
    
    # load cshares to account for management within changing climate
    cshare <- gdxAggregate(gdx, cshare(gdx, level="cell", reference=reference), to="grid", spamfiledirectory = spamfiledirectory, absolute=FALSE)
    
    # scale potential natural vegetation carbon densities with cshare 
    som_dens <- cshare * som_pot_grid
    
    
    land <- land(gdx, level="grid", spamfiledirectory=spamfiledirectory)
    land <- mbind(land[,,"crop", pmatch=TRUE, invert=TRUE], setNames(dimSums(land[,,"crop", pmatch=TRUE],dim=3),"crop")) #can be removed, if interpolation is delivering pure crop again
    land <- mbind(setNames(land[,,"crop"],                             "cropland"),
                  setNames(dimSums(land[,,"crop",invert=TRUE], dim=3), "noncropland"), 
                  setNames(dimSums(land                      , dim=3), "total") ) 
    
    som_stock  <- som_dens * land
    som_stock[is.infinite(som_stock)] <- NA
    som_stock[is.na(som_stock)]       <- 0
    
    grid_stock    <- gdxAggregate(gdx, som_stock, to="reg", spamfiledirectory = spamfiledirectory, na.rm=TRUE)
    cluster_stock <- SOM(gdx, type="stock", reference=reference, level="reg")
    
    if(any(abs((cluster_stock-grid_stock)/cluster_stock)>0.1)){
      warning(paste0("Disaggregation on grid level will not conserve total carbon stocks, but cshares.\n", 
                    "This leeds to a mismatch in cropland carbon stock of over 10% in: ", 
                     paste(where(abs((cluster_stock-grid_stock)/cluster_stock)>0.1)$true$reg, collapse = ", "),
                    ". On cluster level this is even worse."))
    }
    
    if(type=="density")     out  <- som_dens
    else if(type=="stock")  out  <- som_stock
    else {stop(paste("Type", type, "does not exist yet."))}
    
    #################################################################
    ### THIS DISAGGEGRATION DO NOT CONSERVE CSHARES FROM CLUSTERS ###
    #################################################################
    # # Load land information on grid level
    # land <- land(gdx, level="grid", spamfiledirectory=spamfiledirectory)
    # land <- mbind(land[,,"crop", pmatch=TRUE, invert=TRUE], setNames(dimSums(land[,,"crop", pmatch=TRUE],dim=3),"crop")) #can be removed, if interpolation is delivering pure crop again
    # land <- mbind(setNames(land[,,"crop"],"cropland"), setNames(dimSums(land[,,"crop",invert=TRUE], dim=3),"noncropland") ) 
    # 
    # # disaggregation routine takes care of spatial variablity of soil carbon stocks within clusters 
    # # by using 1995 half-degree potential natural vegetation soil stock pattern, but scaling them to simulated cluster values
    # 
    # # calculate reference stock with original half-degree lpj data and aggregate to cluster level
    # som_ref     <- gdxAggregate(gdx, setCells(lpj_soc_dens, getCells(land)) * land, to="cell", spamfiledirectory = spamfiledirectory)
    # 
    # # calculate sclaing ratio for cropland/noncropland and aggregate back to grid level ( + remove NAs)
    # som_scaling <- som_stock/som_ref
    # som_scaling[is.na(som_scaling)]          <- 0
    # som_scaling[is.infinite(som_scaling)]    <- 0
    # som_scaling <- gdxAggregate(gdx, som_scaling, to="grid", spamfiledirectory = spamfiledirectory, absolute=FALSE)
    # 
    # # scale grid level carbon stocks to meet model output 
    # som_stock_grid  <- setCells(lpj_soc_dens, getCells(land)) * land * som_scaling
    # 
    # # test if scaling process succeeded (aggregate grid level data back to cluster level and compare)
    # test            <- gdxAggregate(gdx, som_stock_grid , to="cell", spamfiledirectory = spamfiledirectory)
    # if(any(round(test,4)!=round(som_stock,4))){
    #   warning(paste0("Disaggregated soil carbon stock on grid level are not matching cluster values in: ", paste(where(round(test,4)!=round(som_stock,4))$true$reg, collapse = ",")))
    # }
    # 
    # som_stock <- som_stock_grid
  }
  
  return(out)
}
