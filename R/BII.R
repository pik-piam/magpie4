#' @title Biodiversity Intactness Index
#' @description reads biodiversity intactness index out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global).
#' @param mode "auto" (default), "MAgPIE" or "postprocessing". "MAgPIE" reports the BII based on values from the MAgPIE biodiversity module. "postprocessing" calculates the BII based on land information from MAgPIE. "auto" uses "MAgPIE" if available and falls back to "postprocessing" otherwise.
#' @param bii_coeff file containing BII coefficients. Only needed for mode="postprocessing". NULL tries to automatically detected the file.
#' @param rr_layer file containing the range-rarity layer. Only needed for mode="postprocessing". NULL tries to automatically detected the file.
#' @param side_layers file containing LUH2 side layers. Only needed for mode="postprocessing". NULL tries to automatically detected the file.
#' @details Calculates global and regional biodiversity intactness index
#' @return Biodiversity intactness index (unitless)
#' @author Patrick v. Jeetze, Florian Humpenoeder
#' @importFrom gdx readGDX out
#' @importFrom magclass dimSums 
#' @importFrom luscale superAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- BII(gdx)
#'   }

BII <- function(gdx, file=NULL, level="glo",mode="auto",bii_coeff=NULL,rr_layer=NULL,side_layers=NULL) {
  if(mode=="auto") {
    ov44_biodiv <- readGDX(gdx,"ov44_biodiv",select = list(type="level"),react = "silent")
    if(!is.null(ov44_biodiv)) {
      mode <- "MAgPIE" 
    } else if (all(is.null(bii_coeff),is.null(rr_layer),is.null(side_layers))){
      bii_coeff <- c("input/f44_bii_coeff.cs3","modules/44_biodiversity/bii_btc_apr20/input/f44_bii_coeff.cs3")
      rr_layer <- c("input/rr_layer_c200.mz","modules/44_biodiversity/bii_btc_apr20/input/rr_layer.cs2")
      side_layers <- c("input/luh2_side_layers_c200.mz","modules/44_biodiversity/bii_btc_apr20/input/luh2_side_layers.cs3")
      bii_coeff <- suppressWarnings(bii_coeff[min(which(file.exists(bii_coeff)))])
      rr_layer <- suppressWarnings(rr_layer[min(which(file.exists(rr_layer)))])
      side_layers <- suppressWarnings(side_layers[min(which(file.exists(side_layers)))])
      ov32_land <- readGDX(gdx,"ov32_land","ov_land_fore",select=list(type="level"),react = "silent")
      if(!is.null(ov32_land)) {
        if (names(dimnames(ov32_land))[3] == "type32.ac") ac <- TRUE else ac <- FALSE
      } else ac <- FALSE
      if (all(!is.na(bii_coeff),!is.na(rr_layer),!is.na(side_layers),ac)) {
        mode <- "postprocessing"
      } else mode <- "off"
    } else stop("In automatic mode, input files must be NULL to allow for automatic detection")
  }
  
  if(mode=="MAgPIE") {
    ov44_biodiv <- readGDX(gdx,"ov44_biodiv",select = list(type="level"),react = "silent")
    if (is.null(ov44_biodiv)) stop("No Biodiversity Module in MAgPIE")
    rr_layer <- readGDX(gdx,"f44_rr_layer",react = "silent") #includes range rarity layer
    #spatial aggregation
    ov44_biodiv <- dimSums(ov44_biodiv,dim=3)
    cell_area <- land(gdx,level="cell",sum = TRUE)
    reg <- superAggregate(ov44_biodiv,level="reg",aggr_type = "sum")/superAggregate(cell_area*rr_layer,level="reg",aggr_type = "sum")
    glo <- superAggregate(ov44_biodiv,level="glo",aggr_type = "sum")/superAggregate(cell_area*rr_layer,level="glo",aggr_type = "sum")
    if(level == "reg") {x <- reg}
    else if (level == "glo") {x <- glo}
    else if (level == "regglo") {x <- mbind(reg,glo)}
  } else if (mode=="postprocessing") {
    #check if postprocessing is possible
    if(any(is.null(bii_coeff),is.null(rr_layer),is.null(side_layers))) stop("In postprocessing mode, input files are needed!")
    ov32_land <- readGDX(gdx,"ov32_land","ov_land_fore",select=list(type="level"),react = "silent")
    if(!is.null(ov32_land)) {
      if (names(dimnames(ov32_land))[3] == "type32.ac") ac <- TRUE else ac <- FALSE
    } else ac <- FALSE
    if(!ac) stop("In postprocessing mode, age-classes are needed")

    #input files
    bii_coeff <- read.magpie(bii_coeff)
    #add timber if not included in input file; only added for intermediate compatability;
    if(!"timber" %in% getNames(bii_coeff,dim=1)) {
      timber <- bii_coeff[,,"secd_mature"]
      getNames(timber,dim=1) <- "timber"
      timber[,,"forested"] <- 0.734401034
      timber[,,"nonforested"] <- 0.539010935
      bii_coeff <- mbind(bii_coeff,timber)
    }
    rr_layer <- read.magpie(rr_layer)
    side_layers <- read.magpie(side_layers)

    #magpie outputs
    ov_land <- land(gdx,level="cell")
    ov_area <- croparea(gdx,level="cell",product_aggr = FALSE)
    ov32_land <- readGDX(gdx,"ov32_land","ov_land_fore",select=list(type="level"))
    ov35_secdforest <- readGDX(gdx,"ov35_secdforest","ov_natveg_secdforest",select=list(type="level"))
    ov35_other <- readGDX(gdx,"ov35_other","ov_natveg_other",select=list(type="level"))
    
    #sets
    crop_ann44 <- c("tece", "maiz", "trce", "rice_pro", "rapeseed", "sunflower", "potato", "cassav_sp", "sugr_beet", "others", "cottn_pro", "foddr", "soybean", "groundnut", "puls_pro")
    crop_per44 <- c("oilpalm", "begr", "sugr_cane", "betr")
    ac <- readGDX(gdx,"ac")
    ac_young <- paste0("ac",seq(0,30,by=5))
    ac_mature <- setdiff(ac,ac_young)
    
    #calc ov44_bii
    ov44_bii <- new.magpie(getCells(ov_land),getYears(ov_land),getNames(bii_coeff),fill = 0)
    ov44_bii[,,"crop_ann"] <- dimSums(ov_area[,,crop_ann44],dim=3) * bii_coeff[,,"crop_ann"] * side_layers[,,c("forested","nonforested")]
    ov44_bii[,,"crop_per"] <- dimSums(ov_area[,,crop_per44],dim=3) * bii_coeff[,,"crop_per"] * side_layers[,,c("forested","nonforested")]
    ov44_bii[,,"manpast"] <- collapseNames(ov_land[,,"past"]) * side_layers[,,"manpast"] * bii_coeff[,,"manpast"] * side_layers[,,c("forested","nonforested")]
    ov44_bii[,,"rangeland"] <- collapseNames(ov_land[,,"past"]) * side_layers[,,"rangeland"] * bii_coeff[,,"rangeland"] * side_layers[,,c("forested","nonforested")]
    ov44_bii[,,"urban"] <- collapseNames(ov_land[,,"urban"]) * bii_coeff[,,"urban"] * side_layers[,,c("forested","nonforested")]
    ov44_bii[,,"primary"] <- collapseNames(ov_land[,,"primforest"]) * bii_coeff[,,"primary"] * side_layers[,,c("forested","nonforested")]
    ov44_bii[,,"secd_young"] <- dimSums(collapseNames(ov32_land[,,"ndc"])[,,ac_young] + ov35_secdforest[,,ac_young] + ov35_other[,,ac_young],dim=3) * bii_coeff[,,"secd_young"] * side_layers[,,c("forested","nonforested")]
    ov44_bii[,,"secd_mature"] <- dimSums(collapseNames(ov32_land[,,"ndc"])[,,ac_mature] + ov35_secdforest[,,ac_mature] + ov35_other[,,ac_mature],dim=3) * bii_coeff[,,"secd_mature"] * side_layers[,,c("forested","nonforested")]
    
    #Afforestation can be based on natveg or plantation growth curves
    s32_aff_plantation <- readGDX(gdx,"s32_aff_plantation",react = "silent")
    if(is.null(s32_aff_plantation)) s32_aff_plantation <- 0 #default
    if (s32_aff_plantation == 0) {
      ov44_bii[,,"secd_young"] <- ov44_bii[,,"secd_young"] + dimSums(collapseNames(ov32_land[,,"aff"])[,,ac_young],dim=3) * bii_coeff[,,"secd_young"] * side_layers[,,c("forested","nonforested")]
      ov44_bii[,,"secd_mature"] <- ov44_bii[,,"secd_mature"] + dimSums(collapseNames(ov32_land[,,"aff"])[,,ac_mature],dim=3) * bii_coeff[,,"secd_mature"] * side_layers[,,c("forested","nonforested")]
    } else if (s32_aff_plantation == 1) {
      ov44_bii[,,"timber"] <- ov44_bii[,,"timber"] + dimSums(collapseNames(ov32_land[,,"aff"]),dim=3) * bii_coeff[,,"timber"] * side_layers[,,c("forested","nonforested")]
    }
    
    #Timber plantations can be based on natveg or plantation growth curves
    s32_timber_plantation <- readGDX(gdx,"s32_timber_plantation",react = "silent")
    if(is.null(s32_timber_plantation)) s32_timber_plantation <- 1 #default
    if (s32_timber_plantation == 0) {
      ov44_bii[,,"secd_young"] <- ov44_bii[,,"secd_young"] + dimSums(collapseNames(ov32_land[,,"plant"])[,,ac_young],dim=3) * bii_coeff[,,"secd_young"] * side_layers[,,c("forested","nonforested")]
      ov44_bii[,,"secd_mature"] <- ov44_bii[,,"secd_mature"] + dimSums(collapseNames(ov32_land[,,"plant"])[,,ac_mature],dim=3) * bii_coeff[,,"secd_mature"] * side_layers[,,c("forested","nonforested")]
    } else if (s32_timber_plantation == 1) {
      ov44_bii[,,"timber"] <- ov44_bii[,,"timber"] + dimSums(collapseNames(ov32_land[,,"plant"]),dim=3) * bii_coeff[,,"timber"] * side_layers[,,c("forested","nonforested")]
    }
    
    ov44_biodiv <- rr_layer * dimSums(ov44_bii,dim=3.2)
    
    #spatial aggregation
    ov44_biodiv <- dimSums(ov44_biodiv,dim=3)
    cell_area <- land(gdx,level="cell",sum = TRUE)
    reg <- superAggregate(ov44_biodiv,level="reg",aggr_type = "sum")/superAggregate(cell_area*rr_layer,level="reg",aggr_type = "sum")
    glo <- superAggregate(ov44_biodiv,level="glo",aggr_type = "sum")/superAggregate(cell_area*rr_layer,level="glo",aggr_type = "sum")
    if(level == "reg") {x <- reg}
    else if (level == "glo") {x <- glo}
    else if (level == "regglo") {x <- mbind(reg,glo)}
  } else if (mode=="off") {
    x <- NULL
  }
  
  
  out(x,file)
}


