#' @title reportGridManureExcretion
#' @description reports Manure with reortingnames on grid level.
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' 
#' @return MAgPIE object
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- reportGridManureExcretion(gdx)
#'   }
#' 

reportGridManureExcretion <- function(gdx,dir=".",spamfiledirectory="") {
  
  dir <- getDirectory(dir,spamfiledirectory)
  
  manure <- ManureExcretion(gdx, level="grid")
  awms <- dimSums(manure, dim="kli")
  kli <- dimSums(manure,dim="awms")
  confinement=collapseNames(manure[,,"confinement"])
  
  vm_manure_confinement <- collapseNames(readGDX(gdx,"ov_manure_confinement")[,,"level"][,,"nr"])
  vm_manure_confinement <- gdxAggregate(gdx=gdx, x = vm_manure_confinement, weight = manure[,,"confinement"], to = "grid",dir = ".",absolute = TRUE)
  
  pollutants=c("n2o_n_direct","nh3_n","no2_n", "no3_n")
  f55_awms_recycling_share <- readGDX(gdx,"f55_awms_recycling_share")
  f51_ef3_confinement <- readGDX(gdx,"f51_ef3_confinement")
  im_maccs_mitigation=readGDX(gdx,"im_maccs_mitigation")[,,"awms"][,,pollutants]
  f51_ef3_confinement = f51_ef3_confinement * collapseNames(1-im_maccs_mitigation)
  destiny <- add_columns(f51_ef3_confinement,addnm = c("recycling"),dim = 3.3)
  destiny[,,"recycling"]<-f55_awms_recycling_share
  #destiny[,,"n2_n"]<- (1-dimSums(destiny,dim=3.3,na.rm=T))
  if(any(dimSums(destiny,dim=3.3,na.rm=T)>1)){stop("error in emission factors")}
  destiny <- gdxAggregate(gdx=gdx, x = destiny, to = "grid",dir = ".",absolute = FALSE)
  
  # memory problems
  emis1 = vm_manure_confinement[,,c("livst_rum","livst_milk")] * destiny[,,c("livst_rum","livst_milk")]
  emis2 = vm_manure_confinement[,,c("livst_pig","livst_chick","livst_egg")] * destiny[,,c("livst_pig","livst_chick","livst_egg")]
  emis1 = dimSums(emis1,dim="awms_conf")
  emis2 = dimSums(emis2,dim="awms_conf")
  destiny = mbind(emis1,emis2)
  
  total <- setNames(dimSums(manure),"Manure")
  getNames(awms) <- paste0("Manure|+|",reportingnames(getNames(awms)))
  getNames(kli) <- paste0("Manure|++|",reportingnames(getNames(kli)))
  
  losses = dimSums(destiny[,,pollutants],dim="kli")
  getNames(losses) <-paste0("Manure|Manure In Confinements|Losses|",reportingnames(getNames(losses)))
  recycling <- dimSums(destiny[,,"recycling"],dim="kli")
  getNames(recycling) <-paste0("Manure|Manure In Confinements|+|Recycled")
  confinement_loss = dimSums(confinement,dim=3)-recycling
  getNames(confinement_loss) <-paste0("Manure|Manure In Confinements|+|Losses")
  
  out <- mbind(total,awms,kli,recycling,confinement_loss,losses)
  
  return(out)
}

