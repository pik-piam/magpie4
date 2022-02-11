#' @title water_usage
#' @description reads area usage from a MAgPIE gdx file
#' 
#' @importFrom gdx expand.set
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level spatial level of aggregation: "cell" (cellular), "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param users NULL or "sectors". If NULL, all sectors including crop-wise water use and livestock will be obtained. 
#' @param users If sectors, will only report for high-level sectors - agriculture, industry, electricity, domestic, ecosystem. Sum not applicable in this case
#' @param sum determines whether output should be sector specific (FALSE) or aggregated over all sectors (TRUE)
#' @param digits integer. For rounding of the return values
#' @return A MAgPIE object containing the water usage (km^3/yr)
#' @author Markus Bonsch, Vartika Singh 
#' @examples
#' 
#'   \dontrun{
#'     x <- water_usage(gdx)
#'   }
#' 

water_usage <- function(gdx, file=NULL, level="reg", users=NULL, sum=FALSE, digits=0) {
  sectors<-readGDX(gdx,"wat_dem")
  kcr<-readGDX(gdx,"kcr")
  kli<-readGDX(gdx,"kli")
  if(is.null(users)){
    users<-expand.set(gdx,c(sectors,kcr,kli),c(sectors,kcr,kli))
  } else{
    users<-sectors
  }
  user<-list()
  user$crops<-match(users,kcr)
  user$crops<-user$crops[!is.na(user$crops)]
  if(length(user$crops)>0) user$crops<-kcr[user$crops]

  user$kli<-match(users,kli)
  user$kli<-user$kli[!is.na(user$kli)]
  if(length(user$kli)>0) user$kli<-kli[user$kli]
  
  user$sectors<-match(users,sectors)
  user$sectors<-user$sectors[!is.na(user$sectors)]
  if(length(user$sectors)>0) user$sectors<-sectors[user$sectors]

  # Test if all users belong either to kcr or sectors
  if(length(unlist(user))!=length(users)){
    bad<-users[which(is.na(match(users,unlist(user))))]
    stop(paste("ERROR: The following elements of users are not water users: ",bad))
  }
  if((length(user$crops)>0 | length(user$kli)>0) && length(user$sectors)>0 && sum==TRUE) warning("Summing over specific crops and other sectors in water_usage")

  out<-list()
  if(length(user$sectors)>0){
    out$sectors<-readGDX(gdx,"ov_watdem","ovm_watdem", format="first_found")[,,"level"][,,user$sectors]
    out$sectors<-setNames(out$sectors,gsub(".level","",getNames(out$sectors),fixed=TRUE))
    if(level!="cell") out$sectors <- superAggregate(as.magpie(out$sectors),aggr_type="sum",level=level)
  }

  if(length(user$crops)>0){  
    i_wat_req_k_cell <- readGDX(gdx,"i42_wat_req_k","i43_wat_req_k","i17_wat_req_k", format="first_found")[,,user$crops]
    if(is.null(i_wat_req_k_cell)) {
      warning("Water usage cannot be calculated as needed data could not be found in GDX file! NULL is returned!")
      return(NULL)
    }
    ovm_area_cell<-croparea(gdx,level="cell",products=user$crops,product_aggr=FALSE,water_aggr=FALSE)[,,"irrigated"]
    ovm_area_cell<-setNames(ovm_area_cell,gsub(".irrigated","",getNames(ovm_area_cell),fixed=TRUE))
    #For back compatibility only
    tmp <- readGDX(gdx,"ov42_irrig_eff","ov43_irrig_eff","ov17_irrig_eff", "i42_irrig_eff", format="first_found")
    if (length(getNames(tmp)) > length(as.matrix(readGDX(gdx,"type", format="first_found")))) {
      ov_irrig_eff_cell <- tmp[,,"level"][,,user$crops]
    } else {
      ov_irrig_eff_cell_tmp <- tmp[,,"level"]
      ov_irrig_eff_cell <- ovm_area_cell
      for (crop in user$crops) {
        ov_irrig_eff_cell[,,crop] <- setNames(ov_irrig_eff_cell_tmp,NULL)
      }
    }
    out$kcr<-ovm_area_cell*i_wat_req_k_cell/ov_irrig_eff_cell
    if(level!="cell") out$kcr <- superAggregate(out$kcr,aggr_type="sum",level=level)
  }
 
  if (length(user$kli)>0) {
    i_wat_req_k_cell <- readGDX(gdx,"i42_wat_req_k","i43_wat_req_k","i17_wat_req_k", format="first_found")[,,user$kli]
    ovm_prod_cell <- readGDX(gdx,"ov_prod","ovm_prod", format="first_found")[,,"level"][,,user$kli]
    ovm_prod_cell<-setNames(ovm_prod_cell,gsub(".level","",getNames(ovm_prod_cell),fixed=TRUE))
    out$kli<-as.magpie(ovm_prod_cell*i_wat_req_k_cell)
    if(level!="cell") out$kli <- superAggregate(out$kli,aggr_type="sum",level=level)
  }


  outout<-NULL
  for(i in 1:length(out)){
    if(is.null(outout)) {
      outout<-out[[i]]
    } else{
      outout<-mbind(outout,out[[i]])
    }
  }
  
  
  if(sum==TRUE){ 
    #Summing over high level sectors for water use i.e., agriculture, industry, manufacturing, livestock and ecosystems
    sectors <- out$sectors
    sectors<-rowSums(sectors,dims=2)  
    outout <- sectors
  }
    
  #from mio m^3 to km^3
  outout<-outout/1000
  outout <- round(outout,digits)
  out(outout,file)
  
}

