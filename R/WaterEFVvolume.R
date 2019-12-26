#' @title WaterEFVvolume
#' @description calculation of water environmental flow violation volume
#' 
#' @importFrom magclass time_interpolate
#' @export
#' 
#' @param gdx GDX file
#' @param cfg config file
#' @param spam spam matrix
#' @param input_folder input folder
#' @param years years
#' @return A MAgPIE object containing the volume of environmental flow violations
#' @author Markus Bonsch, Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- WaterEFVvolume(gdx)
#'   }
#' 


WaterEFVvolume<-function(gdx,cfg,spam,input_folder,years){
  #human water withdrawals
  WW_grper<- water_usage(gdx,level = "cell",digits=15,users=c("agriculture","industry","electricity","domestic"),sum=FALSE)
  #environemntal flow requirementsgit s
  EFR <-read.magpie(path(input_folder,paste0("lpj_envflow_grper_",cfg$high_res,".mz")))/1000
  EFR <- speed_aggregate(EFR,rel = spam)
  if(cfg$static_inputs) EFR<-EFR[,"y1995",]
  EFR<-time_interpolate(EFR,interpolated_year=years,extrapolation_type="constant")
  EFR<-round(EFR,15)              
  #available water
  AVL_grper<-water_avail(gdx,level = "cell",sum = TRUE,digits=15)
  #Determine, by how much environmental flows are violated in each cluster
  tmp<-dimSums(WW_grper,dim=3.1)+setNames(EFR,NULL)-AVL_grper
  tmp[tmp<=0]<-0
  #Calculate, how much of the violations can be attributed to agriculture
  violation<-tmp
  violation[,,]<-NA
  violation[which(tmp<=WW_grper[,,"agriculture"])]<-tmp[which(tmp<=WW_grper[,,"agriculture"])]
  violation[which(tmp>WW_grper[,,"agriculture"])]<-WW_grper[,,"agriculture"][which(tmp>WW_grper[,,"agriculture"])]
  reg <- superAggregate(violation,level="reg",aggr_type="sum")
  return(reg)
}