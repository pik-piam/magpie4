#' @title gdxAggregate
#' @description aggregates and disaggregates on spatial scales using mappings from the gdx files. Very specific to MAgPIE.
#'
#' @param gdx gdx file
#' @param x object to be aggrgeagted or disaggregated
#' @param weight weight can be either an object or a functionname in "", where the function provides the weight
#' @param to options: cell, iso, reg, glo, regglo
#' @param absolute is it a absolute or a relative value (absolute: tons, relative: tons per hectare)
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' @param ... further parameters handed on to weight function.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' gdp_pc <- income(gdx,level="reg")
#' is.function(population)
#' gdp_pc_iso <- gdxAggregate(gdx=gdx,x=gdp_pc,weight="population", to="iso", absolute=FALSE)
#' gdp_pc_glo <- gdxAggregate(gdx=gdx,x=gdp_pc,weight="population", to="glo", absolute=FALSE)
#' gdp <- income(gdx,level="reg",per_capita=FALSE)
#' gdp_iso <- gdxAggregate(gdx=gdx,x=gdp,weight="population", to="iso", absolute=TRUE)
#' gdp_glo <- gdxAggregate(gdx=gdx,x=gdp,weight="population", to="glo", absolute=TRUE)
#' 
#' }
#' @export
#' @importFrom magclass getSets
#' @importFrom luscale speed_aggregate
#' @importFrom lucode path
#' @importFrom spam triplet
#' @importFrom luscale read.spam

gdxAggregate<-function(gdx, x, weight=NULL, to, absolute=TRUE, spamfiledirectory="", ...){
  
  if(is.function(weight)){warning("You provide a function as weihgt. Better but the functionname in '' to avoid overlapping naming in the R environment")}
  if(length(weight==1)){
    if (is.character(weight)){
      weight<-get(weight,mode = "function")
    }
  }
  if(to=="GLO"){to<-"glo"}
  if(to=="REGGLO"){to<-"regglo"}

  iso_to_cell<-readGDX(gdx=gdx,"iso_to_j",react = "silent")
  reg_to_iso<-readGDX(gdx=gdx,"i_to_iso")
  names(reg_to_iso)<-c("reg","iso")
  reg_to_cell<-readGDX(gdx=gdx,"cell")
  names(reg_to_cell)<-c("reg","cell")
  reg_to_cell$cell<-gsub(reg_to_cell$cell,pattern = "_",replacement = ".")
  
  #0.5 grid mapping
  spamfile <- Sys.glob(path(spamfiledirectory,"*_sum.spam"))
  if(length(spamfile==1)){
    grid_to_cell=triplet(read.spam(spamfile))$indices
    grid_to_cell=grid_to_cell[order(grid_to_cell[,2]),1]
    grid_to_cell<-reg_to_cell[match(x = grid_to_cell, table = as.integer(substring(reg_to_cell[,2],5,7))),]
    grid_to_cell$grid<-paste0(grid_to_cell[,1],".",1:dim(grid_to_cell)[1])
  } else {
    grid_to_cell=NULL
  }
  
  
  if(all(dimnames(x)[[1]] %in% reg_to_cell$cell)){
    from="cell"
  } else {
    if(all(dimnames(x)[[1]]%in%reg_to_iso$iso)){
      from = "iso"
    } else if (all(dimnames(x)[[1]]%in%reg_to_iso$reg)){
      from = "reg"
    } else if (all(dimnames(x)[[1]]%in%c("GLO","GLO.1"))){
      from="glo"
    } else if (all(dimnames(x)[[1]]%in%c("GLO","glo",reg_to_iso$reg))){
      from="regglo"
    } else if (all(dimnames(x)[[1]]%in%c(grid_to_cell$grid))){
      from="grid"
    } else {stop("unknown regions, wrong or missing spamfiledirectory")}
  }
  

  # get rid of unnecessary data
  if(from%in%c("REGGLO","regglo")){
    x<-x["GLO",,,invert=TRUE]
    if(!is.function(weight)){
      if("GLO"%in%getRegions(weight)){
        if (length(getRegions(weight))>1){
          weight<-weight["GLO",,,invert=TRUE]
        }
      }
    }
    from="reg"
  }
  
  
  if(to%in%c("regglo")){
    to2="regglo"
    to="reg"
  } else {to2=FALSE}

  # no aggregation needed?
  if(from==to){
    out<-x
    cat(" no aggregation needed")
  } else {
    cat(paste0("mapping: ",from,"_",to))
    # select mapping
    if(((from=="cell")&(to=="iso"))|(((from=="iso")&(to=="cell")))){
      if(is.null(iso_to_cell)){
        stop("No iso_to_j mapping provided by gdx")
      }
      mapping<-iso_to_cell
    } else if(((from=="reg")&(to=="iso"))|(((from=="iso")&(to=="reg")))){
      mapping<-reg_to_iso
    } else if(((from=="cell")&(to=="reg"))|(((from=="reg")&(to=="cell")))){
      mapping<-reg_to_cell
    } else if(to%in%c("glo")) {
      mapping<-data.frame(
        from=dimnames(x)[[1]],
        glo="GLO"
      )
      names(mapping)[1]<-from
    } else if(((from=="grid")&(to=="cell"))|(((from=="cell")&(to=="grid")))|(((from=="reg")&(to=="grid")))|(((from=="grid")&(to=="reg")))){
      mapping<-grid_to_cell
    } else {stop("unknown mapping")}
  
    if(absolute==TRUE){
      # gewicht nur notwenig bei aggregation
      if(!is.function(weight)){
        if(paste0(from,to)%in%c("gridcell","gridiso","gridreg","gridglo","celliso","cellreg","cellglo","isoreg","isoglo","regglo")) {
          # aggregation of absolute values needs no weight
          if(!is.null(weight)){stop("weight provided, but aggregation of absolute values needs no weight")}
        } else {
          # disaggregation of absolute values needs weight
          if(is.null(weight)){stop("no weight provided, but disaggregation of absolute values needs weight")}
        }
      }else{
        if(paste0(from,to)%in%c("gridcell","gridiso","gridreg","gridglo","celliso","cellreg","cellglo","isoreg","isoglo","regglo")) {
          # aggregation of absolute values needs no weight
          weight=NULL
        } else {
          # disaggregation of absolute values needs weight
          weight<-weight(gdx=gdx, level=to, ...)
        }
      }
    } else if (absolute==FALSE){
      if(!is.function(weight)){
        if(paste0(from,to)%in%c("gridcell","gridiso","gridreg","gridglo","celliso","cellreg","cellglo","isoreg","isoglo","regglo")) {
          # aggregation of relative values needs weight
          if(is.null(weight)){stop("weight not provided, but aggregation of relative values needs weight")}
        } else {
          # disaggregation of relative values needs no weight
          if(!is.null(weight)){stop("weight provided, but aggregation needs no weight")}
        }
      }else{
        if(paste0(from,to)%in%c("gridcell","gridiso","gridreg","gridglo","celliso","cellreg","cellglo","isoreg","isoglo","regglo")) {
          # aggregation of relative values needs weight
          weight<-weight(gdx=gdx, level=from, ...)
        } else {
          # disaggregation of relative values needs no weight
          weight=NULL
        }
      }
    } else {stop("absolute has to be binary")}
      
    if(!is.null(weight)|!is.null(getYears(weight))){weight<-weight[,getYears(x),]}  # problems can occur if function provides different years than object has
    out <- speed_aggregate(x = x,rel=mapping,weight = weight,from = from,to = to,dim = 1)
    if(!is.null(weight)){weight <- speed_aggregate(x = weight,rel=mapping,from = from,to = to,dim = 1)} # aggregate weight too for the case its needed again in regglo
  }
  
  
  if (to2=="regglo"){
    if(absolute==TRUE){
      out<- mbind(out,dimSums(out,dim=1))
    } else {
      out<- mbind(out,
                  dimSums(out*weight,dim=1)/dimSums(weight,dim=1)
      )
    }
  }
  return(out)
  
} 